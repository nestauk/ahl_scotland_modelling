rm(list = ls())

library(here)

source(here("pipeline", "extrapolate.R"))


###########################################################################
# this script contains alternative representations of plotting EDI breakdowns

# OPTION A: extrapolate to obtain average kcal/day per EDI group
# OPTION B: calculate the total share of kcal that should be cut for each edi group
# OPTION C: calculate reduction in obesity prevalence for each EDI group according to each scenario

# function that returns the mean intake for each EDI group and year

make_by_edi <- function(year, class, sex_value, edi){
  
  df2019 <- df2019 %>% mutate(intake = pal*rmr) %>% filter(!! ensym(edi) > 0)
  
  svy_2019 <- svydesign(ids = ~df2019$psu,
                        nest = T,
                        data = df2019,
                        strata = df2019$strata,
                        weights = df2019$int_wt)
  
  edi_values <- df2019 %>% select(!! ensym(edi)) %>% unique() 
  
  list_edi_values <- as.list(edi_values[[1]])
  
  
  by_edi <- function(edi_value){
    
    group_2019 <- subset(svy_2019, eval(expr(bmi_class_c %in% class & sex == sex_value & !! ensym(edi) == edi_value)))
    
    current_ei <- svymean(~intake, group_2019)[[1]]
    
    year_dat <- all_full[[paste0("full_", year)]]
    
    svy_full <- svydesign(ids = ~year_dat$psu,
                          nest = T,
                          data = year_dat,
                          strata = year_dat$strata,
                          weights = year_dat$int_wt)
    
    group_full <- subset(svy_full, eval(expr(bmi_class_c %in% class & sex == sex_value & !! ensym(edi) == edi_value)))
    
    out <- rbind(svyby(~intake_end, ~year_model, group_full, svymean ) %>% 
                   as.data.frame() %>% 
                   select(year_model, intake_end),
                 data.frame(year_model = 2019, intake_end = current_ei)) 
    
    out[[edi]] <-  edi_value
    
    return(out)
  }
  
  do.call("rbind", lapply(list_edi_values, by_edi))
}


# function that applies make_pred_table to produce scenarios

make_pred_table_by_edi <- function(scenario, edi, thr){
  
  df_split <- scenario %>% group_by(!! ensym(edi))
  
  df_list <- group_split(df_split) %>% set_names(unlist(group_keys(df_split))) 
  
  df_table <- lapply(df_list, make_pred_table, thresholds = thr)
  
  tst <- do.call("rbind", df_table)
  
  tst[[edi]] <- substr(rownames(tst),1,1)
  
  return(tst)
}


# prepare 2019 data

df2019 <- df2019 %>% mutate(intake = pal*rmr)

svy_2019 <- svydesign(ids = ~df2019$psu,
                      nest = T,
                      data = df2019,
                      strata = df2019$strata,
                      weights = df2019$int_wt)

female_2019 <- subset(svy_2019, sex == "female")
male_2019 <- subset(svy_2019, sex == "male")


# obese
female_ob = 705000
male_ob = 635000

# obese female

# IMD

# get proportions to use as weights for option B

imd_prop_female <- svytable(~bmi_class_c + imd, female_2019) %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese") %>% 
  mutate(sum = sum(Freq),
         share = Freq/sum)

options(scipen = 999)

scenario_obese_female <- do.call("rbind", mapply(make_by_edi, 
                                                 years, 
                                                 MoreArgs = list(class = "obese", sex_value = "female", edi = "imd"), 
                                                 SIMPLIFY = F) ) %>% distinct()
# baseline distribution  
ggplot(imd_prop_female, aes(x = imd, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = imd, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese women")

# option B
make_pred_table_by_edi(scenario_obese_female, edi = "imd", thr = c(0.5)) %>% 
  filter(rel_freq == 0.5) %>% 
  merge(., imd_prop_female, by = "imd") %>% 
  mutate(tot = female_ob,
         share_imd = tot*share,
         kcal_share = share_imd * diff) %>% 
  ggplot(., aes(x = imd, y = kcal_share)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(imd), label = paste(format(round(kcal_share / 1e6, 1), trim = TRUE), "M")), vjust= -1 , color = "white") +
  geom_text(aes(x = as.factor(imd), label = label_percent(accuracy = 0.01)(share)), vjust= 1.5 ) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Obese Female")


# option A
make_pred_table_by_edi(scenario_obese_female, edi = "imd", thr = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  facet_grid(imd ~ . ) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Obese Female")

# obese male

imd_prop_male <- svytable(~bmi_class_c + imd, male_2019) %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese") %>% 
  mutate(sum = sum(Freq),
         share = Freq/sum)

options(scipen = 999)

scenario_obese_male <- do.call("rbind", mapply(make_by_edi, 
                                               years, 
                                               MoreArgs = list(class = "obese", sex_value = "male", edi = "imd"), 
                                               SIMPLIFY = F) ) %>% distinct()

# baseline distribution
ggplot(imd_prop_male, aes(x = imd, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = imd, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Male")

# option B
make_pred_table_by_edi(scenario_obese_male, edi = "imd", thr = c(0.5)) %>% 
  filter(rel_freq == 0.5) %>% 
  merge(., imd_prop_male, by = "imd") %>% 
  mutate(tot = male_ob,
         share_imd = tot*share,
         kcal_share = share_imd * diff) %>% 
  ggplot(., aes(x = imd, y = kcal_share)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(imd), label = paste(format(round(kcal_share / 1e6, 1), trim = TRUE), "M")), vjust= -1 , color = "white") +
  geom_text(aes(x = as.factor(imd), label = label_percent(accuracy = 0.01)(share)), vjust= 1.5 ) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Obese Male")


# option A
make_pred_table_by_edi(scenario_obese_male, edi = "imd", thr = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  facet_grid(imd ~ . ) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Obese Male")

# INCOME

# obese female

income_prop_female <- svytable(~bmi_class_c + eqv_income, female_2019) %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese" & as.numeric(as.character(eqv_income)) > 0) %>% 
  mutate(sum = sum(Freq),
         share = Freq/sum)


scenario_obese_female <- do.call("rbind", mapply(make_by_edi, 
                                                 years, 
                                                 MoreArgs = list(class = "obese", sex_value = "female", edi = "eqv_income"), 
                                                 SIMPLIFY = F) ) %>% distinct()

# baseline distribution
ggplot(income_prop_female, aes(x = eqv_income, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = eqv_income, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Female")

# option B
make_pred_table_by_edi(scenario_obese_female, edi = "eqv_income", thr = c(0.5)) %>% 
  filter(rel_freq == 0.5) %>% 
  merge(., income_prop_female, by = "eqv_income") %>% 
  mutate(tot = female_ob,
         share_inc = tot*share,
         kcal_share = share_inc * diff) %>% 
  ggplot(., aes(x = eqv_income, y = kcal_share)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(eqv_income), label = paste(format(round(kcal_share / 1e6, 1), trim = TRUE), "M")), vjust= -1 , color = "white") +
  geom_text(aes(x = as.factor(eqv_income), label = label_percent(accuracy = 0.01)(share)), vjust= 1.5 ) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Obese Female")


# option A
make_pred_table_by_edi(scenario_obese_female, edi = "eqv_income", thr = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  facet_grid(eqv_income ~ . ) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Obese Female")

# obese male

income_prop_male <- svytable(~bmi_class_c + eqv_income, female_2019) %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese" & as.numeric(as.character(eqv_income)) > 0) %>% 
  mutate(sum = sum(Freq),
         share = Freq/sum)

scenario_obese_male <- do.call("rbind", mapply(make_by_edi, 
                                               years, 
                                               MoreArgs = list(class = "obese", sex_value = "male", edi = "eqv_income"), 
                                               SIMPLIFY = F) ) %>% distinct()
# baseline distribution

ggplot(income_prop_male, aes(x = eqv_income, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = eqv_income, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Male")

# option B
make_pred_table_by_edi(scenario_obese_male, edi = "eqv_income", thr = c(0.5)) %>% 
  filter(rel_freq == 0.5) %>% 
  merge(., income_prop_male, by = "eqv_income") %>% 
  mutate(tot = male_ob,
         share_inc = tot*share,
         kcal_share = share_inc * diff) %>% 
  ggplot(., aes(x = eqv_income, y = kcal_share)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(eqv_income), label = paste(format(round(kcal_share / 1e6, 1), trim = TRUE), "M")), vjust= -1 , color = "white") +
  geom_text(aes(x = as.factor(eqv_income), label = label_percent(accuracy = 0.01)(share)), vjust= 1.5 ) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Obese Male")


# option A
make_pred_table_by_edi(scenario_obese_male, edi = "eqv_income", thr = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  facet_grid(eqv_income ~ . ) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Obese Male")

# EDUCATION

# distribution in the population

svytable(~educ, svy_2019) %>% 
  as.data.frame() %>% 
  filter(as.numeric(as.character(educ)) > 0) %>% 
  mutate(educ = fct_reorder(educ,desc(educ))) %>% 
  mutate(share = Freq/sum(Freq)) %>% 
  ggplot(., aes(x = educ, y =share)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(x = as.factor(educ), label = label_percent(accuracy = 0.01)(share)) , hjust = 1,  color = "white") +
  coord_flip() 

# obese female

educ_prop_female <- svytable(~bmi_class_c + educ, female_2019) %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese" & as.numeric(as.character(educ)) > 0) %>% 
  mutate(sum = sum(Freq),
         share = Freq/sum)

scenario_obese_female <- do.call("rbind", mapply(make_by_edi, 
                                                 years, 
                                                 MoreArgs = list(class = "obese", sex_value = "female", edi = "educ"), 
                                                 SIMPLIFY = F) ) %>% distinct()

# baseline distribution
ggplot(educ_prop_female, aes(x = educ, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = educ, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Female")

# option B
make_pred_table_by_edi(scenario_obese_female, edi = "educ", thr = c(0.5)) %>% 
  filter(rel_freq == 0.5) %>% 
  merge(., educ_prop_female, by = "educ") %>% 
  mutate(tot = female_ob,
         share_inc = tot*share,
         kcal_share = share_inc * diff) %>% 
  ggplot(., aes(x = educ, y = kcal_share)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(educ), label = paste(format(round(kcal_share / 1e6, 1), trim = TRUE), "M")), vjust= -1 , color = "white") +
  geom_text(aes(x = as.factor(educ), label = label_percent(accuracy = 0.01)(share)), vjust= 1.5 ) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Obese Female")

# option A
make_pred_table_by_edi(scenario_obese_female, edi = "educ", thr = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  facet_grid(educ ~ . ) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Obese Female")

# obese male

educ_prop_male <- svytable(~bmi_class_c + educ, male_2019) %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese" & as.numeric(as.character(educ)) > 0) %>% 
  mutate(sum = sum(Freq),
         share = Freq/sum)


scenario_obese_male <- do.call("rbind", mapply(make_by_edi, 
                                               years, 
                                               MoreArgs = list(class = "obese", sex_value = "male", edi = "educ"), 
                                               SIMPLIFY = F) ) %>% distinct()

# basleine distribution
ggplot(educ_prop_male, aes(x = educ, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = educ, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Male")

# option B
make_pred_table_by_edi(scenario_obese_male, edi = "educ", thr = c(0.5)) %>% 
  filter(rel_freq == 0.5) %>% 
  merge(., educ_prop_male, by = "educ") %>% 
  mutate(tot = male_ob,
         share_inc = tot*share,
         kcal_share = share_inc * diff) %>% 
  ggplot(., aes(x = educ, y = kcal_share)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(educ), label = paste(format(round(kcal_share / 1e6, 1), trim = TRUE), "M")), vjust= -1 , color = "white") +
  geom_text(aes(x = as.factor(educ), label = label_percent(accuracy = 0.01)(share)), vjust= 1.5 ) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Obese Male")



# option A
make_pred_table_by_edi(scenario_obese_male, edi = "educ", thr = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  facet_grid(educ ~ . ) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Obese Male")



# ETHNICITY

# distribution in the population

svytable(~ethnic, svy_2019) %>% 
  as.data.frame() %>% 
  filter(as.numeric(as.character(ethnic)) > 0) %>% 
  mutate(ethnic = fct_reorder(ethnic,desc(ethnic))) %>% 
  mutate(share = Freq/sum(Freq)) %>% 
  ggplot(., aes(x = ethnic, y =share)) +
  geom_bar(stat = "identity") + 
  ylim(0,1) +
  geom_text(aes(x = as.factor(ethnic), label = label_percent(accuracy = 0.01)(share)) , hjust = -0.1,  color = "black") +
  coord_flip() 

# obese female

ethnic_prop_female <- svytable(~bmi_class_c + ethnic, female_2019) %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese" & as.numeric(as.character(ethnic)) > 0) %>% 
  mutate(sum = sum(Freq),
         share = Freq/sum)

scenario_obese_female <- do.call("rbind", mapply(make_by_edi, 
                                                 years, 
                                                 MoreArgs = list(class = "obese", sex_value = "female", edi = "ethnic"), 
                                                 SIMPLIFY = F) ) %>% distinct()

# baseline distribution
ggplot(ethnic_prop_female, aes(x = ethnic, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = ethnic, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Female")

# option B
make_pred_table_by_edi(scenario_obese_female, edi = "ethnic", thr = c(0.5)) %>% 
  filter(rel_freq == 0.5) %>% 
  merge(., ethnic_prop_female, by = "ethnic") %>% 
  mutate(tot = female_ob,
         share_inc = tot*share,
         kcal_share = share_inc * diff) %>% 
  ggplot(., aes(x = ethnic, y = kcal_share)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(ethnic), label = paste(format(round(kcal_share / 1e6, 1), trim = TRUE), "M")), vjust= -1 , color = "white") +
  geom_text(aes(x = as.factor(ethnic), label = label_percent(accuracy = 0.01)(share)), vjust= 1.5 ) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Obese Female")

# option A
make_pred_table_by_edi(scenario_obese_female, edi = "ethnic", thr = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  facet_grid(ethnic ~ . ) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Obese Female")

# obese male

ethnic_prop_male <- svytable(~bmi_class_c + ethnic, male_2019) %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese" & as.numeric(as.character(ethnic)) > 0) %>% 
  mutate(sum = sum(Freq),
         share = Freq/sum)


scenario_obese_male <- do.call("rbind", mapply(make_by_edi, 
                                               years, 
                                               MoreArgs = list(class = "obese", sex_value = "male", edi = "ethnic"), 
                                               SIMPLIFY = F) ) %>% distinct()

# baseline distribution
ggplot(ethnic_prop_male, aes(x = ethnic, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = ethnic, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Male")

# option B
make_pred_table_by_edi(scenario_obese_male, edi = "ethnic", thr = c(0.5)) %>% 
  filter(rel_freq == 0.5) %>% 
  merge(., ethnic_prop_male, by = "ethnic") %>% 
  mutate(tot = male_ob,
         share_inc = tot*share,
         kcal_share = share_inc * diff) %>% 
  ggplot(., aes(x = ethnic, y = kcal_share)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(ethnic), label = paste(format(round(kcal_share / 1e6, 1), trim = TRUE), "M")), vjust= -1 , color = "white") +
  geom_text(aes(x = as.factor(ethnic), label = label_percent(accuracy = 0.01)(share)), vjust= 1.5 ) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Obese Male")



# option A
make_pred_table_by_edi(scenario_obese_male, edi = "ethnic", thr = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  facet_grid(ethnic ~ . ) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Obese Male")


# Option C

# function that for a given EDI reports the prevalence of a given BMI class for each modelling year

make_bmi_prev <- function(year, class, sex_value, edi){
  
  svy_2019 <- svydesign(ids = ~df2019$psu,
                        nest = T,
                        data = df2019,
                        strata = df2019$strata,
                        weights = df2019$int_wt)
  
  group_2019 <- subset(svy_2019,  sex == sex_value)
  
  current_prev <- prop.table(svytable(eval(expr(~bmi_class_c + !! ensym(edi))), group_2019),2) %>% as.data.frame() %>% filter(bmi_class_c == class) %>% rename(bmi_class_final = bmi_class_c)
  
  year_dat <- all_full[[paste0("full_", year)]]
  
  svy_full <- svydesign(ids = ~year_dat$psu,
                        nest = T,
                        data = year_dat,
                        strata = year_dat$strata,
                        weights = year_dat$int_wt)
  
  group_full <- subset(svy_full,  sex == sex_value)
  
  scen_prev <- prop.table(svytable(eval(expr(~bmi_class_final + !! ensym(edi))), group_full),2) %>% as.data.frame() %>% filter(bmi_class_final == class)
  
  rbind(current_prev %>% mutate(year_model = 2019),
        scen_prev %>% mutate(year_model = year)) %>% 
    rename(.Freq = Freq)
}


# obese male
scenario_obese_male_edi <- do.call("rbind", mapply(make_bmi_prev, 
                                                   years, 
                                                   MoreArgs = list(class = "obese", sex_value = "male", edi = "imd"), 
                                                   SIMPLIFY = F) ) %>% distinct()

# obese female
scenario_obese_female_edi <- do.call("rbind", mapply(make_bmi_prev, 
                                                     years, 
                                                     MoreArgs = list(class = "obese", sex_value = "female", edi = "imd"), 
                                                     SIMPLIFY = F) ) %>% distinct()

# TESTED for IMD ONLY
# function that runs a linear regression of frequency of a BMI class against the relative overall obesity prevalence reduction and EDI group

make_pred_table_imd <- function(scenario_df, thresholds =c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)){
  plot_df <- merge(scenario_df, year_trend, by.x = "year_model", by.y = "year_c")
  mod <- lm(.Freq ~ poly(rel_freq,2)*imd, plot_df)
  
  extra <- expand.grid(rel_freq = seq(0, 0.5, 0.01), imd = as.factor(seq(1,5,1)))
  
  pred <- data.frame(pred = predict(mod, extra), extra) %>% 
    filter(rel_freq %in% c(0, thresholds))
  
  pred$base <- pred[which(pred$rel_freq == 0),1]
  
  pred$diff <- pred$pred - pred$base
  
  pred$pp <- pred$diff/pred$base
  
  return(pred)
}

# test plots - need to be refined
make_pred_table_imd(scenario_obese_male_edi) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = pred, fill = imd)) + 
  geom_bar(stat = "identity") + 
  facet_grid(imd ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pred)), vjust= 1.5 ) +
  labs(title = "Obese Men")


make_pred_table_imd(scenario_obese_female_edi) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = pred, fill = imd)) + 
  geom_bar(stat = "identity") + 
  facet_grid(imd ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pred)), vjust= 1.5 ) + 
  labs(title = "Obese Women")
