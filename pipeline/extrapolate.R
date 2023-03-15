rm(list = ls())

library(tidyverse)
library(here)
library(reshape2)
library(survey)
library(scales)
library(gridExtra)
library(magrittr)

# read 2019 file

df2019 <- read_csv(here("outputs", "data",  "shs_2019_adult_clean.csv")) %>% 
  filter(!is.na(int_wt) & bmi > 0) %>% 
  mutate(bmi_class_c = case_when(bmi < 18.5 ~ "underweight",
                                 (bmi >= 18.5 & bmi < 25) ~ "normal",
                                 (bmi >= 25 & bmi < 30) ~ "overweight",
                                 (bmi >= 30 & bmi < 40) ~ "obese",
                                 bmi >= 40 ~ "morbidly obese")) %>% 
  mutate(sex = ifelse(sex == 2 ,"female", "male")) 

# mean weight change

weight_change <- lapply(list.files(here("outputs", "reports"), "weight_change", full.names = T), read_csv)

names(weight_change) <- c("wc_1995", "wc_1998", "wc_2003", "wc_2012")

# for each year merge data to get weight target

get_weight_target <- function(year){
  w_change <- weight_change[[paste0("wc_", year)]] %>% 
    dplyr::select(bmi_class_c, female, male) %>% 
    melt(., id.vars = "bmi_class_c")
  
  dat <- merge(df2019, w_change, by.x = c("bmi_class_c", "sex"), by.y = c("bmi_class_c", "variable")) %>% 
    mutate(target = wt*(1 + value/100))
  
  return(dat)
}

weight_target <- lapply(c(1995, 1998, 2003, 2012), get_weight_target)

names(weight_target) <- c("wt_1995", "wt_1998", "wt_2003", "wt_2012")

# read new intake files

get_intake <- lapply(list.files(here("outputs", "data"), "morb|overweight|obese", full.names = T), read_csv)

names(get_intake) <- sub(".csv", "", list.files(here("outputs", "data"), "morb|overweight|obese"))

# merge intake files with 2019 data

make_full <- function(year){
  full <- bind_rows(get_intake[grep(year, names(get_intake))], .id = "info") %>% 
    separate(info, c("type", "year_model"), "_") %>% 
    select(-type) %>% 
    merge(., weight_target[[paste0("wt_", year)]], by = "id", all = T) %>% 
    mutate(bmi_final = target/(ht/100)^2) %>% 
    mutate(bmi_class_final = case_when(bmi_final <= 18.5 ~ "underweight",
                                       bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                       bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                       bmi_final >= 30 & bmi_final < 40 ~ "obese",
                                       bmi_final >= 40 ~ "morbidly obese",
                                       TRUE ~ "NA")) %>% 
    mutate(intake_start = pal*rmr,
           intake_end = ifelse(bmi_class_c %in% c("underweight", "normal"), intake_start, (pal*rmr) + ei)) 
  return(full)
}

all_full <- lapply(c(1995, 1998, 2003, 2012), make_full)

names(all_full) <- c("full_1995", "full_1998", "full_2003", "full_2012")

# average intake by scenario 

make_avg_ei <- function(year, class, sex_value){
  df2019 <- df2019 %>% mutate(intake = pal*rmr)

  svy_2019 <- svydesign(ids = ~df2019$psu,
                      nest = T,
                      data = df2019,
                      strata = df2019$strata,
                      weights = df2019$int_wt)

  group_2019 <- subset(svy_2019, bmi_class_c %in% class & sex == sex_value)

  current_ei <- svymean(~intake, group_2019)[[1]]
  
  year_dat <- all_full[[paste0("full_", year)]]

  svy_full <- svydesign(ids = ~year_dat$psu,
                      nest = T,
                      data = year_dat,
                      strata = year_dat$strata,
                      weights = year_dat$int_wt)

  group_full <- subset(svy_full, bmi_class_c %in% class & sex == sex_value)

  rbind(svyby(~intake_end, ~year_model, group_full, svymean ) %>% 
    as.data.frame() %>% 
    select(year_model, intake_end),
    data.frame(year_model = 2019, intake_end = current_ei))
}

# overweight female
scenario_over_female <- do.call("rbind", mapply(make_avg_ei, 
                                         c(1995, 1998, 2003, 2012), 
                                         MoreArgs = list(class = "overweight", sex_value = "female"), 
                                         SIMPLIFY = F) ) %>% distinct()

# overweight male
scenario_over_male <- do.call("rbind", mapply(make_avg_ei, 
                                                c(1995, 1998, 2003, 2012), 
                                                MoreArgs = list(class = "overweight", sex_value = "male"), 
                                                SIMPLIFY = F) ) %>% distinct()

# obese female
scenario_obese_female <- do.call("rbind", mapply(make_avg_ei, 
                                          c(1995, 1998, 2003, 2012), 
                                          MoreArgs = list(class = "obese", sex_value = "female"), 
                                          SIMPLIFY = F) ) %>% distinct()

# obese male
scenario_obese_male <- do.call("rbind", mapply(make_avg_ei, 
                                                 c(1995, 1998, 2003, 2012), 
                                                 MoreArgs = list(class = "obese", sex_value = "male"), 
                                                 SIMPLIFY = F) ) %>% distinct()

# morbidly obese female
scenario_morb_female <- do.call("rbind", mapply(make_avg_ei, 
                                         c(1995, 1998, 2003, 2012), 
                                         MoreArgs = list(class = "morbidly obese", sex_value = "female"),
                                         SIMPLIFY = F) ) %>% distinct()

# morbidly obese male
scenario_morb_male <- do.call("rbind", mapply(make_avg_ei, 
                                                c(1995, 1998, 2003, 2012), 
                                                MoreArgs = list(class = "morbidly obese", sex_value = "male"),
                                                SIMPLIFY = F) ) %>% distinct()

# excess female
scenario_excess_female <- do.call("rbind", mapply(make_avg_ei, 
                                           c(1995, 1998, 2003, 2012), 
                                           MoreArgs = list(class = c("overweight","obese", "morbidly obese"), sex_value = "female"), 
                                           SIMPLIFY = F) ) %>% distinct()

# excess male
scenario_excess_male <- do.call("rbind", mapply(make_avg_ei, 
                                                  c(1995, 1998, 2003, 2012), 
                                                  MoreArgs = list(class = c("overweight","obese", "morbidly obese"), sex_value = "male"), 
                                                  SIMPLIFY = F) ) %>% distinct()

# pop female
scenario_pop_female <- do.call("rbind", mapply(make_avg_ei, 
                                                  c(1995, 1998, 2003, 2012), 
                                                  MoreArgs = list(class = c("underweight","normal" , "overweight","obese", "morbidly obese"), sex_value = "female"), 
                                                  SIMPLIFY = F) ) %>% distinct()

# pop male
scenario_pop_male <- do.call("rbind", mapply(make_avg_ei, 
                                                c(1995, 1998, 2003, 2012), 
                                                MoreArgs = list(class = c("underweight","normal", "overweight","obese", "morbidly obese"), sex_value = "male"), 
                                                SIMPLIFY = F) ) %>% distinct()


#############


year_trend <- read_csv(here("outputs/reports/obesity_change.csv"))


make_pred_table <- function(scenario_df, thresholds =c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)){
  plot_df <- merge(scenario_df, year_trend, by.x = "year_model", by.y = "year_c")
  mod <- lm(intake_end ~ rel_freq, plot_df)

  extra <- data.frame(rel_freq = seq(0, 0.5, 0.01))

  pred <- data.frame(pred = predict(mod, extra), extra) %>% 
    filter(rel_freq %in% c(0, thresholds))

  pred$base <- pred[which(pred$rel_freq == 0),1]

  pred$diff <- pred$pred - pred$base

  pred$pp <- pred$diff/pred$base

  return(pred)
}


# list all df with scenarios

sc<-do.call("list",mget(grep("scenario",names(.GlobalEnv),value=TRUE)))

# generate reports

reports <- lapply(sc, make_pred_table)

# save reports

for (i in 1:length(reports)){
  write_csv(reports[[i]], here("outputs", "reports", paste0(names(reports)[i], '.csv')))
}


# plots

png(here("outputs", "figures", "png", "overweight.png"))
grid.arrange(
  make_pred_table(scenario_over_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Overweight Female"),
  make_pred_table(scenario_over_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Overweight Male"),
  nrow = 1)
dev.off()

png(here("outputs", "figures", "png", "obese.png"))
grid.arrange(
  make_pred_table(scenario_obese_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Obese Female"),
  make_pred_table(scenario_obese_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Obese Male"),
  nrow = 1)
dev.off()

png(here("outputs", "figures", "png", "morbidly_obese.png"))
grid.arrange(
  make_pred_table(scenario_morb_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Morbidly Obese Female"),
  make_pred_table(scenario_morb_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Morbidly Obese Male"),
  nrow = 1)
dev.off()

png(here("outputs", "figures", "png", "excess_weight.png"))
grid.arrange(
  make_pred_table(scenario_excess_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Excess Weight Female"),
  make_pred_table(scenario_excess_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Excess Weight Male"),
  nrow = 1)
dev.off()

png(here("outputs", "figures", "png", "population.png"))
grid.arrange(
  make_pred_table(scenario_pop_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Population Female"),
  make_pred_table(scenario_pop_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Population Male"),
  nrow = 1)
dev.off()

# example plots

plot_df <- merge(scenario_obese_male, year_trend, by.x = "year_model", by.y = "year_c")

plot_df %>% 
  ggplot(., aes(x = rel_freq, y = intake_end)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  ggtitle("Example: obese men")

mod <- lm(intake_end ~ rel_freq, plot_df)

extra <- data.frame(rel_freq = seq(0, 0.5, 0.01))

pred <- data.frame(pred = predict(mod, extra), extra) 

pred %>% 
  ggplot(., aes(x = rel_freq, y = pred)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  ggtitle("Example: obese men")

################################################################

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




make_pred_table_by_edi <- function(scenario, edi, thr){
  
  df_split <- scenario %>% group_by(!! ensym(edi))
  
  df_list <- group_split(df_split) %>% set_names(unlist(group_keys(df_split))) 
  
  df_table <- lapply(df_list, make_pred_table, thresholds = thr)

  tst <- do.call("rbind", df_table)
  
  tst[[edi]] <- substr(rownames(tst),1,1)

  return(tst)
  }

# edi

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

# get proportions to use as weights

imd_prop_female <- svytable(~bmi_class_c + imd, female_2019) %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese") %>% 
  mutate(sum = sum(Freq),
         share = Freq/sum)

options(scipen = 999)

scenario_obese_female <- do.call("rbind", mapply(make_by_edi, 
                                                c(1995, 1998, 2003, 2012), 
                                                MoreArgs = list(class = "obese", sex_value = "female", edi = "imd"), 
                                                SIMPLIFY = F) ) %>% distinct()

ggplot(imd_prop_female, aes(x = imd, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = imd, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese women")

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
                                                 c(1995, 1998, 2003, 2012), 
                                                 MoreArgs = list(class = "obese", sex_value = "male", edi = "imd"), 
                                                 SIMPLIFY = F) ) %>% distinct()

ggplot(imd_prop_male, aes(x = imd, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = imd, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Male")

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

make_pred_table_by_edi(scenario_obese_male, edi = "imd", thr = c(0.5)) %>% 
  filter(rel_freq == 0.5) %>% 
  merge(., imd_prop %>% filter(sex == "male"), by = "imd") %>% 
  mutate(tot = male_ob,
         share_imd = tot*share,
         kcal_share = share_imd * diff) %>% 
  ggplot(., aes(x = imd, y = kcal_share)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(imd), label = paste(format(round(kcal_share / 1e6, 1), trim = TRUE), "M")), vjust= -1 , color = "white") +
  geom_text(aes(x = as.factor(imd), label = label_percent(accuracy = 0.01)(share)), vjust= 1.5 ) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Obese Male")

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
                                                 c(1995, 1998, 2003, 2012), 
                                                 MoreArgs = list(class = "obese", sex_value = "female", edi = "eqv_income"), 
                                                 SIMPLIFY = F) ) %>% distinct()


ggplot(income_prop_female, aes(x = eqv_income, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = eqv_income, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Female")

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
                                               c(1995, 1998, 2003, 2012), 
                                               MoreArgs = list(class = "obese", sex_value = "male", edi = "eqv_income"), 
                                               SIMPLIFY = F) ) %>% distinct()

ggplot(income_prop_male, aes(x = eqv_income, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = eqv_income, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Male")

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
                                                 c(1995, 1998, 2003, 2012), 
                                                 MoreArgs = list(class = "obese", sex_value = "female", edi = "educ"), 
                                                 SIMPLIFY = F) ) %>% distinct()

ggplot(educ_prop_female, aes(x = educ, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = educ, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Female")

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
                                               c(1995, 1998, 2003, 2012), 
                                               MoreArgs = list(class = "obese", sex_value = "male", edi = "educ"), 
                                               SIMPLIFY = F) ) %>% distinct()

ggplot(educ_prop_male, aes(x = educ, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = educ, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Male")

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
                                                 c(1995, 1998, 2003, 2012), 
                                                 MoreArgs = list(class = "obese", sex_value = "female", edi = "ethnic"), 
                                                 SIMPLIFY = F) ) %>% distinct()

ggplot(ethnic_prop_female, aes(x = ethnic, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = ethnic, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Female")

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
                                               c(1995, 1998, 2003, 2012), 
                                               MoreArgs = list(class = "obese", sex_value = "male", edi = "ethnic"), 
                                               SIMPLIFY = F) ) %>% distinct()

ggplot(ethnic_prop_male, aes(x = ethnic, y=share)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = ethnic, label = label_percent(accuracy = 0.01)(share)), color = "white", vjust = 1.5) +
  labs(title = "Obese Male")

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



make_pred_table_by_edi(scenario_obese_male, edi = "ethnic", thr = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
  facet_grid(ethnic ~ . ) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
  labs(title = "Obese Male")



