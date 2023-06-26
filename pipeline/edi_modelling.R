rm(list = ls())

library(here)
library(tidyverse)
library(reshape2)
library(bw)
library(hrbrthemes)
library(survey)
library(scales)
library(bw)

theme_set(theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta"))

# read files and assign names
files <- list.files(here("outputs", "reports"), pattern = "morb|obese|over", full.names = T) 

files <- files[grepl(here("outputs", "reports","scenario"), files)]

scenarios <- lapply(files, read_csv)

names <- list.files(here("outputs", "reports"), pattern = "morb|obese|over") 

names <- names[grepl("scenario", names)]

names(scenarios) <- gsub("scenario_", "", gsub(".csv", "", names))

# append in a dataset
scenarios_df <- do.call(rbind, scenarios)

scenarios_df$scenario <- rep(names(scenarios), sapply(scenarios, nrow))

scenarios_df$sex <- str_split_fixed(scenarios_df$scenario, "_",2)[,2]

scenarios_df$bmi_class_m <- str_split_fixed(scenarios_df$scenario, "_",2)[,1]

# read 2019 file

df2019 <- read_csv(here("outputs", "data",  "shs_2019_adult_clean.csv")) %>% 
  filter(!is.na(int_wt) & bmi > 0) %>% 
  mutate(bmi_class_c = case_when(bmi < 18.5 ~ "underweight",
                                 (bmi >= 18.5 & bmi < 25) ~ "normal",
                                 (bmi >= 25 & bmi < 30) ~ "overweight",
                                 (bmi >= 30 & bmi < 40) ~ "obese",
                                 bmi >= 40 ~ "morbidly obese")) %>% 
  mutate(sex = ifelse(sex == 2 ,"female", "male")) %>% 
  mutate(intake = pal*rmr) %>% 
  mutate(ethnic_2 = ifelse(ethnic %in% c(4,5),4, ethnic))


# function to find weight given ei change

find_weight <- function(val) {
  scenario_val <- scenarios_df %>% 
    filter(rel_freq == val) %>% 
    mutate(bmi_class_c = case_when(bmi_class_m == "morb" ~ "morbidly obese", 
                                   bmi_class_m == "over" ~ "overweight",
                                   TRUE ~ bmi_class_m)) %>% 
    left_join(df2019, by = c("sex", "bmi_class_c")) %>% 
    mutate(pp = ifelse(is.na(pp),0, pp)) %>% 
    mutate(new_intake = intake*(1 + pp)) %>% 
    mutate(intake_diff = new_intake - intake)
  
  weights <- scenario_val$wt
  heights <- scenario_val$ht/100
  ages <- scenario_val$age
  sexes <- scenario_val$sex
  
  eichange <- t(apply(scenario_val, 1, function(x) rep(as.numeric(x["intake_diff"]), 365*3)))
  
  #eichange <- energy_build(cbind(rep(0,nrow(scenario_val)), scenario_val$intake_diff), c(0, 365*3))
  
  model_weight <- adult_weight(weights, heights, ages, sexes, eichange, days = 365*3)
  
  
  bw <- model_weight$Body_Weight %>% as.data.frame() %>% select(V1095) %>% rename(final_weight = V1095)
  bmi <- model_weight$Body_Mass_Index %>% as.data.frame() %>% select(V1095) %>% rename(final_bmi = V1095)
  
  out <- plyr::rbind.fill(cbind(scenario_val, bw, bmi) %>% 
                            mutate(final_bmi_class_c = case_when(final_bmi < 18.5 ~ "underweight",
                                                                 (final_bmi >= 18.5 & final_bmi < 25) ~ "normal",
                                                                 (final_bmi >= 25 & final_bmi < 30) ~ "overweight",
                                                                 (final_bmi >= 30 & final_bmi < 40) ~ "obese",
                                                                 final_bmi >= 40 ~ "morbidly obese")), 
                          df2019 %>% filter(bmi_class_c %in% c("underweight", "normal"))) %>% 
    mutate(new_intake = ifelse(is.na(new_intake), intake, new_intake),
           final_weight = ifelse(is.na(final_weight), wt, final_weight),
           final_bmi = ifelse(is.na(final_bmi), bmi, final_bmi),
           final_bmi_class_c = ifelse(is.na(final_bmi_class_c), bmi_class_c, final_bmi_class_c),
           rel_freq = max(rel_freq, na.rm = T))
  return(out)
 
}


all_weight <- lapply(c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5), find_weight)

names(all_weight) <- paste0("red_", c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5))


# inputs: reduction, sex, edi, bmiclass


make_table <- function(df, sex_value, edi, class){
  
  df <- df %>% filter(!! ensym(edi) >0 )

  svy_des <- svydesign(ids = ~df$psu,
                       nest = T,
                       data = df,
                       strata = df$strata,
                       weights = df$int_wt)
  
  sub <- subset(svy_des, sex == sex_value)
  
  prop <- prop.table(svytable(eval(expr(~final_bmi_class_c + !! ensym(edi))), sub),2) %>% 
    as.data.frame() %>% 
    filter(final_bmi_class_c == class) %>% 
    mutate(level = max(df$rel_freq))
  
  return(prop)
  
}

perc = c("0%", "5%", "10%", "20%", "30%", "40%", "50%")

# IMD

png(here("outputs", "figures", "png", "female_imd.png"), units = "px", width = 1000, height = 800, res = 100)
do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "female", class = "obese", edi = "imd"),
               SIMPLIFY = F)) %>%
  mutate(rel_freq = factor(label_percent()(level), levels = perc, labels = perc)) %>% 
  mutate(imd_l = factor(imd, labels = c("1 - most deprived", "2", "3", "4", "5 - least deprived"))) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = Freq, fill = imd_l)) + 
  geom_bar(stat = "identity") + 
  facet_grid(imd_l ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(Freq)), vjust= 1.5 ) + 
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  labs(title = "Women: Obesity Prevalence by SIMD", 
       x = "Obesity Prevalence Reduction Compared to 2019",
       y = "Prevalence",
       fill = "SIMD")
dev.off()


png(here("outputs", "figures", "png", "male_imd.png"), units = "px", width = 1000, height = 800, res = 100)

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "male", class = "obese", edi = "imd"),
               SIMPLIFY = F)) %>%
  mutate(rel_freq = factor(label_percent()(level), levels = perc, labels = perc)) %>% 
  mutate(imd_l = factor(imd, labels = c("1 - most deprived", "2", "3", "4", "5 - least deprived"))) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = Freq, fill = imd_l)) + 
  geom_bar(stat = "identity") + 
  facet_grid(imd_l ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(Freq)), vjust= 1.5 ) + 
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  labs(title = "Men: Obesity Prevalence by SIMD", 
       x = "Obesity Prevalence Reduction Compared to 2019",
       y = "Prevalence",
       fill = "SIMD")
dev.off()

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "male", class = "obese", edi = "imd"),
               SIMPLIFY = F)) %>% 
  dcast(., final_bmi_class_c + imd ~ level, value.var = "Freq") %>% 
  mutate(diff = (`0.5` - `0`)/`0`)


do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "female", class = "obese", edi = "imd"),
               SIMPLIFY = F)) %>% 
  dcast(., final_bmi_class_c + imd ~ level, value.var = "Freq") %>% 
  mutate(diff = (`0.5` - `0`)/`0`)

# ETHNICITY

png(here("outputs", "figures", "png", "female_ethnic.png"), units = "px", width = 1000, height = 800, res = 100)

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "female", class = "obese", edi = "ethnic_2"),
               SIMPLIFY = F)) %>%
  mutate(rel_freq = factor(label_percent()(level), levels = perc, labels = perc)) %>% 
  mutate(ethnic_l = factor(ethnic_2, labels = c("White Scottish", "White British", "White Other", "Asian and Other Minority Ethnic"))) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = Freq, fill = ethnic_l)) + 
  geom_bar(stat = "identity") + 
  facet_grid(ethnic_l ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(Freq)), vjust= 1.5 ) + 
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  labs(title = "Women: Obesity Prevalence by Ethnic Group", 
       x = "Obesity Prevalence Reduction Compared to 2019",
       y = "Prevalence",
       fill = "Ethnic Group")
dev.off()

png(here("outputs", "figures", "png", "male_ethnic.png"), units = "px", width = 1000, height = 800, res = 100)

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "male", class = "obese", edi = "ethnic_2"),
               SIMPLIFY = F)) %>%
  mutate(rel_freq = factor(label_percent()(level), levels = perc, labels = perc)) %>% 
  mutate(ethnic_l = factor(ethnic_2, labels = c("White Scottish", "White British", "White Other", "Asian and Other Minority Ethnic"))) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = Freq, fill = ethnic_l)) + 
  geom_bar(stat = "identity") + 
  facet_grid(ethnic_l ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(Freq)), vjust= 1.5 ) + 
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  labs(title = "Men: Obesity Prevalence by Ethnic Group", 
       x = "Obesity Prevalence Reduction Compared to 2019",
       y = "Prevalence",
       fill = "Ethnic Group")
dev.off()

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "male", class = "obese", edi = "ethnic"),
               SIMPLIFY = F)) %>% 
  dcast(., final_bmi_class_c + ethnic ~ level, value.var = "Freq") %>% 
  mutate(diff = (`0.5` - `0`)/`0`)


do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "female", class = "obese", edi = "ethnic"),
               SIMPLIFY = F)) %>% 
  dcast(., final_bmi_class_c + ethnic ~ level, value.var = "Freq") %>% 
  mutate(diff = (`0.5` - `0`)/`0`)

# INCOME

png(here("outputs", "figures", "png", "female_income.png"), units = "px", width = 1000, height = 800, res = 100)

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "female", class = "obese", edi = "eqv_income"),
               SIMPLIFY = F)) %>%
  mutate(rel_freq = factor(label_percent()(level), levels = perc, labels = perc)) %>% 
  mutate(eqv_income_l = factor(eqv_income, labels = c("Top Q", "2nd Q", "3rd Q", "4th Q", "Bottom Q"))) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = Freq, fill = eqv_income_l)) + 
  geom_bar(stat = "identity") + 
  facet_grid(eqv_income_l ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(Freq)), vjust= 1.5 ) + 
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  labs(title = "Women: Obesity Prevalence by Income Quintile", 
       x = "Obesity Prevalence Reduction Compared to 2019",
       y = "Prevalence",
       fill = "Income Quintile")
dev.off()


png(here("outputs", "figures", "png", "male_income.png"), units = "px", width = 1000, height = 800, res = 100)

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "male", class = "obese", edi = "eqv_income"),
               SIMPLIFY = F)) %>%
  mutate(rel_freq = factor(label_percent()(level), levels = perc, labels = perc)) %>% 
  mutate(eqv_income_l = factor(eqv_income, labels = c("Top Q", "2nd Q", "3rd Q", "4th Q", "Bottom Q"))) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = Freq, fill = eqv_income_l)) + 
  geom_bar(stat = "identity") + 
  facet_grid(eqv_income_l ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(Freq)), vjust= 1.5 ) + 
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  labs(title = "Men: Obesity Prevalence by Income Quintile", 
       x = "Obesity Prevalence Reduction Compared to 2019",
       y = "Prevalence",
       fill = "Income Quintile")
dev.off()

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "male", class = "obese", edi = "eqv_income"),
               SIMPLIFY = F)) %>% 
  dcast(., final_bmi_class_c + eqv_income ~ level, value.var = "Freq") %>% 
  mutate(diff = (`0.5` - `0`)/`0`)


do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "female", class = "obese", edi = "eqv_income"),
               SIMPLIFY = F)) %>% 
  dcast(., final_bmi_class_c + eqv_income ~ level, value.var = "Freq") %>% 
  mutate(diff = (`0.5` - `0`)/`0`)

# EDUCATION

png(here("outputs", "figures", "png", "female_education.png"), units = "px", width = 1000, height = 800, res = 100)

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "female", class = "obese", edi = "educ"),
               SIMPLIFY = F)) %>%
  mutate(rel_freq = factor(label_percent()(level), levels = perc, labels = perc)) %>% 
  mutate(educ_l = factor(educ, labels = c("Degree or higher", "HNC/D", "Higher Grade", "Standard Grade", "Other", "No qual"))) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = Freq, fill = educ_l)) + 
  geom_bar(stat = "identity") + 
  facet_grid(educ_l ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(Freq)), vjust= 1.5 ) + 
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  labs(title = "Women: Obesity Prevalence by Education Level", 
       x = "Obesity Prevalence Reduction Compared to 2019",
       y = "Prevalence",
       fill = "Education Level")
dev.off()

png(here("outputs", "figures", "png", "male_education.png"), units = "px", width = 1000, height = 800, res = 100)

do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "male", class = "obese", edi = "educ"),
               SIMPLIFY = F)) %>%
  mutate(rel_freq = factor(label_percent()(level), levels = perc, labels = perc)) %>% 
  mutate(educ_l = factor(educ, labels = c("Degree or higher", "HNC/D", "Higher Grade", "Standard Grade", "Other", "No qual"))) %>% 
  ggplot(., aes(x = as.factor(rel_freq), y = Freq, fill = educ_l)) + 
  geom_bar(stat = "identity") + 
  facet_grid(educ_l ~ .) + 
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(accuracy = 0.01)(Freq)), vjust= 1.5 ) + 
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  labs(title = "Men: Obesity Prevalence by Education Level", 
       x = "Obesity Prevalence Reduction Compared to 2019",
       y = "Prevalence",
       fill = "Education Level")
dev.off()


do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "male", class = "obese", edi = "educ"),
               SIMPLIFY = F)) %>% 
  dcast(., final_bmi_class_c + educ ~ level, value.var = "Freq") %>% 
  mutate(diff = (`0.5` - `0`)/`0`)


do.call(rbind, 
        mapply(make_table, 
               all_weight, 
               MoreArgs = list(sex_value = "female", class = "obese", edi = "educ"),
               SIMPLIFY = F)) %>% 
  dcast(., final_bmi_class_c + educ ~ level, value.var = "Freq") %>% 
  mutate(diff = (`0.5` - `0`)/`0`)


