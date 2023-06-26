rm(list = ls())

library(here)
library(tidyverse)
library(reshape2)
library(bw)
library(hrbrthemes)
library(survey)
library(scales)
library(gganimate)


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
  mutate(intake = pal*rmr)


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


# 

make_table_sex <- function(df){
  
  svy_des <- svydesign(ids = ~df$psu,
                       nest = T,
                       data = df,
                       strata = df$strata,
                       weights = df$int_wt)
  
  
  prop <- prop.table(svytable(~final_bmi_class_c + sex, svy_des),2) %>% 
    as.data.frame() %>% 
    mutate(level = max(df$rel_freq)) %>% 
    mutate(final_bmi_class_c = factor(final_bmi_class_c, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese")))
  
  return(prop)
  
}

perc = c("0%", "5%", "10%", "20%", "30%", "40%", "50%")

tbl1 <- do.call(rbind, lapply(all_weight, make_table_sex)) %>% 
  mutate(state = factor(label_percent()(level), levels = perc)) %>% 
  mutate(pp = label_percent()(Freq))

  
p1 <- ggplot(tbl1, aes(x = final_bmi_class_c, y =Freq)) +
  geom_bar(stat = "identity", fill = "#0000ff") +
  facet_grid(~ sex) +
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  geom_text(aes(label = as.factor(pp)), vjust= -0.5 , color = "black") +
  ggtitle("Obesity Reduction: {closest_state}") +
    labs(x = "BMI class",
       y = "Prevalence") +
    theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
      transition_states(state,3,3)
p1


anim_save(here("outputs", "figures", "gif", "distribution_by_sex.gif"), p1)


# 
make_table <- function(df){
  
  svy_des <- svydesign(ids = ~df$psu,
                       nest = T,
                       data = df,
                       strata = df$strata,
                       weights = df$int_wt)
  
  
  prop <- prop.table(svytable(~final_bmi_class_c, svy_des)) %>% 
    as.data.frame() %>% 
    mutate(level = max(df$rel_freq)) %>% 
    mutate(final_bmi_class_c = factor(final_bmi_class_c, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese")))
  
  return(prop)
  
}


tbl2 <- do.call(rbind, lapply(all_weight, make_table)) %>% 
  mutate(state = factor(label_percent()(level), levels = perc)) %>% 
  mutate(pp = label_percent(1)(Freq))


p2 <- ggplot(tbl2, aes(x = final_bmi_class_c, y =Freq)) +
  geom_bar(stat = "identity", fill = "#0000ff") +
  scale_y_continuous (labels = scales::percent_format (accuracy=1)) +
  geom_text(aes(label = as.factor(pp)), vjust= -0.5 , color = "black") +
  ggtitle("Obesity Reduction: {closest_state}") +
  labs(x = "BMI class",
       y = "Prevalence") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  transition_states(state,3,3)
p2


anim_save(here("outputs", "figures", "gif", "distribution.gif"), p2)
