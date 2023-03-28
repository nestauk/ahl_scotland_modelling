library(tidyverse)
library(here)
library(survey)
library(plotly)
library(magrittr)
library(reshape2)

get_select <- function(x){
  df <- read_csv(x) %>% 
    select(id, year, psu, strata, int_wt, bmi, ht, wt, age, sex)
  return(df)
}

df <- do.call("rbind" , lapply(list.files(here("outputs", "data"), "adult", full.names = T), get_select))

# drop 2 obeservations in 2011 that had missing weights and filter to valid bmi

df_clean <- df %>% filter(!is.na(int_wt) & bmi > 0) %>% 
  mutate(bmi_class_c = case_when(bmi < 18.5 ~ "underweight",
                                 (bmi >= 18.5 & bmi < 25) ~ "normal",
                                 (bmi >= 25 & bmi < 30) ~ "overweight",
                                 (bmi >= 30 & bmi < 40) ~ "obese",
                                 bmi >= 40 ~ "morbidly obese")) %>% 
  mutate(year_c = case_when((year <= 0) ~ year + 2003,
                            (year >=1) ~ year +2007))

# declare survey design
svy.dat <- svydesign(ids=~df_clean$psu, 
                     nest = T,
                     data=df_clean,
                     strata = df_clean$strata,
                     weights=df_clean$int_wt)

prevalence <- prop.table(svytable(~year_c + bmi_class_c,svy.dat),1)

prevalence %>% 
  as.data.frame() %>% 
  filter(bmi_class_c == "obese") %>% 
  mutate(chosen = ifelse(year_c %in% c(1995, 1998, 2003, 2012, 2019),2,0)) %>% 
  filter(chosen == 2) %>% 
  mutate(rel_freq = 1 - Freq/prevalence[45]) %T>% 
  write_csv(here("outputs/reports/obesity_change.csv"))
