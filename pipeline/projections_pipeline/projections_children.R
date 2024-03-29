rm(list = ls())

library(tidyverse)
library(here)
library(survey)
library(plotly)
library(magrittr)
library(reshape2)
library(readxl)
library(purrr)
library(broom)
library(tibble)
library(hrbrthemes)

theme_set(theme_ipsum())

# read data

get_select <- function(x){
  df <- read_csv(x) %>% 
    select(id, year, psu, strata, cint_wt, bmi, ht, wt, age, sex, child_bmi_class)
  return(df)
}

df <- do.call("rbind" , lapply(list.files(here("outputs", "data"), "child", full.names = T), get_select))

# drop obeservations with invalid BMI

df_clean <- df %>% 
  filter(!is.na(cint_wt) & bmi > 0 & child_bmi_class > 0) %>% 
  filter(age >= 2) %>% 
  filter(year > 0) %>% 
  mutate(bmi_class_c = case_when(child_bmi_class == 1 ~ "underweight",
                                 (child_bmi_class == 2) ~ "normal",
                                 (child_bmi_class == 3) ~ "overweight",
                                 (child_bmi_class == 4) ~ "obese",
                                 (child_bmi_class == 5) ~ "morbidlyobese",
                                 TRUE ~ "unknown")) %>% 
  mutate(year_c = case_when((year <= 0) ~ year + 2003,
                            (year >=1) ~ year +2007))

# declare survey design
svy.dat <- svydesign(ids=~df_clean$psu, 
                     nest = T,
                     data=df_clean,
                     strata = df_clean$strata,
                     weights=df_clean$cint_wt)

# create subsets by sex
svy.female <- subset(svy.dat, sex == 2)

svy.male <- subset(svy.dat, sex == 1)

# bmi class prevalence

prevalence_all <- prop.table(svytable(~year_c + bmi_class_c,svy.dat),1)

prevalence_female <- prop.table(svytable(~year_c + bmi_class_c,svy.female),1)

prevalence_male <- prop.table(svytable(~year_c + bmi_class_c,svy.male),1)


# population data (mid year population estimates)

pop <- read_excel(here("inputs", "data", "mid-year-pop-est-21-time-series-data.xlsx"), sheet = "Table_6", range = "A6:CP129") 


# children data
under16 <- pop %>% 
  select(- `All Ages`) %>% 
  melt(., id.vars = c("Year", "Sex")) %>% 
  filter(variable %in%  c(0,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,15)) %>% 
  group_by(Year, Sex) %>% 
  summarise(under16 = sum(value))


# read in population projections
proj <- read_csv(here("inputs", "data", "pop-proj-2020-profile-custom.csv")) %>% 
  melt(., id.vars = c("Variant", "Year", "Sex")) %>% 
  mutate(age = gsub("Age_", "", variable))


# children data
under16_proj <- rbind(proj %>% 
                        filter(age %in%  c(0,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,15)) %>% 
                        group_by(Year, Sex) %>% 
                        summarise(under16_proj = sum(value)),
                      proj %>% 
                        filter(age %in%  c(0,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,15)) %>% 
                        group_by(Year) %>% 
                        summarise(under16_proj = sum(value)) %>% 
                        mutate(Sex = "Persons"))

# combine datasets to get prevalence given a bmi class

get_prevalence_children <- function(bmi_class){
  if (length(bmi_class) == 1){
  p <- rbind(prevalence_female %>% 
               as.data.frame() %>% 
               filter(bmi_class_c == bmi_class) %>% 
               mutate(Sex = "Females"),
             prevalence_male %>% 
               as.data.frame() %>% 
               filter(bmi_class_c == bmi_class) %>% 
               mutate(Sex = "Males"),
             prevalence_all %>% 
               as.data.frame() %>% 
               filter(bmi_class_c == bmi_class) %>% 
               mutate(Sex = "Persons")) %>% 
    mutate(Year = as.numeric(as.character(year_c))) %>% 
    merge(under16, by = c("Year", "Sex")) %>% 
    mutate(prevalence = under16*Freq) %>% 
    select(Year, Sex, prevalence)
    } else {
      p <- rbind(prevalence_female %>% 
                   as.data.frame() %>% 
                   filter(bmi_class_c %in% c(bmi_class)) %>% 
                   mutate(Sex = "Females") %>% 
                   group_by(Sex, year_c) %>% 
                   summarise(Freq = sum(Freq)),
                 prevalence_male %>% 
                   as.data.frame() %>% 
                   filter(bmi_class_c %in% c(bmi_class)) %>% 
                   mutate(Sex = "Males")%>% 
                   group_by(Sex, year_c) %>% 
                   summarise(Freq = sum(Freq)),
                 prevalence_all %>% 
                   as.data.frame() %>% 
                   filter(bmi_class_c %in% c(bmi_class)) %>% 
                   mutate(Sex = "Persons") %>% 
                   group_by(Sex, year_c) %>% 
                   summarise(Freq = sum(Freq))) %>% 
        mutate(Year = as.numeric(as.character(year_c))) %>% 
        merge(under16, by = c("Year", "Sex")) %>% 
        mutate(prevalence = under16*Freq) %>% 
        select(Year, Sex, prevalence)
    
    } 
  return(p)
}

get_prediction_data <- function(bmi_class){
  
  if (length(bmi_class) == 1){
    t <- rbind(
      prevalence_all %>% as.data.frame() %>% filter(bmi_class_c == bmi_class) %>% mutate(Sex = "Persons"),
      prevalence_male %>% as.data.frame() %>% filter(bmi_class_c == bmi_class) %>% mutate(Sex = "Males"),
      prevalence_female %>% as.data.frame() %>% filter(bmi_class_c == bmi_class) %>% mutate(Sex = "Females")) %>% 
      rename(Year = year_c) %>% select(Year, Sex, Freq) %>% 
      mutate(Year = as.numeric(as.character(Year)))
  } else {
    t <- rbind(
      prevalence_all %>% as.data.frame() %>% 
        filter(bmi_class_c %in% bmi_class) %>% 
        group_by(year_c) %>% 
        summarise(Freq = sum(Freq)) %>% 
        mutate(Sex = "Persons"),
      prevalence_male %>% 
        as.data.frame() %>% 
        filter(bmi_class_c %in% bmi_class) %>% 
        group_by(year_c) %>% 
        summarise(Freq = sum(Freq)) %>% 
        mutate(Sex = "Males"),
      prevalence_female %>% 
        as.data.frame() %>% 
        filter(bmi_class_c %in% bmi_class) %>% 
        group_by(year_c) %>% 
        summarise(Freq = sum(Freq)) %>% 
        mutate(Sex = "Females")) %>% 
      rename(Year = year_c) %>% 
      select(Year, Sex, Freq) %>% 
      mutate(Year = as.numeric(as.character(Year)))
  }
  out <- merge(t, under16, by = c("Year", "Sex"))
  return(out)
}


# data for prediction
new <- expand.grid(Year = seq(2020,2030, 1), Sex = unique(get_prevalence_children("obese")$Sex)) %>% nest(new = -Sex)


# run regressions for every sex given a bmi class

# oveweight

regressions_children_over <- get_prevalence_children("overweight")  %>% 
  nest(data = -Sex) %>% 
  left_join(new, by = "Sex") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new, ~augment(.x, newdata=.y)))


# extract predictions
predictions_children_over <- regressions_children_over %>% 
  unnest(augmented) %>% 
  select(Sex, Year, .fitted) %>% 
  merge(., under16_proj, by = c("Year", "Sex")) %>% 
  mutate(Freq = .fitted/under16_proj)

# save csv
plyr::rbind.fill(predictions_children_over, get_prediction_data("overweight")) %T>% 
  write_csv(here("outputs", "reports", "overweight_children.csv"))


# obese

regressions_children_obese <- get_prevalence_children("obese")  %>% 
  nest(data = -Sex) %>% 
  left_join(new, by = "Sex") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new, ~augment(.x, newdata=.y)))


# extract predictions
predictions_children_obese <- regressions_children_obese %>% 
  unnest(augmented) %>% 
  select(Sex, Year, .fitted) %>% 
  merge(., under16_proj, by = c("Year", "Sex")) %>% 
  mutate(Freq = .fitted/under16_proj)

# save csv

plyr::rbind.fill(predictions_children_obese, get_prediction_data("obese")) %T>% 
  write_csv(here("outputs", "reports", "obesity_children.csv"))

# morbidly obese

regressions_children_morb_obese <- get_prevalence_children("morbidlyobese")  %>% 
  nest(data = -Sex) %>% 
  left_join(new, by = "Sex") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new, ~augment(.x, newdata=.y)))


# extract predictions
predictions_children_morb_obese <- regressions_children_morb_obese %>% 
  unnest(augmented) %>% 
  select(Sex, Year, .fitted) %>% 
  merge(., under16_proj, by = c("Year", "Sex")) %>% 
  mutate(Freq = .fitted/under16_proj)

# save csv

plyr::rbind.fill(predictions_children_morb_obese, get_prediction_data("morbidlyobese")) %T>% 
  write_csv(here("outputs", "reports", "morb_obesity_children.csv"))

# obese + morbidly obese

regressions_children_morb_obese_and_morb <- get_prevalence_children(c("obese", "morbidlyobese"))  %>% 
  nest(data = -Sex) %>% 
  left_join(new, by = "Sex") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new, ~augment(.x, newdata=.y)))


# extract predictions
predictions_children_morb_obese_and_morb <- regressions_children_morb_obese_and_morb %>% 
  unnest(augmented) %>% 
  select(Sex, Year, .fitted) %>% 
  merge(., under16_proj, by = c("Year", "Sex")) %>% 
  mutate(Freq = .fitted/under16_proj)

# save csv

plyr::rbind.fill(predictions_children_morb_obese_and_morb, get_prediction_data(c("obese","morbidlyobese"))) %T>% 
  write_csv(here("outputs", "reports", "obesity_and_morbidly_children.csv"))

# plots

rbind(prevalence_female %>% 
        as.data.frame() %>% 
        filter(bmi_class_c == "obese") %>% 
        mutate(Sex = "Females"),
      prevalence_male %>% 
        as.data.frame() %>% 
        filter(bmi_class_c == "obese") %>% 
        mutate(Sex = "Males"),
      prevalence_all %>% 
        as.data.frame() %>% 
        filter(bmi_class_c == "obese") %>% 
        mutate(Sex = "Persons")) %>% 
  mutate(Year = as.numeric(as.character(year_c))) %>% 
  merge(under16, by = c("Year", "Sex")) %>% 
  mutate(prevalence = under16*Freq) %>% 
  select(Year, Sex, Freq) %>% 
  rbind(., predictions_children_obese %>% select(Year, Sex, Freq)) %>% 
  mutate(ispred = ifelse(Year>2019,1,0)) %>% 
  ggplot(., aes(x = Year, y = Freq, colour = as.factor(ispred))) +
  facet_grid(Sex ~ .) +
  ylim(0,0.2) +
  geom_point() +
  geom_line() +
  labs(title = "Children Obesity")

