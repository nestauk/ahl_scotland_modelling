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
    select(id, year, psu, strata, cint_wt, bmi, ht, wt, age, sex, imd, child_bmi_class)
  return(df)
}

df <- do.call("rbind" , lapply(list.files(here("outputs", "data"), "child", full.names = T), get_select))

# drop obeservations with invalid BMI

df_clean <- df %>% 
  filter(!is.na(cint_wt) & bmi > 0 & child_bmi_class > 0) %>% 
  filter(year > 0) %>% 
  mutate(bmi_class_c = case_when(child_bmi_class == 1 ~ "underweight",
                                 (child_bmi_class == 2) ~ "normal",
                                 (child_bmi_class == 3) ~ "overweight",
                                 (child_bmi_class == 4) ~ "obese",
                                 (child_bmi_class == 5) ~ "morbidlyobese",
                                 TRUE ~ "unknown")) %>% 
  mutate(year_c = case_when((year <= 0) ~ year + 2003,
                            (year >=1) ~ year +2007)) %>% 
  filter(age >=2 )

# declare survey design
svy.dat <- svydesign(ids=~df_clean$psu, 
                     nest = T,
                     data=df_clean,
                     strata = df_clean$strata,
                     weights=df_clean$cint_wt)


# bmi class prevalence by imd

prevalence_all_imd <- svytable(~interaction(year_c, bmi_class_c, imd),svy.dat) %>% 
  as.data.frame() %>% 
  separate(`interaction.year_c..bmi_class_c..imd.`,  c("Year", "bmi_class_c", "imd"))

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

# population data (mid year population estimates)

pop <- read_excel(here("inputs", "data", "mid-year-pop-est-21-time-series-data.xlsx"), sheet = "Table_6", range = "A6:CP129") 

# adult data
under16 <- pop %>% 
  select(- `All Ages`) %>% 
  melt(., id.vars = c("Year", "Sex")) %>% 
  filter( variable %in%  c(0,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,15)) %>% 
  group_by(Year, Sex) %>% 
  summarise(under16 = sum(value))

# all population

all_children <- rbind(under16 %>% rename(.under16 = under16) %>% filter(Year < 2020),
                   under16_proj %>% rename(.under16 = under16_proj)) %>% 
  filter(Sex == "Persons")


# read population by imd

all_imd <- lapply(seq(10, 21, 1), function(x) read_excel(here("inputs", "data", "simd-21-tab1.xlsx"), sheet = x, range = "A4:CP34"))

stack_imd_children <- function(x){
  x %>% 
    select(- `Total`) %>% 
    melt(., id.vars = c("Decile", "Sex")) %>% 
    mutate(age = parse_number(as.character(variable))) %>% 
    filter(age %in%  c(0,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,15)) %>% 
    mutate(imd = case_when(Decile %in% c(1,2) ~ 1,
                           Decile %in% c(3,4) ~ 2,
                           Decile %in% c(5,6) ~ 3,
                           Decile %in% c(7,8) ~ 4,
                           Decile %in% c(9,10) ~ 5)) %>%
    group_by(imd, Sex) %>% 
    summarise(under16 = sum(value))
}

all_imd_stacked <- do.call("rbind", map2(lapply(all_imd, stack_imd_children), seq(2008, 2019, 1), ~cbind(.x, Year = .y)))

imd_2019 <- stack_imd_children(read_excel(here("inputs", "data", "simd-21-tab1.xlsx"), sheet = 21, range = "A4:CP34")) %>% 
  rename(y2019_imd = under16)


# projection of trends in IMD

year_trend <- all_children %>% 
  ungroup() %>% 
  left_join(all_children %>% 
              ungroup() %>% 
              filter(Year == 2019) %>% 
              rename(y2019 = .under16) %>% 
              select(Sex, y2019), by = "Sex") %>% 
  mutate(change = (.under16/y2019 - 1)) %>% 
  filter(Year > 2019)



# data for prediction
new_imd <- expand.grid(Year = seq(2020,2030, 1), imd = seq(1,5,1))

# regression

under16_proj_imd <- expand.grid(Year = seq(2020,2030, 1), imd = seq(1,5,1)) %>% 
  merge(year_trend, by = "Year") %>% 
  merge(imd_2019, by = c("imd", "Sex")) %>% 
  mutate(under16_imd = (1 + change)*y2019_imd) %>% 
  select(imd, Sex, Year, under16_imd)

# combine datasets to get prevalence given a bmi class by imd

get_prevalence_children_imd <- function(bmi_class){
  
  if (length(bmi_class) == 1){
    
    p <- merge(prevalence_all_imd %>% 
                 group_by(Year, imd, bmi_class_c) %>% 
                 summarise(.Freq = sum(Freq)),
               prevalence_all_imd %>% 
                 group_by(Year, imd) %>% 
                 summarise(.Freq_all = sum(Freq)),
               by = c("Year", "imd")) %>% 
      mutate(Freq = .Freq/.Freq_all) %>% 
      mutate(Sex = "Persons") %>% 
      filter(bmi_class_c == bmi_class)  %>% 
      merge(all_imd_stacked, by = c("Year", "imd", "Sex")) %>% 
      mutate(prevalence = under16*Freq) %>% 
      mutate(Year = as.numeric(Year)) %>% 
      select(Year, Sex, imd, prevalence)}
  else {
    p <- merge(prevalence_all_imd %>% 
                 group_by(Year, imd, bmi_class_c) %>% 
                 summarise(.Freq = sum(Freq)),
               prevalence_all_imd %>% 
                 group_by(Year, imd) %>% 
                 summarise(.Freq_all = sum(Freq)),
               by = c("Year", "imd")) %>% 
      mutate(Freq = .Freq/.Freq_all) %>% 
      mutate(Sex = "Persons") %>% 
      filter(bmi_class_c %in% bmi_class)  %>% 
      group_by(Year, imd, Sex) %>% 
      summarise(Freq = sum(Freq)) %>% 
      merge(all_imd_stacked, by = c("Year", "imd", "Sex")) %>% 
      mutate(prevalence = under16*Freq) %>% 
      mutate(Year = as.numeric(as.character(Year))) %>% 
      select(Year, Sex, imd, prevalence)
    
  }
  
  return(p)
}

get_prediction_data <- function(bmi_class){
  if (length(bmi_class) == 1){
    t <- merge(prevalence_all_imd %>% 
                 group_by(Year, imd, bmi_class_c) %>% 
                 summarise(.Freq = sum(Freq)),
               prevalence_all_imd %>% 
                 group_by(Year, imd) %>% 
                 summarise(.Freq_all = sum(Freq)),
               by = c("Year", "imd")) %>% 
      mutate(Freq = .Freq/.Freq_all) %>% 
      filter(bmi_class_c == bmi_class) %>% 
      select(imd, Year, Freq)}
  else {
    t <-  merge(prevalence_all_imd %>% 
                  group_by(Year, imd, bmi_class_c) %>% 
                  summarise(.Freq = sum(Freq)),
                prevalence_all_imd %>% 
                  group_by(Year, imd) %>% 
                  summarise(.Freq_all = sum(Freq)),
                by = c("Year", "imd")) %>% 
      mutate(Freq = .Freq/.Freq_all) %>% 
      filter(bmi_class_c %in% bmi_class) %>% 
      group_by(Year, imd) %>% 
      summarise(Freq = sum(Freq))
  }
  
  out <- merge(t, all_imd_stacked %>% filter(Sex == "Persons"), by = c("imd", "Year"))
  return(out)
}



# data for prediction
new_imd <- expand.grid(Year = seq(2020,2030, 1), imd = unique(get_prevalence_children_imd("obese")$imd)) %>% nest(new_imd = -imd)


# run regressions for every sex given a bmi class

# overweight


regressions_children_imd_over <- get_prevalence_children_imd("overweight")  %>% 
  nest(data = -imd) %>% 
  left_join(new_imd, by = "imd") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new_imd, ~augment(.x, newdata=.y)))


# extract predictions
predictions_children_imd_over <- regressions_children_imd_over %>% 
  unnest(augmented) %>% 
  select(imd, Year, .fitted) %>% 
  merge(., under16_proj_imd, by = c("Year", "imd")) %>% 
  mutate(Freq = .fitted/under16_imd)

# save csv
plyr::rbind.fill(predictions_children_imd_over, get_prediction_data("overweight")) %T>% 
  write_csv(here("outputs", "reports", "overweight_children_imd.csv"))

# obese

regressions_children_imd_obese <- get_prevalence_children_imd("obese")  %>% 
  nest(data = -imd) %>% 
  left_join(new_imd, by = "imd") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new_imd, ~augment(.x, newdata=.y)))


# extract predictions
predictions_children_imd_obese <- regressions_children_imd_obese %>% 
  unnest(augmented) %>% 
  select(imd, Year, .fitted) %>% 
  merge(., under16_proj_imd, by = c("Year", "imd")) %>% 
  mutate(Freq = .fitted/under16_imd)

# save csv

plyr::rbind.fill(predictions_children_imd_obese, get_prediction_data("obese")) %T>% 
  write_csv(here("outputs", "reports", "obesity_children_imd.csv"))


# morbidly obese

regressions_children_imd_morb_obese <- get_prevalence_children_imd("morbidlyobese")  %>% 
  nest(data = -imd) %>% 
  left_join(new_imd, by = "imd") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new_imd, ~augment(.x, newdata=.y)))


# extract predictions
predictions_children_imd_morb_obese <- regressions_children_imd_morb_obese %>% 
  unnest(augmented) %>% 
  select(imd, Year, .fitted) %>% 
  merge(., under16_proj_imd, by = c("Year", "imd")) %>% 
  mutate(Freq = .fitted/under16_imd)

# save csv
plyr::rbind.fill(predictions_children_imd_morb_obese, get_prediction_data("morbidlyobese")) %T>% 
  write_csv(here("outputs", "reports", "morb_obesity_children_imd.csv"))

# morbidly obese + obese

regressions_children_imd_obese_and_morb <- get_prevalence_children_imd(c("obese", "morbidlyobese"))  %>% 
  nest(data = -imd) %>% 
  left_join(new_imd, by = "imd") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new_imd, ~augment(.x, newdata=.y)))


# extract predictions
predictions_children_imd_obese_and_morb <- regressions_children_imd_obese_and_morb %>% 
  unnest(augmented) %>% 
  select(imd, Year, .fitted) %>% 
  merge(., under16_proj_imd, by = c("Year", "imd")) %>% 
  mutate(Freq = .fitted/under16_imd)

# save csv
plyr::rbind.fill(predictions_children_imd_obese_and_morb, get_prediction_data(c("obese", "morbidlyobese"))) %T>% 
  write_csv(here("outputs", "reports", "obesity_and_morbidly_children_imd.csv"))


# plots

rbind(merge(prevalence_all_imd %>% 
              group_by(Year, imd, bmi_class_c) %>% 
              summarise(.Freq = sum(Freq)),
            prevalence_all_imd %>% 
              group_by(Year, imd) %>% 
              summarise(.Freq_all = sum(Freq)),
            by = c("Year", "imd")) %>% 
        mutate(Freq = .Freq/.Freq_all) %>% 
        filter(bmi_class_c == "obese") %>% 
        select(Year, imd, Freq),
      predictions_children_imd_obese %>% select(Year, imd, Freq)) %>% 
  mutate(ispred = ifelse(Year>2019,1,0)) %>% 
  ggplot(., aes(x = Year, y = Freq, colour = as.factor(ispred), group = 1)) +
  facet_grid(imd ~ .) +
  ylim(0,0.5) +
  geom_point() +
  geom_line() +
  labs(title = "Children Obesity")
