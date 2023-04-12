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
    select(id, year, psu, strata, int_wt, bmi, ht, wt, age, sex, imd)
  return(df)
}

df <- do.call("rbind" , lapply(list.files(here("outputs", "data"), "adult", full.names = T), get_select))

# drop 2 obeservations in 2011 that had missing weights and filter to valid bmi

df_clean <- df %>% 
  filter(!is.na(int_wt) & bmi > 0) %>% 
  filter(year > 0) %>% 
  mutate(bmi_class_c = case_when(bmi < 18.5 ~ "underweight",
                                 (bmi >= 18.5 & bmi < 25) ~ "normal",
                                 (bmi >= 25 & bmi < 30) ~ "overweight",
                                 (bmi >= 30 & bmi < 40) ~ "obese",
                                 bmi >= 40 ~ "morbidlyobese")) %>% 
  mutate(year_c = case_when((year <= 0) ~ year + 2003,
                            (year >=1) ~ year +2007))

# declare survey design
svy.dat <- svydesign(ids=~df_clean$psu, 
                     nest = T,
                     data=df_clean,
                     strata = df_clean$strata,
                     weights=df_clean$int_wt)


# bmi class prevalence by imd

prevalence_all_imd <- svytable(~interaction(year_c, bmi_class_c, imd),svy.dat) %>% 
  as.data.frame() %>% 
  separate(`interaction.year_c..bmi_class_c..imd.`,  c("Year", "bmi_class_c", "imd"))

# read in population projections
proj <- read_csv(here("inputs", "data", "pop-proj-2020-profile-custom.csv")) %>% 
  melt(., id.vars = c("Variant", "Year", "Sex")) %>% 
  mutate(age = gsub("Age_", "", variable))

# adult data
over16_proj <- rbind(proj %>% 
                       filter(! age %in%  c(0,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,15)) %>% 
                       group_by(Year, Sex) %>% 
                       summarise(over16_proj = sum(value)),
                     proj %>% 
                       filter(! age %in%  c(0,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,15)) %>% 
                       group_by(Year) %>% 
                       summarise(over16_proj = sum(value)) %>% 
                       mutate(Sex = "Persons"))


# read population by imd

all_imd <- lapply(seq(10, 21, 1), function(x) read_excel(here("inputs", "data", "simd-21-tab1.xlsx"), sheet = x, range = "A4:CP34"))

stack_imd_adults <- function(x){
  x %>% 
    select(- `Total`) %>% 
    melt(., id.vars = c("Decile", "Sex")) %>% 
    mutate(age = parse_number(as.character(variable))) %>% 
    filter(! age %in%  c(0,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,15)) %>% 
    group_by(Decile, Sex) %>% 
    summarise(over16 = sum(value))
}

all_imd_stacked <- do.call("rbind", map2(lapply(all_imd, stack_imd_adults), seq(2008, 2019, 1), ~cbind(.x, Year = .y))) %>% 
  mutate(imd = case_when(Decile %in% c(1,2) ~ 1,
                         Decile %in% c(3,4) ~ 2,
                         Decile %in% c(5,6) ~ 3,
                         Decile %in% c(7,8) ~ 4,
                         Decile %in% c(9,10) ~ 5))

# projection of trends in IMD
# to be confirmed when NAOMI replies

dat_imd <- merge(all_imd_stacked %>% 
                   filter(Sex == "Persons"),
                 all_imd_stacked %>% 
                   filter(Sex == "Persons") %>% 
                   group_by(Year) %>% 
                   summarise(sum = sum(over16)),
                 by = c( "Year")) %>% 
  mutate(.Freq = over16/sum)

# data for prediction
new_imd <- expand.grid(Year = seq(2020,2030, 1), imd = seq(1,5,1))

# regression

mod <- lm(.Freq ~ Year*as.factor(imd), dat_imd)

summary(mod)

over16_proj_imd <- cbind(new_imd, .over16 = predict(mod, new_imd)) %>% 
  merge(., over16_proj %>% filter(Sex == "Persons") %>% distinct(Year, over16_proj), by = "Year") %>% 
  mutate(over16_imd = over16_proj*.over16)


# combine datasets to get prevalence given a bmi class by imd

get_prevalence_adult_imd <- function(bmi_class){
  
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
    merge(all_imd_stacked, by.x = c("Year", "imd", "Sex"), by.y = c("Year", "Decile", "Sex")) %>% 
    mutate(prevalence = over16*Freq) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    select(Year, Sex, imd, prevalence)
  
  return(p)
}


# data for prediction
new_imd <- expand.grid(Year = seq(2020,2030, 1), imd = unique(get_prevalence_adult_imd("obese")$imd)) %>% nest(new_imd = -imd)


# run regressions for every sex given a bmi class

regressions_adult_imd <- get_prevalence_adult_imd("obese")  %>% 
  nest(data = -imd) %>% 
  left_join(new_imd, by = "imd") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new_imd, ~augment(.x, newdata=.y)))


# extract predictions
predictions_adult_imd <- regressions_adult_imd %>% 
  unnest(augmented) %>% 
  select(imd, Year, .fitted) %>% 
  merge(., over16_proj_imd, by = c("Year", "imd")) %>% 
  mutate(Freq = .fitted/over16_imd)

# save csv
predictions_adult_imd %T>% write_csv(here("outputs", "reports", "obesity_adult_imd.csv"))


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
  predictions_adult_imd %>% select(Year, imd, Freq)) %>% 
  mutate(ispred = ifelse(Year>2019,1,0)) %>% 
  ggplot(., aes(x = Year, y = Freq, colour = as.factor(ispred), group = 1)) +
  facet_grid(imd ~ .) +
  ylim(0,0.5) +
  geom_point() +
  geom_line() +
  labs(title = "Adult Obesity")
