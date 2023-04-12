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
                            (year >=1) ~ year +2007))

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


# read population by imd

all_imd <- lapply(seq(10, 21, 1), function(x) read_excel(here("inputs", "data", "simd-21-tab1.xlsx"), sheet = x, range = "A4:CP34"))

stack_imd_children <- function(x){
  x %>% 
    select(- `Total`) %>% 
    melt(., id.vars = c("Decile", "Sex")) %>% 
    mutate(age = parse_number(as.character(variable))) %>% 
    filter(age %in%  c(0,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,15)) %>% 
    group_by(Decile, Sex) %>% 
    summarise(under16 = sum(value))
}

all_imd_stacked <- do.call("rbind", map2(lapply(all_imd, stack_imd_children), seq(2008, 2019, 1), ~cbind(.x, Year = .y))) %>% 
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
                   summarise(sum = sum(under16)),
                 by = c( "Year")) %>% 
  mutate(.Freq = under16/sum)

# data for prediction
new_imd <- expand.grid(Year = seq(2020,2030, 1), imd = seq(1,5,1))

# regression

mod <- lm(.Freq ~ Year*as.factor(imd), dat_imd)

summary(mod)

under16_proj_imd <- cbind(new_imd, .under16 = predict(mod, new_imd)) %>% 
  merge(., under16_proj %>% filter(Sex == "Persons") %>% distinct(Year, under16_proj), by = "Year") %>% 
  mutate(under16_imd = under16_proj*.under16)


# combine datasets to get prevalence given a bmi class by imd

get_prevalence_children_imd <- function(bmi_class){
  
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
    mutate(prevalence = under16*Freq) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    select(Year, Sex, imd, prevalence)
  
  return(p)
}


# data for prediction
new_imd <- expand.grid(Year = seq(2020,2030, 1), imd = unique(get_prevalence_children_imd("obese")$imd)) %>% nest(new_imd = -imd)


# run regressions for every sex given a bmi class

regressions_children_imd <- get_prevalence_children_imd("obese")  %>% 
  nest(data = -imd) %>% 
  left_join(new_imd, by = "imd") %>% 
  mutate(fit = map(data, ~lm(prevalence ~ Year, data = .x)),
         augmented = map2(fit, new_imd, ~augment(.x, newdata=.y)))


# extract predictions
predictions_children_imd <- regressions_children_imd %>% 
  unnest(augmented) %>% 
  select(imd, Year, .fitted) %>% 
  merge(., under16_proj_imd, by = c("Year", "imd")) %>% 
  mutate(Freq = .fitted/under16_imd)

# save csv
predictions_children_imd %T>% write_csv(here("outputs", "reports", "obesity_children_imd.csv"))


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
      predictions_children_imd %>% select(Year, imd, Freq)) %>% 
  mutate(ispred = ifelse(Year>2019,1,0)) %>% 
  ggplot(., aes(x = Year, y = Freq, colour = as.factor(ispred), group = 1)) +
  facet_grid(imd ~ .) +
  ylim(0,0.2) +
  geom_point() +
  geom_line() +
  labs(title = "Children Obesity")
