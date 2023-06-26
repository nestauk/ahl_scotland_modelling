rm(list = ls())

library(tidyverse)
library(here)
library(reshape2)
library(survey)
library(scales)
library(gridExtra)
library(magrittr)
library(hrbrthemes)


theme_set(theme_ipsum())

# read 2019 file

df2019 <- read_csv(here("outputs", "data",  "shs_2019_adult_clean.csv")) %>% 
  filter(!is.na(int_wt) & bmi > 0) %>% 
  mutate(bmi_class_c = case_when(bmi < 18.5 ~ "underweight",
                                 (bmi >= 18.5 & bmi < 25) ~ "normal",
                                 (bmi >= 25 & bmi < 30) ~ "overweight",
                                 (bmi >= 30 & bmi < 40) ~ "obese",
                                 bmi >= 40 ~ "morbidly obese")) %>% 
  mutate(sex = ifelse(sex == 2 ,"female", "male")) 

# define years that will be used

years <- c(1995, 1998, 2003, 2008, 2011, 2012, 2013)


# mean weight change

weight_change <- lapply(list.files(here("outputs", "reports"), "weight_change", full.names = T), read_csv)

names(weight_change) <- paste0("wc_", years)

# for each year merge data to get weight target

get_weight_target <- function(year){
  w_change <- weight_change[[paste0("wc_", year)]] %>% 
    dplyr::select(bmi_class_c, female, male) %>% 
    melt(., id.vars = "bmi_class_c")
  
  dat <- merge(df2019, w_change, by.x = c("bmi_class_c", "sex"), by.y = c("bmi_class_c", "variable")) %>% 
    mutate(target = wt*(1 + value/100))
  
  return(dat)
}

weight_target <- lapply(years, get_weight_target)

names(weight_target) <- paste0("wt_", years)

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

all_full <- lapply(years, make_full)

names(all_full) <- paste0("full_", years)

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
                                         years, 
                                         MoreArgs = list(class = "overweight", sex_value = "female"), 
                                         SIMPLIFY = F) ) %>% distinct()

# overweight male
scenario_over_male <- do.call("rbind", mapply(make_avg_ei, 
                                                years, 
                                                MoreArgs = list(class = "overweight", sex_value = "male"), 
                                                SIMPLIFY = F) ) %>% distinct()

# obese female
scenario_obese_female <- do.call("rbind", mapply(make_avg_ei, 
                                          years, 
                                          MoreArgs = list(class = "obese", sex_value = "female"), 
                                          SIMPLIFY = F) ) %>% distinct()

# obese male
scenario_obese_male <- do.call("rbind", mapply(make_avg_ei, 
                                                 years, 
                                                 MoreArgs = list(class = "obese", sex_value = "male"), 
                                                 SIMPLIFY = F) ) %>% distinct()

# morbidly obese female
scenario_morb_female <- do.call("rbind", mapply(make_avg_ei, 
                                         years, 
                                         MoreArgs = list(class = "morbidly obese", sex_value = "female"),
                                         SIMPLIFY = F) ) %>% distinct()

# morbidly obese male
scenario_morb_male <- do.call("rbind", mapply(make_avg_ei, 
                                                years, 
                                                MoreArgs = list(class = "morbidly obese", sex_value = "male"),
                                                SIMPLIFY = F) ) %>% distinct()

# excess female
scenario_excess_female <- do.call("rbind", mapply(make_avg_ei, 
                                           years, 
                                           MoreArgs = list(class = c("overweight","obese", "morbidly obese"), sex_value = "female"), 
                                           SIMPLIFY = F) ) %>% distinct()

# excess male
scenario_excess_male <- do.call("rbind", mapply(make_avg_ei, 
                                                  years, 
                                                  MoreArgs = list(class = c("overweight","obese", "morbidly obese"), sex_value = "male"), 
                                                  SIMPLIFY = F) ) %>% distinct()

# pop female
scenario_pop_female <- do.call("rbind", mapply(make_avg_ei, 
                                                  years, 
                                                  MoreArgs = list(class = c("underweight","normal" , "overweight","obese", "morbidly obese"), sex_value = "female"), 
                                                  SIMPLIFY = F) ) %>% distinct()

# pop male
scenario_pop_male <- do.call("rbind", mapply(make_avg_ei, 
                                                years, 
                                                MoreArgs = list(class = c("underweight","normal", "overweight","obese", "morbidly obese"), sex_value = "male"), 
                                                SIMPLIFY = F) ) %>% distinct()


#############

# read file with percentage change in obesity prevalence for all years considered

year_trend <- read_csv(here("outputs/reports/obesity_change.csv"))


# linear regression to extrapolate beyond the actual reduction in prevalence observed

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

# generate report for all 

svy_2019 <- svydesign(ids = ~df2019$psu,
                      nest = T,
                      data = df2019,
                      strata = df2019$strata,
                      weights = df2019$int_wt)

share <- svytable(~sex, svy_2019) %>% prop.table()


over <- rbind(
  reports[["scenario_over_male"]] %>% 
  mutate(share = share[[2]]),
  reports[["scenario_over_female"]] %>% 
    mutate(share = share[[1]])) %>% 
  group_by(rel_freq) %>% 
  summarise(avg_kcal = sum(diff*share),
            avg_share = sum(pp*share)) %>% 
  mutate(bmi_class = "overweight")


obese <- rbind(
  reports[["scenario_obese_male"]] %>% 
    mutate(share = share[[2]]),
  reports[["scenario_obese_female"]] %>% 
    mutate(share = share[[1]])) %>% 
  group_by(rel_freq) %>% 
  summarise(avg_kcal = sum(diff*share),
            avg_share = sum(pp*share)) %>% 
  mutate(bmi_class = "obese")


morb <- rbind(
  reports[["scenario_morb_male"]] %>% 
    mutate(share = share[[2]]),
  reports[["scenario_morb_female"]] %>% 
    mutate(share = share[[1]])) %>% 
  group_by(rel_freq) %>% 
  summarise(avg_kcal = sum(diff*share),
            avg_share = sum(pp*share)) %>% 
  mutate(bmi_class = "morb")
  

excess <- rbind(
  reports[["scenario_excess_male"]] %>% 
    mutate(share = share[[2]]),
  reports[["scenario_excess_female"]] %>% 
    mutate(share = share[[1]])) %>% 
  group_by(rel_freq) %>% 
  summarise(avg_kcal = sum(diff*share),
            avg_share = sum(pp*share)) %>% 
  mutate(bmi_class = "excess")

pop <- rbind(
  reports[["scenario_pop_male"]] %>% 
    mutate(share = share[[2]]),
  reports[["scenario_pop_female"]] %>% 
    mutate(share = share[[1]])) %>% 
  group_by(rel_freq) %>% 
  summarise(avg_kcal = sum(diff*share),
            avg_share = sum(pp*share)) %>% 
  mutate(bmi_class = "pop")

