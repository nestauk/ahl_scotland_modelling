rm(list = ls())

library(Hmisc)
library(reshape2)
library(magrittr)

equating <- function(old_year){
  
  df2019 <- read_csv(here("outputs", "data",  "shs_2019_adult_clean.csv")) %>% 
    filter(!is.na(int_wt) & bmi > 0) %>% 
    mutate(bmi_class_c = case_when(bmi < 18.5 ~ "underweight",
                                   (bmi >= 18.5 & bmi < 25) ~ "normal",
                                   (bmi >= 25 & bmi < 30) ~ "overweight",
                                   (bmi >= 30 & bmi < 40) ~ "obese",
                                   bmi >= 40 ~ "morbidly obese"))
  
  dfold <- read_csv(here("outputs", "data", paste0("shs_", old_year, "_adult_clean.csv"))) %>% 
    filter(!is.na(int_wt) & bmi > 0) %>% 
    mutate(bmi_class_c = case_when(bmi < 18.5 ~ "underweight",
                                   (bmi >= 18.5 & bmi < 25) ~ "normal",
                                   (bmi >= 25 & bmi < 30) ~ "overweight",
                                   (bmi >= 30 & bmi < 40) ~ "obese",
                                   bmi >= 40 ~ "morbidly obese"))
  
  svy_2019 <- svydesign(ids=~df2019$psu, 
                        nest = T,
                        data=df2019,
                        strata = df2019$strata,
                        weights=df2019$int_wt)
  
  svy_old <- svydesign(ids=~dfold$psu, 
                       nest = T,
                       data=dfold,
                       strata = dfold$strata,
                       weights=dfold$int_wt)
  
  prop_a <- prop.table(svytable(~bmi_class_c,svy_2019)) %>% as.data.frame()
  
  morb <- prop_a[which(prop_a$bmi_class_c == "morbidly obese"),]$Freq
  obese <- prop_a[which(prop_a$bmi_class_c == "obese"),]$Freq
  over <- prop_a[which(prop_a$bmi_class_c == "overweight"),]$Freq
  normal <- prop_a[which(prop_a$bmi_class_c == "normal"),]$Freq
  under <- prop_a[which(prop_a$bmi_class_c == "underweight"),]$Freq
  
  cut_1 <- svyquantile(~bmi, svy_old, under)$bmi[1,1]
  cut_2 <- svyquantile(~bmi, svy_old, 1 - morb - obese - over)$bmi[1,1]
  cut_3 <- svyquantile(~bmi, svy_old, 1 - morb - obese)$bmi[1,1]
  cut_4 <- svyquantile(~bmi, svy_old, 1 - morb)$bmi[1,1]
  
  dfold_cutoff <- dfold %>% 
    mutate(bmi_class_c = case_when(bmi <= cut_1 ~ "underweight",
                                   bmi > cut_1 & bmi < cut_2 ~ "normal",
                                   bmi >= cut_2 & bmi < cut_3 ~ "overweight",
                                   bmi >= cut_3 & bmi < cut_4 ~ "obese",
                                   bmi >= cut_4 ~ "morbidly obese",
                                   TRUE ~ "NA"))
  
  all <- rbind(df2019, dfold_cutoff)
  
  weight_change_mean <- all %>% 
    group_by(bmi_class_c , year, sex) %>% 
    summarise(weight_m = wtd.mean(wt, weight = int_wt)) %>% 
    dcast(., bmi_class_c  ~ year + sex, value.var = "weight_m") %>% 
    mutate(female = 100*(.[[3]]/.[[5]]-1),
           male = 100*(.[[2]]/.[[4]]-1))
  
  names(weight_change_mean) <- c("bmi_class_c", paste0("male_", old_year), paste0("female_", old_year), "male_2019", "female_2019", "female", "male")
  
  return(weight_change_mean)
  
}

for (x in c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) {
  equating(x) %T>% write_csv(here(paste0("outputs/reports/weight_change_table_mean_", x,".csv")))
}
