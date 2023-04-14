require("bw")

source(here("utils/find_EI.R"))


print("This script takes about 30 minutes to run.")


df2019 <- read_csv(here("outputs", "data",  "shs_2019_adult_clean.csv")) %>% 
  filter(!is.na(int_wt) & bmi > 0) %>% 
  mutate(bmi_class_c = case_when(bmi < 18.5 ~ "underweight",
                                 (bmi >= 18.5 & bmi < 25) ~ "normal",
                                 (bmi >= 25 & bmi < 30) ~ "overweight",
                                 (bmi >= 30 & bmi < 40) ~ "obese",
                                 bmi >= 40 ~ "morbidly obese")) %>% 
  mutate(sex = ifelse(sex == 2 ,"female", "male")) 


findEI_by_year <- function(year){
  
  # mean weight change
  w_change <- read.csv(here(paste0("outputs/reports/weight_change_table_mean_",year ,".csv"))) %>% 
    dplyr::select(bmi_class_c, female, male) %>% 
    melt(., id.vars = "bmi_class_c")
    
  # merge data to obtain weight target
  
  dat <- merge(df2019, w_change, by.x = c("bmi_class_c", "sex"), by.y = c("bmi_class_c", "variable")) %>% 
      mutate(target = wt*(1 + value/100))
    
    # split data by bmi class
    dat_split <- split(dat, dat$bmi_class_c)
    

    print("Running obese sample")

    list(id = dat_split$obese$id,
                       bw = dat_split$obese$wt,
                       ht = dat_split$obese$ht/100,
                       age = dat_split$obese$age,
                       sex = dat_split$obese$sex,
                       weight_goal = dat_split$obese$target,
                       days = 365*3,
                       ei_limit = 1000,
                       pal = 1.6) %>% 
      pmap_dfr(., find_EI) %T>% write_csv(here(paste0("outputs/data/obese_", year, ".csv")) )
    

    print("Running overweight sample")

    list(id = dat_split$overweight$id,
                  bw = dat_split$overweight$wt,
                  ht = dat_split$overweight$ht/100,
                  age = dat_split$overweight$age,
                  sex = dat_split$overweight$sex,
                  weight_goal = dat_split$overweight$target,
                  days = 365*3,
                  ei_limit = 1000,
                  pal = 1.6) %>% 
      pmap_dfr(., find_EI) %T>% write_csv(here(paste0("outputs/data/overweight_", year, ".csv"))) 

  
   print("Running morbidly obese sample")

   list(id = dat_split$`morbidly obese`$id,
                 bw = dat_split$`morbidly obese`$wt,
                 ht = dat_split$`morbidly obese`$ht/100,
                 age = dat_split$`morbidly obese`$age,
                 sex = dat_split$`morbidly obese`$sex,
                 weight_goal = dat_split$`morbidly obese`$target,
                 days = 365*3,
                 ei_limit = 1000,
                 pal = 1.6) %>% 
      pmap_dfr(., find_EI)  %T>% write_csv(here(paste0("outputs/data/morb_", year, ".csv"))) 
  
}

for (x in c(1995, 1998, 2003, 2008, 2011, 2012, 2013)) {
  print(paste0("Running ", x))
  findEI_by_year(x)
}
