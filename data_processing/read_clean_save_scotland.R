rm(list = ls())

library(tidyverse)
library(here)

# 2019 data

df2019 <- read.table(here("inputs", "data", "shes19i_eul.tab"), sep = "\t", header = T) %>% select(CPSerialA, SYear,PSU, Strata, int19wt, cint19wt, bmival, htval, wtval, BMIvg5, CBMIg5_new, age, Sex, SIMD20_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2019) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2019_adult <- df2019 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2019_child <- df2019 %>% filter(age <16)

write_csv(df2019_adult, here("outputs", "data",  "shs_2019_adult_clean.csv"))

write_csv(df2019_child, here("outputs", "data",  "shs_2019_child_clean.csv"))

# 2018 data

df2018 <- read.table(here("inputs", "data", "shes_18i_eul_v3.tab"), sep = "\t", header = T) %>% select(CPSerialA, SYear,PSU, Strata, int18wt, cint18wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD16_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2018) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2018_adult <- df2018 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2018_child <- df2018 %>% filter(age <16)

write_csv(df2018_adult, here("outputs", "data",  "shs_2018_adult_clean.csv"))

write_csv(df2018_child, here("outputs", "data",  "shs_2018_child_clean.csv"))

# 2017 data

df2017 <- read.table(here("inputs", "data", "shes_17i_archive_v1.tab"), sep = "\t", header = T) %>% select(CPSerialA, SYear,PSU, Strata, int17wt, cint17wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD16_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2017) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2017_adult <- df2017 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2017_child <- df2017 %>% filter(age <16)

write_csv(df2017_adult, here("outputs", "data",  "shs_2017_adult_clean.csv"))

write_csv(df2017_child, here("outputs", "data",  "shs_2017_child_clean.csv"))

# 2016 data

df2016 <- read.table(here("inputs", "data", "shes16i_archive_v1.tab"), sep = "\t", header = T) %>%  select(cpserialA, SYear,PSU, Strata, int16wt, cint16wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD16_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2016) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2016_adult <- df2016 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2016_child <- df2016 %>% filter(age <16)

write_csv(df2016_adult, here("outputs", "data",  "shs_2016_adult_clean.csv"))

write_csv(df2016_child, here("outputs", "data",  "shs_2016_child_clean.csv"))

# 2015 data

df2015 <- read.table(here("inputs", "data", "shes15i_archive_v1.tab"), sep = "\t", header = T)  %>% select(cpserialA, SYear,psu, STRATA, int15wt, cint15wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2015) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2015_adult <- df2015 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2015_child <- df2015 %>% filter(age <16)

write_csv(df2015_adult, here("outputs", "data",  "shs_2015_adult_clean.csv"))

write_csv(df2015_child, here("outputs", "data",  "shs_2015_child_clean.csv"))

# 2014 data

df2014 <- read.table(here("inputs", "data", "shes14i_archive_v6.tab"), sep = "\t", header = T)  %>% select(cpserialA, SYear,psu, Strata, int14wt, cint14wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2014) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2014_adult <- df2014 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2014_child <- df2014 %>% filter(age <16)

write_csv(df2014_adult, here("outputs", "data",  "shs_2014_adult_clean.csv"))

write_csv(df2014_child, here("outputs", "data",  "shs_2014_child_clean.csv"))

# 2013 data

df2013 <- read.table(here("inputs", "data", "shes13i_archive_v6.tab"), sep = "\t", header = T)  %>% select(CpserialA, SYear,psu, STRATA, int13wt, cint13wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic12, totinc, eqv5_15, hedqul08)

names(df2013) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2013_adult <- df2013 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2013_child <- df2013 %>% filter(age <16)

write_csv(df2013_adult, here("outputs", "data",  "shs_2013_adult_clean.csv"))

write_csv(df2013_child, here("outputs", "data",  "shs_2013_child_clean.csv"))

# 2012 data
# there are issue with the bmival variable in this year where some individuals have valid height and weight but no bmi, so we need to recalculate it

df2012 <- read.table(here("inputs", "data", "shes12i_archive_v5.tab"), sep = "\t", header = T)  %>% select(cpserialA, SYear,psu, Strata, int12wt, cint12wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic12, totinc, eqv5_15, hedqul08)

names(df2012) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2012$bmi <- case_when( (df2012$wt > 0 & df2012$ht>0) ~ df2012$wt/df2012$ht/df2012$ht*10000,
                          TRUE ~ -1)

df2012_adult <- df2012 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2012_child <- df2012 %>% filter(age <16)

write_csv(df2012_adult, here("outputs", "data",  "shs_2012_adult_clean.csv"))

write_csv(df2012_child, here("outputs", "data",  "shs_2012_child_clean.csv"))

# 2011 data

# no survey year
df2011 <- read.table(here("inputs", "data", "shes11i_v7.tab"), sep = "\t", header = T)  %>% select(pserialA,psu, strata, int11wt, cint11wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, Ethnic09, totinc, eqv5_15, hedqul08)

df2011$year <- 4

names(df2011) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

df2011_adult <- df2011 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2011_child <- df2011 %>% filter(age <16)

write_csv(df2011_adult, here("outputs", "data",  "shs_2011_adult_clean.csv"))

write_csv(df2011_child, here("outputs", "data",  "shs_2011_child_clean.csv"))

# 2010 data

df2010 <- read.table(here("inputs", "data", "shes10i_v5.tab"), sep = "\t", header = T)  %>% select(pserialA,psu, strata, int10wt, cint10wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, ethgroup, totinc, eqv5_15, hedqul08)

df2010$year <- 3

names(df2010) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

df2010_adult <- df2010 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2010_child <- df2010 %>% filter(age <16)

write_csv(df2010_adult, here("outputs", "data",  "shs_2010_adult_clean.csv"))

write_csv(df2010_child, here("outputs", "data",  "shs_2010_child_clean.csv"))

# 2009 data

df2009 <- read.table(here("inputs", "data", "shes09i_v6.tab"), sep = "\t", header = T)  %>% select(pserialA,psu, strata, int09wt, cint09wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, Ethnic09, totinc, eqv5_15, hedqul08)

df2009$year <- 2

names(df2009) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

df2009_adult <- df2009 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2009_child <- df2009 %>% filter(age <16)

write_csv(df2009_adult, here("outputs", "data",  "shs_2009_adult_clean.csv"))

write_csv(df2009_child, here("outputs", "data",  "shs_2009_child_clean.csv"))

# 2008 data

df2008 <- read.table(here("inputs", "data", "shes08i_v12.tab"), sep = "\t", header = T)  %>% select(pserialA,psu, strata, int08wt, cint08wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, EthnicI, totinc, eqv5_15, hedqul08)

df2008$year <- 1

names(df2008) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

df2008_adult <- df2008 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2008_child <- df2008 %>% filter(age <16)

write_csv(df2008_adult, here("outputs", "data",  "shs_2008_adult_clean.csv"))

write_csv(df2008_child, here("outputs", "data",  "shs_2008_child_clean.csv"))

# 2003 data

# no children BMI
df2003 <- read.table(here("inputs", "data", "shs03i_revised.tab"), sep = "\t", header = T)  %>% select(PSERIAL,psu, strata, int_wt, cint_wt, bmival, htval, wtval, bmivg5, age, Sex, simd5, EthnicI, totinc, eqv5, hedqual)

df2003$year <- 0

df2003$child_bmi_class <- -2

names(df2003) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year", "child_bmi_class")

df2003_adult <- df2003 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df2003_child <- df2003 %>% filter(age <16)

write_csv(df2003_adult, here("outputs", "data",  "shs_2003_adult_clean.csv"))

write_csv(df2003_child, here("outputs", "data",  "shs_2003_child_clean.csv"))



# no split between children and adult weight, bmi grouped in 6 categories, carstair index instead of multiple deprivation, no income, no education

df1998 <- read.table(here("inputs", "data", "shs98i.tab"), sep = "\t", header = T)  %>% 
  select(archsn,psu, region,  bmival, htval, wtval, bmivg6, age, sex, carstg5, ethnic)

df1998$year <- -5

df1998$int_wt <- 1

df1998$cint_wt <- 1

df1998$income <- NA

df1998$eqv_income <- NA

df1998$educ <- NA

df1998$child_bmi_class <- NA

names(df1998) <- c("id", "psu", "strata", "bmi", "ht", "wt", "bmi_class", "age", "sex", "imd", "ethnic", "year", "int_wt", "cint_wt", "income", "eqv_income", "educ", "child_bmi_class")

df1998_adult <- df1998 %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df1998_child <- df1998 %>% filter(age <16)

write_csv(df1998_adult, here("outputs", "data",  "shs_1998_adult_clean.csv"))

write_csv(df1998_child, here("outputs", "data",  "shs_1998_child_clean.csv"))

# no derived weight and height variables
df1995 <- read.table(here("inputs", "data", "shs95i.tab"), sep = "\t", header = T)  %>% 
  select(serialx,psu, region, bmiok, bmi, htok, wtok, height, weight, bmiag1, respage, respsex, cargp5, ethnic)

df1995$htval <- ifelse(df1995$htok == 1, df1995$height, -1)

df1995$wtval <- ifelse(df1995$htok == 1, df1995$weight, -1)

df1995$bmival <- ifelse(df1995$bmiok == 1, df1995$bmi, -1)

df1995_clean <- df1995 %>% select(serialx, psu, region, bmival, htval, wtval, bmiag1, respage, respsex, cargp5, ethnic)

names(df1995_clean) <- c("id", "psu", "strata", "bmi", "ht", "wt", "bmi_class", "age", "sex", "imd", "ethnic")

df1995_clean$year <- -8

df1995_clean$int_wt <- 1

df1995_clean$cint_wt <- 1

df1995_clean$income <- NA

df1995_clean$eqv_income <- NA

df1995_clean$educ <- NA

df1995_clean$child_bmi_class <- NA

df1995_adult <- df1995_clean %>% 
  filter(age >=16) %>% 
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * wt) + (6.25 * ht) - (5 * age) + 5),
                         TRUE ~ ((10 * wt) + (6.25 * ht) - (5 * age) - 161)))

df1995_child <- df1995_clean %>% filter(age <16)

write_csv(df1995_adult, here("outputs", "data",  "shs_1995_adult_clean.csv"))

write_csv(df1995_child, here("outputs", "data",  "shs_1995_child_clean.csv"))
