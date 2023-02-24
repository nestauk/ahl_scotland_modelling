library(tidyverse)
library(here)
library(survey)

# 2019 data

df2019 <- read.table(here("inputs", "data", "shes19i_eul.tab"), sep = "\t", header = T) %>% select(CPSerialA, SYear,PSU, Strata, int19wt, cint19wt, bmival, htval, wtval, BMIvg5, CBMIg5_new, age, Sex, SIMD20_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2019) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2019_adult <- df2019 %>% filter(age >=16)

df2019_child <- df2019 %>% filter(age <16)

write_csv(df2019_adult, here("outputs", "data", "shs", "shs_2019_adult_clean.csv"))

write_csv(df2019_child, here("outputs", "data", "shs", "shs_2019_child_clean.csv"))

# 2018 data

df2018 <- read.table(here("inputs", "data", "shes_18i_eul_v3.tab"), sep = "\t", header = T) %>% select(CPSerialA, SYear,PSU, Strata, int18wt, cint18wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD16_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2018) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2018_adult <- df2018 %>% filter(age >=16)

df2018_child <- df2018 %>% filter(age <16)

write_csv(df2018_adult, here("outputs", "data", "shs", "shs_2018_adult_clean.csv"))

write_csv(df2018_child, here("outputs", "data", "shs", "shs_2018_child_clean.csv"))

# 2017 data

df2017 <- read.table(here("inputs", "data", "shes_17i_archive_v1.tab"), sep = "\t", header = T) %>% select(CPSerialA, SYear,PSU, Strata, int17wt, cint17wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD16_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2017) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2017_adult <- df2017 %>% filter(age >=16)

df2017_child <- df2017 %>% filter(age <16)

write_csv(df2017_adult, here("outputs", "data", "shs", "shs_2017_adult_clean.csv"))

write_csv(df2017_child, here("outputs", "data", "shs", "shs_2017_child_clean.csv"))

# 2016 data

df2016 <- read.table(here("inputs", "data", "shes16i_archive_v1.tab"), sep = "\t", header = T) %>%  select(cpserialA, SYear,PSU, Strata, int16wt, cint16wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD16_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2016) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2016_adult <- df2016 %>% filter(age >=16)

df2016_child <- df2016 %>% filter(age <16)

write_csv(df2016_adult, here("outputs", "data", "shs", "shs_2016_adult_clean.csv"))

write_csv(df2016_child, here("outputs", "data", "shs", "shs_2016_child_clean.csv"))

# 2015 data

df2015 <- read.table(here("inputs", "data", "shes15i_archive_v1.tab"), sep = "\t", header = T)  %>% select(cpserialA, SYear,psu, STRATA, int15wt, cint15wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2015) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2015_adult <- df2015 %>% filter(age >=16)

df2015_child <- df2015 %>% filter(age <16)

write_csv(df2015_adult, here("outputs", "data", "shs", "shs_2015_adult_clean.csv"))

write_csv(df2015_child, here("outputs", "data", "shs", "shs_2015_child_clean.csv"))

# 2014 data

df2014 <- read.table(here("inputs", "data", "shes14i_archive_v6.tab"), sep = "\t", header = T)  %>% select(cpserialA, SYear,psu, Strata, int14wt, cint14wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2014) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2014_adult <- df2014 %>% filter(age >=16)

df2014_child <- df2014 %>% filter(age <16)

write_csv(df2014_adult, here("outputs", "data", "shs", "shs_2014_adult_clean.csv"))

write_csv(df2014_child, here("outputs", "data", "shs", "shs_2014_child_clean.csv"))

# 2013 data

df2013 <- read.table(here("inputs", "data", "shes13i_archive_v6.tab"), sep = "\t", header = T)  %>% select(CpserialA, SYear,psu, STRATA, int13wt, cint13wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic12, totinc, eqv5_15, hedqul08)

names(df2013) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2013_adult <- df2013 %>% filter(age >=16)

df2013_child <- df2013 %>% filter(age <16)

write_csv(df2013_adult, here("outputs", "data", "shs", "shs_2013_adult_clean.csv"))

write_csv(df2013_child, here("outputs", "data", "shs", "shs_2013_child_clean.csv"))

# 2012 data

df2012 <- read.table(here("inputs", "data", "shes12i_archive_v5.tab"), sep = "\t", header = T)  %>% select(cpserialA, SYear,psu, Strata, int12wt, cint12wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic12, totinc, eqv5_15, hedqul08)

names(df2012) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2012_adult <- df2012 %>% filter(age >=16)

df2012_child <- df2012 %>% filter(age <16)

write_csv(df2012_adult, here("outputs", "data", "shs", "shs_2012_adult_clean.csv"))

write_csv(df2012_child, here("outputs", "data", "shs", "shs_2012_child_clean.csv"))

# 2011 data

# no survey year
df2011 <- read.table(here("inputs", "data", "shes11i_v7.tab"), sep = "\t", header = T)  %>% select(pserialA,psu, strata, int11wt, cint11wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, Ethnic09, totinc, eqv5_15, hedqul08)

df2011$year <- 4

names(df2011) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

df2011_adult <- df2011 %>% filter(age >=16)

df2011_child <- df2011 %>% filter(age <16)

write_csv(df2011_adult, here("outputs", "data", "shs", "shs_2011_adult_clean.csv"))

write_csv(df2011_child, here("outputs", "data", "shs", "shs_2011_child_clean.csv"))

# 2010 data

df2010 <- read.table(here("inputs", "data", "shes10i_v5.tab"), sep = "\t", header = T)  %>% select(pserialA,psu, strata, int10wt, cint10wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, ethgroup, totinc, eqv5_15, hedqul08)

df2010$year <- 3

names(df2010) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

df2010_adult <- df2010 %>% filter(age >=16)

df2010_child <- df2010 %>% filter(age <16)

write_csv(df2010_adult, here("outputs", "data", "shs", "shs_2010_adult_clean.csv"))

write_csv(df2010_child, here("outputs", "data", "shs", "shs_2010_child_clean.csv"))

# 2009 data

df2009 <- read.table(here("inputs", "data", "shes09i_v6.tab"), sep = "\t", header = T)  %>% select(pserialA,psu, strata, int09wt, cint09wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, Ethnic09, totinc, eqv5_15, hedqul08)

df2009$year <- 2

names(df2009) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

df2009_adult <- df2009 %>% filter(age >=16)

df2009_child <- df2009 %>% filter(age <16)

write_csv(df2009_adult, here("outputs", "data", "shs", "shs_2009_adult_clean.csv"))

write_csv(df2009_child, here("outputs", "data", "shs", "shs_2009_child_clean.csv"))

# 2008 data

df2008 <- read.table(here("inputs", "data", "shes08i_v12.tab"), sep = "\t", header = T)  %>% select(pserialA,psu, strata, int08wt, cint08wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, EthnicI, totinc, eqv5_15, hedqul08)

df2008$year <- 1

names(df2008) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

df2008_adult <- df2008 %>% filter(age >=16)

df2008_child <- df2008 %>% filter(age <16)

write_csv(df2008_adult, here("outputs", "data", "shs", "shs_2008_adult_clean.csv"))

write_csv(df2008_child, here("outputs", "data", "shs", "shs_2008_child_clean.csv"))

# 2003 data

# no children BMI
df2003 <- read.table(here("inputs", "data", "shs03i_revised.tab"), sep = "\t", header = T)  %>% select(PSERIAL,psu, strata, int_wt, cint_wt, bmival, htval, wtval, bmivg5, age, Sex, simd5, EthnicI, totinc, eqv5, hedqual)

df2003$year <- 0

names(df2003) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

df2003_adult <- df2003 %>% filter(age >=16)

df2003_child <- df2003 %>% filter(age <16)

write_csv(df2003_adult, here("outputs", "data", "shs", "shs_2003_adult_clean.csv"))

write_csv(df2003_child, here("outputs", "data", "shs", "shs_2003_child_clean.csv"))



# no split between children and adult weight, bmi grouped in 6 categories, carstair index instead of multiple deprivation, no income, no education
df1998 <- read.table(here("inputs", "data", "shs98i.tab"), sep = "\t", header = T)  %>% select(archsn,psu, region, weight, bmival, htval, wtval, bmivg6, age, sex, carstg5, ethnic)

# no derived weight and height variables
df1995 <- read.table(here("inputs", "data", "shs95i.tab"), sep = "\t", header = T)  %>% select(serialx,psu, region, weight, bmiok, bmi, htok, wtok, height, weight, bmiag1, respage, respsex, cargp5, ethnic)


# First, create a survey design object for the dataset.

adult <- df2019 %>% filter(!is.na(int_wt))

svy.dat <- svydesign(ids=~adult$psu, 
                     nest = T,
                     data=adult,
                     strata = adult$strata,
                     weights=adult$int_wt)

svy.child <- svydesign(ids=~child$PSU, 
                     nest = T,
                     data=child,
                     strata = child$Strata,
                     weights=child$cint19wt)

svymean(~bmi, svy.dat)
svymean(~age, svy.dat)

svytable(~bmi_class,svy.dat)

# equating





