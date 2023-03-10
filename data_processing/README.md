
# Data Cleaning

The file `read_clean_save_scotland.R` reads each of the input files and:

* selects the variables needed for the analysis
* renames all variables so that they are consistent across different waves
* splits each wave between an adult and a child file
* in case of missing variables in a give year, the variables are added with a suitable value (e.g. wave year)
* saves cleaned version of files to `outputs\data`
