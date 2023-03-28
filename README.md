# Simulating Calorie Reduction Values to Meet Obesity Reduction Targets in Scotland

## Requirements

This project was built with R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid" and RStudio 2022.07.2+576 "Spotted Wakerobin" Release (e7373ef832b49b2a9b88162cfe7eac5f22c40b34, 2022-09-06) for macOS Mozilla/5.0 (Macintosh; Intel Mac OS X 12_6\_0) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.10 Chrome/69.0.3497.128 Safari/537.36


The list of packages needed to run the project is automatically installed by opening and running the script `requirements.R` which automatically checks whether the packages needed for this analysis are installed and if they are not it installs them.


## Datasets

This analysis is based on Scottish Health Survey data made available on the [UK Data Service](https://ukdataservice.ac.uk/) portal. To run this project you need to download and save a copy of the files locally to the `inputs\data` folder. Please note that you need a UKDS account to access data. More information on how to create a UKDS account is available on the [UK DataService website](https://beta.ukdataservice.ac.uk/myaccount/credentials). You also need to create a project on the UKDS portal. After having created an account, logged in and created a project follow the instructions below:

1.  Follow this [link](https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000047) to access the Scottish Health Survey series
2.  Click on the Access data button
3.  On the next page click on Add to account
4.  Head to your account and add all the datasets to a project
5.  Head to your project, click on Actions, and then click on Download from the drop down menu
6.  Select the TAB option and then Download selected
7.  A zip folder will be downloaded: extract the files and head to the tab folder: the data files needed for this project are the individual level files (with suffix `i`)
8.  Save the files to `inputs\data`

For internal Nesta users the files are saved in the S3 bucket [ahl-scotland-modelling\inputs](https://s3.console.aws.amazon.com/s3/buckets/ahl-scotland-modelling?region=eu-west-2&prefix=inputs/)

## How to Run this Project

To run this project please ensure that all packages are installed by following the instructions in the Requirements section. After that follow these steps:

-   In the `data_processing` folder open and run the file `read_clean_save_scotland.R` (further info in the README of this folder)
-   In the `pipeline` folder run the files in this order:
    -   `equating.R`

    -   `run_hall.R`

    -   `extrapolate.R`

All outputs are saved in the `outputs` folder (`data`, `reports`, and `figures`).

## Directory

```
  ├── ahl_scotland_modelling                
  │   ├── data_processing
  │   │   ├── read_clean_save_scotland.R    <- Pre-processing of data
  │   │   ...
  │   ├── pipeline                          <- Holds scripts for all pipeline components
  │   ├── utils                             <- Utility functions needed across different parts of the codebase
  │   ...
  ├── inputs
  │   └── data                              <- Holds the Scottish Health Survey Data
  │   ...
  └── outputs
      ├── data                              <- Holds cleaned data
      ├── reports                           <- Summary tables are saved here
      ├── figures                   
      ├── └── png                           <- Charts are saved here
      ...

```

## Ackowledgements

This project relies on the `bw` package which was developed by researchers at the [National Institute of Public Health of Mexico](https://www.insp.mx/insp-overview.html), to which we are really grateful. The package implements the [Dynamic Weight Change model from Hall et al. (2011)](https://pubmed.ncbi.nlm.nih.gov/21872751/) for adults.
