# Pipeline

To run the pipeline in full run the script `run_pipeline.R`. This script reads the other files in this folder and runs them in one go: 

* `obesity_trend.R`: this script generates a processed data table with obesity prevalence for each year in the model
* `equating.R`: this script generates a set of tables with the percentage weight change for each BMI category
* `run_hall.R`: this script runs the Hall model over all the years selected
* `extrapolate.R`: this script uses the outputs of the Hall model to extrapolate to different levels of obesity prevalence
* `main_charts.R`: this script produces the charts that summarise the main findings of the analysis
* `edi_modelling.R`: this script produces charts that summarise the findings for breakdowns of background variables
* `validation_charts.R`: this script produces charts that validate the model by calculating the BMI distribution under different scenarios
