# Pipeline

To run the pipeline in full run the script `run_pipeline.R`. This script reads the other files in this folder and runs them in one go: 

* `equating.R`: this script generates a set of tables with the percentage weight change for each BMI category
* `run_hall.R`: this script runs the Hall model over all the years selected
* `extrapolate.R`: this script uses the outputs of the Hall model to extrapolate to different levels of obesity prevalence
