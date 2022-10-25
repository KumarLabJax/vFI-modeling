# A machine vision based frailty index for mice

## Table of Contents
- [Features](#features)
- [Requirements](#requirements)
- [Load packages](#load-packages)
- [Load functions](#load-functions)
- [Load data](#load-data)
- [Preprocessing](#preprocessing)
- [Run a synthetic experiment](#train-the-model)
- [Sample code](#sample-code)
- [Train models](#train-models)
- [Plot results](#plot-results)
- [References](#references)

## Features

The scripts to generate features used here originate as output from this other repo: https://github.com/KumarLabJax/vFI-features. 

## Requirements
This program requires the following R packages: 
```
reshape
lme4
caret
RLRSim
grf
rfinterval
mgcv
quantreg
ALEPlot
iml
ggplot
ggrepel
```

## Load packages

```
packages <- c("reshape","lme4","caret","RLRSim","grf","rfinterval","mgcv","quantreg","ALEPlot","iml","ggplot","ggrepel")
lapply(packages, require, character.only = TRUE)
```

## Load functions

```
source("vFI_expts.R")
source("vFI_functions.R")
source("vFI_utils.R")
```


## Load data

```
data <- read.csv("Data/data.csv", header = TRUE, stringsAsFactors = FALSE)
data_frailty <- read.csv("Data/data_frailty_parameters.csv", header = TRUE, stringsAsFactors = TRUE)
```

## Preprocessing

```
df <- read_data(data = data, summary_phenotypes = "median", type = "")
df <- preprocess_data(df, type = "")$df

df_frailty <- read_data(data = data_frailty, type = "frailty")
df_frailty <- preprocess_data(df_frailty, type = "frailty")
```

## Run a synthetic experiment

```
#Experiment I: Test the performance of the video features in predicting FI and TestAge

results <- synthetic_experiment(nsim = 50, expt = "I", data = df)
save_results(results, expt = "I")
```

## Sample code

Please refer to ```vFI.R``` as the starting point for running all synthetic experiments, which we recommend running on a high-performance computing cluster. 

## Train models

Please refer to ```vFI_functions.R``` and ```vFI_expts.R``` to see how the models are trained. ```yname``` is the dependent variable you're interested in predicting. 

```
models <- fit_models(traindata, yname, expt = "I")
results <- performance_measure(models, testdata, yname, expt = "I")
save_results(results, expt = "I") #saves mae, rmse and r2 measures as csv files. These csv files are used as inputs to the functions below

plot_results(mae, rmse, r2, yname, expt = "I") 
plot_pred_int(mae, yname, type = "") #plot prediction intervals
interpretable_ML_plot(yname, mae, type) #type = c("feature_importance", "marginal", interaction")


```


## Plot results

Please refer to ```vFI_Figures.R``` to recreate Figures 4,5 and Supplemental Figure 1 in the main manuscript. 

## References
Please refer to the main manuscript for all relevant references. 

