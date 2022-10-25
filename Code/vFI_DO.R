libs <- c("lme4", "lmerTest", "caret", "ggplot2")
sapply(libs, require, character.only = TRUE)

setwd("/home/sabnig/vFI-modeling")
source("Code/vFI_functions.R")
source("Code/vFI_expts.R")
source("Code/vFI_functions_DO.R")
source("Code/vFI_expts_DO.R")
source("Code/vFI_plots_DO.R")

#Load C57BL/6J data
data <- read.csv("Data/df_video_features.csv", header = TRUE, stringsAsFactors = FALSE)
df <- read_data(data = data, summary_phenotypes = "median", type = "")
df <- preprocess_data(df, type = "")$df
df <- df[, order(names(df))]


#Load DO data
dataDO <- read.csv("Data/DOdf_10_12_22.csv")
dfDO <- read_data_DO(data = df, dataDO = dataDO, type = "B6")


#Experiment I: Train a model on B6 mice (greater than 50 weeks old) and test on DO mice.
df_g50 <- df[df$TestAge >= 50, ]
results <- synthetic_experiment_DO(nsim = 1, data = df_g50, dataDO = dfDO, expt = "I")

#Experiment II: Train an independent model for each diet group separately and test performance
results <- synthetic_experiment_DO(nsim = 1, data = df, dataDO = dfDO, expt = "II")
plot_results(results_list = results, expt = "II")

