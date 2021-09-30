setwd("~")
source("vFI_expts.R")
source("vFI_functions.R")
source("vFI_utils.R")


data <- read.csv("Data/df_video_features.csv", header = TRUE, stringsAsFactors = FALSE)
df <- read_data(data = data, summary_phenotypes = "median", type = "")
df <- preprocess_data(df, type = "")$df


data_frailty <- read.csv("Data/df_frailty_parameters.csv", header = TRUE, stringsAsFactors = TRUE)
df_frailty <- read_data(data = data_frailty, type = "frailty")
df_frailty <- preprocess_data(df_frailty, type = "frailty")

#Merged data
df <- df[with(df, order(MouseID)),]
df_frailty <- df_frailty[with(df_frailty, order(MouseID)),]
df_merged <- cbind(df,df_frailty)

#Experiment I: Test the performance of the video features in predicting FI and TestAge
#Experiment II: Test the performance of Age (alone) vs video features in predicting FI
#Experiment III: Test the effect of the train-test split ratio on prediction performance
#Experiment IV: Compare prediction of Age from frailty parameters versus prediction of Age from video features
#Experiment V: Predict frailty parameters from video features

results <- synthetic_experiment(nsim = 50, expt = "I", data = df)
save_results(results, expt = "I")
#results <- synthetic_experiment(nsim = 50, expt = "II", data = df)
#results <- synthetic_experiment(nsim = 50, expt = "III", data = df)
#results <- synthetic_experiment(nsim = 50, expt = "IV", data = df, data_f = df_frailty)
#results <- synthetic_experiment(nsim = 50, expt = "V", data = df_merged)
