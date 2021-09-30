setwd("~")
source("VF_expts.R")
source("VF_functions.R")
source("VF_utils.R")

data <- read.csv("Data/df_video_features.csv", header = TRUE, stringsAsFactors = FALSE)
df <- read_data(data = data, summary_phenotypes = "median", type = "")
df <- preprocess_data(df, type = "")$df

manual_frailty_data <- read.csv("Data/df_frailty_parameters.csv", header = TRUE)
p <- plot_frailty_re(data = manual_frailty_data)
dev.print(pdf, "Tester-effect-frailty-parameters.pdf", height = 5.22, width = 12) 
#Figure 1

#1A,B,C,D
p <- plot_data(df)
dev.print(pdf,'1ABCD.pdf',width=14.4,height=3.3)

#1E,F
p1 <- plot_results(mae = "Age_MAE_I.csv", rmse = "Age_RMSE_I.csv", r2 = "Age_R2_I.csv", yname = "Age", expt = "I")
p2 <- plot_results(mae = "FI_MAE_I.csv", rmse = "FI_RMSE_I.csv", r2 = "FI_R2_I.csv", yname = "score", expt = "I")

p1|p2
dev.print(pdf,'1EF.pdf',width=19.45,height=4.0)

#1G,H
p <- predint_width(mae = "Age_MAE_I.csv", yname = "TestAge")
dev.print(pdf,"1G.pdf", width = 8.5, height = 4.5)
p <- predint_width(mae = "FI_MAE_I.csv", yname = "score")
dev.print(pdf,"1H.pdf", width = 8.5, height = 4.5)

#1I,J
#todo
p <- plot_pred_int(mae = "Age_MAE_I.csv", yname = "TestAge", type = "oob")
dev.print(pdf,'oob_age.pdf',width=19.45,height=4.0)

p <- plot_pred_int(mae = "FI_MAE_I.csv", yname = "score", type = "oob")
dev.print(pdf,'oob_vfi.pdf',width=19.45,height=4.0)



#Figure 2

#2A
p <- interpretable_ML_plot(yname = "score", mae = "FI_MAE_I.csv", type = "feature_importance")
dev.print(pdf,'2A.pdf', width=16,height=5)

#2B
p1 <- interpretable_ML_plot(yname = "score", mae = "FI_MAE_I.csv", type = "marginal", phenotype = "tip_tail_lateral_displacement_iqr", phenoname = "Tip Tail LD IQR")
p2 <- interpretable_ML_plot(yname = "score", mae = "FI_MAE_I.csv", type = "marginal", phenotype = "median_rearpaw", phenoname = "Rearpaw")
p3 <- interpretable_ML_plot(yname = "score", mae = "FI_MAE_I.csv", type = "marginal", phenotype = "median_width", phenoname = "Width")
p4 <- interpretable_ML_plot(yname = "score", mae = "FI_MAE_I.csv", type = "marginal", phenotype = "median_step_width", phenoname = "StepWidth")
p5 <- interpretable_ML_plot(yname = "score", mae = "FI_MAE_I.csv", type = "marginal", phenotype = "dB_median", phenoname = "dB")

p1|p2|p3|p4|p5
dev.print(pdf,'2B.pdf', width=16,height=2.9)


#2C
p <- interpretable_ML_plot(yname = "score", mae = "FI_MAE_I.csv", type = "interaction")
dev.print(pdf,'2C.pdf', width=6,height=10)

#2D,E
p <- interpretable_ML_plot(yname = "score", mae = "FI_MAE_I.csv", type = "", phenotypes = c("median_step_width","median_step_length1"), phenonames = c("Step Width", "Step Length1"))
dev.print(pdf,'2D.pdf', width=7.1,height=5.3)

p <- interpretable_ML_plot(yname = "score", mae = "FI_MAE_I.csv", type = "", phenotypes = c("median_width","median_length"), phenonames = c("Width", "Length"))
dev.print(pdf,'2E.pdf', width=7.1,height=5.3)



#Supplement/Rebuttal figures 
#Experiment II 
p <- plot_results(mae = "FI_MAE_II.csv", rmse = "FI_RMSE_II.csv", r2 = "FI_R2_II.csv", expt = "II")
dev.print(pdf, "syn-expt-II-results.pdf", width = 12, height = 10)

#Feature importance Age versus video 
p <- feature_importance_age_vs_video(mae = c("FI_MAE_I.csv","Age_MAE_I.csv"))
dev.print(pdf,'age-vs-video-feature-importance.pdf', width=8,height=7)


#Experiment III
p1 <- plot_results(mae = "Age_MAE_III.csv", rmse = "Age_RMSE_III.csv", r2 = "Age_R2_III.csv", yname = "Age", expt="III")
p2 <- plot_results(mae = "FI_MAE_III.csv", rmse = "FI_RMSE_III.csv", r2 = "FI_R2_III.csv", yname = "score", expt="III")
p1|p2
dev.print(pdf,'syn-exp-III-results.pdf',width=19.45,height=4.0)

#Experiment IV
mae <- c("Age_MAE_IV.csv","Age_MAE_video_IV.csv")
rmse <- c("Age_RMSE_IV.csv","Age_RMSE_video_IV.csv")
r2 <- c("Age_R2_IV.csv","Age_R2_video_IV.csv")
p1 <- plot_results(mae=mae, rmse = rmse, r2 = r2, expt="IV")
dev.print(pdf,'syn-exp-IV-results.pdf',width=9.5,height=3.5)
p2 <- interpretable_ML_plot(yname = "TestAge", mae = mae, type ="feature_importance", frailty = TRUE)
dev.print(pdf,'syn-exp-IV-var-imp.pdf',width=7.2,height=4.9)

model_comparisons(mae,rmse,r2,type = "frailty")

#Experiment 5
save_results(results, expt = "V")
p <- plot_results(precision = "precision_V.csv", recall = "recall_V.csv", f1score = "f1score_V.csv", acc = "acc_V.csv", expt = "V")
dev.print(pdf,"expt_V_results_2.pdf", width = 12, height = 9)