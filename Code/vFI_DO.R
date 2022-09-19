setwd("/home/sabnig/vFI-modeling")
source("Code/vFI_functions.R")
source("Code/vFI_expts.R")

#ExptI: Train on C57 mice and test on DO mice 
data <- read.csv("/home/sabnig/vFI-modeling/Data/df_video_features.csv", header = TRUE, stringsAsFactors = FALSE)
df <- read_data(data = data, summary_phenotypes = "median", type = "")
df <- preprocess_data(df, type = "")$df
df <- df[, order(names(df))]
df <- df[, -which(names(df) %in% c("dB_nongait_stdev"))]

yname <- "score"
#yname <- "TestAge"
df0 <- do.call(rbind, lapply(split(df, df$MouseID), function(x) x[sample(nrow(x), 1),]))
train <- caret::createDataPartition(df0[,paste0(yname)], p = 1, list = FALSE)
dfTrain <- df[df$MouseID %in% df0[train,'MouseID'],]
#traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID","TestAge"))]
traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID","TestAge"))]
traindata_mean <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], mean))
traindata_sd <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], sd))
traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))] <- scale(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], center = TRUE, scale = TRUE)

#models <- fit_models(traindata = traindata, yname = "score", expt = "I")
models <- fit_models(traindata = traindata, yname = "score", expt = "I")

testdata <- read.csv("Data/DOdf_05_02_22.csv", header = TRUE, stringsAsFactors = TRUE)
names(testdata)[names(testdata) == "CFI"] <- "score"
names(testdata)[names(testdata) == "age_in_weeks"] <- "TestAge"
testdata <- testdata[,which(names(testdata) %in% names(traindata))]
testdata <- testdata[,order(names(testdata))]
testdata <- testdata[complete.cases(testdata),]
testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))] <- scale(testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))], center = traindata_mean, scale = traindata_sd)

#res <- performance_measure(models, testdata = testdata, yname = yname, expt = "I")
Xtest <- testdata[,-which(names(testdata) %in% c(paste0(yname)))]
Ytest <- testdata[,paste0(yname)]

enethat <- predict(models[["enet"]],newdata = Xtest)
svmhat <- predict(models[["svm"]], newdata = Xtest)
rfhat <- predict(models[["rf"]],Xtest)$predictions
xgbhat <- predict(models[["xgb"]],newdata=Xtest)

mae <- c(mean(abs(Ytest-enethat)),mean(abs(Ytest-svmhat)),mean(abs(Ytest-rfhat)),mean(abs(Ytest-xgbhat)))
rmse <- c(sqrt(mean((Ytest-enethat)^2)),sqrt(mean((Ytest-svmhat)^2)),sqrt(mean((Ytest-rfhat)^2)),sqrt(mean((Ytest-xgbhat)^2)))
R2 <- c(cor(Ytest,enethat)^2,cor(Ytest,svmhat)^2,cor(Ytest,rfhat)^2,cor(Ytest,xgbhat)^2)


#ExptII: Train and test on DO mice

#Load DO data
data <- (read.csv("Data/DOdf_05_02_22.csv"))
df <- read_data(data = data, summary_phenotypes = "", type = "DO")
df <- preprocess_data(df, type = "DO")$df

results <- synthetic_experiment(nsim = 10, expt = "I", data = df)
saveRDS(results, file = "Results/exptII_diet_adjusted.RData")
results <- readRDS("Results/exptII_diet_adjusted.RData")

resultsFI <- results$results_FI
resultsFI_mae <- reshape2::melt(resultsFI$MAE)
resultsFI_rmse <- reshape2::melt(resultsFI$RMSE)
resultsFI_R2 <- reshape2::melt(resultsFI$R2)
resultsFI <- data.frame(rbind(resultsFI_mae, resultsFI_rmse, resultsFI_R2))
resultsFI$metric <- c(rep("MAE",40), rep("RMSE",40), rep("R2",40))

p1 <- ggplot(resultsFI, aes(x = variable, y = value)) + geom_boxplot(fill = "#377eb8", alpha = 0.7) + geom_point() + theme_bw(base_size = 22) + labs(y = "Metric", x = "Model") + facet_wrap(.~metric, scales = "free") + ggtitle("Expt II: Predict frailty")
dev.print(pdf, "exptII_frailty.pdf")

resultsAge <- results$results_Age
resultsAge_mae <- reshape2::melt(resultsAge$MAE)
resultsAge_rmse <- reshape2::melt(resultsAge$RMSE)
resultsAge_R2 <- reshape2::melt(resultsAge$R2)
resultsAge <- data.frame(rbind(resultsAge_mae, resultsAge_rmse, resultsAge_R2))
resultsAge$metric <- c(rep("MAE",40), rep("RMSE",40), rep("R2",40))

p2 <- ggplot(resultsAge, aes(x = variable, y = value)) + geom_boxplot(fill = "#e41a1c", alpha = 0.7) + geom_point() + theme_bw(base_size = 22) + labs(y = "Metric", x = "Model") + facet_wrap(.~metric, scales = "free") + ggtitle("Expt II: Predict Age")


require(patchwork)
p2|p1
dev.print(pdf,'do_exptII_results_diet_adjusted.pdf',width=19.45,height=4.0)


#Exploratory analysis 
#Load DO data
data <- (read.csv("Data/DOdf_05_02_22.csv"))
df <- read_data(data = data, summary_phenotypes = "", type = "DO")
df <- preprocess_data(df, type = "DO")$df

df[,-c(1,2,3)] <- scale(df[,-c(1,2,3)], center = TRUE, scale = TRUE)

df_melt <- reshape2::melt(df[,-c(2,3)], id = "MouseID")
ggplot(df_melt, aes(y = value, x = variable)) + geom_point() + geom_boxplot() + theme_bw(base_size = 16) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

ggplot(df, aes(x = score)) + geom_density() + theme_bw()

df_melt$score <- rep(df$score, 46)
df_melt$TestAge <- rep(df$TestAge, 46)

ggplot(df_melt, aes(x = value, y = score)) + geom_point(size = 2) + theme_bw(base_size = 16) + geom_smooth(method = "loess", color = "red") + facet_wrap(. ~ variable, scales = "free")

remove_outlier <- function(trait){
	df_tmp <- df[,paste0(trait)]
	out <- boxplot(df_tmp, plot = FALSE)$out
	ind <- which(df_tmp %in% out)
	df[ind, paste0(trait)] <- NA
	return(as.numeric(df[,paste0(trait)]))
}

df[,-c(1,2,3)] <- sapply(seq(ncol(df[,-c(1,2,3)])), function(x) remove_outlier(colnames(df)[3+x]))

tmp <- data.frame(Feature = names(df[,-c(1,2,3)]), Number = sapply(seq(ncol(df[,-c(1,2,3)])), function(x) sum(is.na(df[,3+x]))))
