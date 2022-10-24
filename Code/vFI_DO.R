libs <- c("lme4", "lmerTest", "caret", "ggplot2")
sapply(libs, require, character.only = TRUE)

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

testdata <- read.csv("Data/DOdf_10_12_22.csv", header = TRUE, stringsAsFactors = TRUE)
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
data <- (read.csv("Data/DOdf_10_12_22.csv"))
df <- read_data(data = data, summary_phenotypes = "", type = "DO")
df <- preprocess_data(df, type = "DO")$df

results <- synthetic_experiment(nsim = 10, expt = "I", data = df)
saveRDS(results, file = "Results/exptII_diet_adjusted_outliers_removed.RData")
results <- readRDS("Results/exptII_diet_adjusted.RData")

resultsFI <- results$results_FI
resultsFI_mae <- reshape2::melt(resultsFI$MAE)
resultsFI_rmse <- reshape2::melt(resultsFI$RMSE)
resultsFI_R2 <- reshape2::melt(resultsFI$R2)
resultsFI <- data.frame(rbind(resultsFI_mae, resultsFI_rmse, resultsFI_R2))
resultsFI$metric <- c(rep("MAE",40), rep("RMSE",40), rep("R2",40))

p1 <- ggplot(resultsFI, aes(x = variable, y = value)) + geom_boxplot(fill = "#377eb8", alpha = 0.7) + geom_point() + theme_bw(base_size = 22) + labs(y = "Metric", x = "Model") + facet_wrap(.~metric, scales = "free") + ggtitle("Expt I: Predict frailty")
dev.print(pdf, "exptII_frailty.pdf")

resultsAge <- results$results_Age
resultsAge_mae <- reshape2::melt(resultsAge$MAE)
resultsAge_rmse <- reshape2::melt(resultsAge$RMSE)
resultsAge_R2 <- reshape2::melt(resultsAge$R2)
resultsAge <- data.frame(rbind(resultsAge_mae, resultsAge_rmse, resultsAge_R2))
resultsAge$metric <- c(rep("MAE",40), rep("RMSE",40), rep("R2",40))

p2 <- ggplot(resultsAge, aes(x = variable, y = value)) + geom_boxplot(fill = "#e41a1c", alpha = 0.7) + geom_point() + theme_bw(base_size = 22) + labs(y = "Metric", x = "Model") + facet_wrap(.~metric, scales = "free") + ggtitle("Expt I: Predict Age")


require(patchwork)
p2|p1
dev.print(pdf,'do_exptII_results_diet_adjusted_outliers_removed.pdf',width=19.45,height=4.0)


#Exploratory analysis 
#Load C57 data 
dataB6 <- read.csv("/home/sabnig/vFI-modeling/Data/df_video_features.csv", header = TRUE, stringsAsFactors = FALSE)
dfB6 <- read_data(data = dataB6, summary_phenotypes = "median", type = "")
dfB6 <- preprocess_data(dfB6, type = "")$df
#dfB6[,-c(1,2,3)] <- scale(dfB6[,-c(1,2,3)], center = TRUE, scale = TRUE)
dfB6$Type <- as.factor(rep("B6", nrow(dfB6)))

dfB6_melt <- reshape2::melt(dfB6[,-c(2,3)], ids = c("MouseID"))

dfB6_y <- dfB6[, which(names(dfB6) %in% c("MouseID", "score", "TestAge", "Type"))]

#ggplot(dfB6_melt, aes(x = value)) + geom_density() + facet_wrap(. ~ variable, scales = "free") + theme_bw()

dfB6_melt$Type <- as.factor(rep("B6", nrow(dfB6_melt)))

#Load DO data
data <- (read.csv("Data/DOdf_10_12_22.csv"))
df <- read_data(data = data, summary_phenotypes = "", type = "DO")
df <- preprocess_data(df, type = "DO")$df
#df[,-c(1,2,3)] <- scale(df[,-c(1,2,3)], center = TRUE, scale = TRUE)
df$Type <- as.factor(rep("DO", nrow(df)))

df_melt <- reshape2::melt(df[,-c(2,3)], ids = c("MouseID"))
df_melt$Type <- as.factor(rep("DO", nrow(df_melt)))

#ggplot(df_melt, aes(x = value)) + geom_density() + facet_wrap(. ~ variable, scales = "free") + theme_bw()

dfall <- rbind(dfB6_melt, df_melt)
dfall <- dfall[!dfall$variable %in% c("dAC_stdev", "dB_stdev", "aABC_stdev"), ]

df_y <- df[, which(names(df) %in% c("MouseID", "score", "TestAge", "Type"))]


dfall_y <- rbind(dfB6_y, df_y)
dfallmelt_y <- reshape2::melt(dfall_y, ids = c("MouseID", "Type"))

#Don't scale the features for the density plot 
ggplot(dfall, aes(x = value, color = Type)) + geom_density() + facet_wrap(. ~ variable, scales = "free") + theme_bw() + scale_color_brewer(palette = "Set1")

#Scale the features for the boxplot 
ggplot(dfall, aes(y = value, x = variable, color = Type)) + geom_boxplot(position = position_dodge(width = 1)) + theme_bw(base_size = 16) + scale_color_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggplot(dfallmelt_y, aes(x = value, color = Type)) + geom_density(lwd = 1) + facet_wrap(. ~ variable, scales = "free") + theme_bw(base_size = 16) + scale_color_brewer(palette = "Set1")





df <- df[, -which(names(df) %in% setdiff(names(df), names(dfB6)))]



df_melt <- reshape2::melt(df[,-c(2,3)], id = c("MouseID"))
dfB6_melt <- reshape2::melt(dfB6[,-c(2,3)], id = c("MouseID"))

df_melt$score <- rep(df$score, 45)
df_melt$TestAge <- rep(df$TestAge, 45)
dfB6_melt$score <- rep(dfB6$score, 45)
dfB6_melt$TestAge <- rep(dfB6$TestAge, 45)

df_melt$Type <- as.factor(rep("DO", nrow(df_melt)))
dfB6_melt$Type <- as.factor(rep("B6", nrow(dfB6_melt)))

dfall <- rbind(dfB6_melt, df_melt)
dfall <- dfall[!dfall$variable == "Type",]


ggplot(dfB6_melt[dfB6_melt$variable %in% c("center_time_secs", "periphery_time_secs"),], aes(x = value)) + geom_density() + theme_bw(base_size = 16) + facet_wrap(. ~ variable, scales = "free")


#dfall <- rbind(dfB6[, intersect(names(dfB6), names(df))], df[, intersect(names(dfB6), names(df))])

df_melt <- reshape2::melt(df[,-c(2,3)], id = c("MouseID"))
ggplot(df_melt, aes(y = value, x = variable, color = Type)) + geom_point() + geom_boxplot() + theme_bw(base_size = 16) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

dev.print(pdf, "feature_boxplots.pdf", width = 22.4, height = 6.8)

ggplot(df, aes(x = score)) + geom_density() + theme_bw()

df_melt$score <- rep(df$score, 46)
df_melt$TestAge <- rep(df$TestAge, 46)



ggplot(df_melt, aes(x = value, y = score, color = Type)) + geom_point(size = 2) + theme_bw(base_size = 16) + geom_smooth(method = "loess", color = "red") + facet_wrap(. ~ variable, scales = "free")
dev.print(pdf, "frailty_density.pdf")

remove_outlier <- function(trait){
	df_tmp <- df[,paste0(trait)]
	out <- boxplot(df_tmp, plot = FALSE)$out
	ind <- which(df_tmp %in% out)
	df[ind, paste0(trait)] <- NA
	return(as.numeric(df[,paste0(trait)]))
}

df[,-c(1,2,3)] <- sapply(seq(ncol(df[,-c(1,2,3)])), function(x) remove_outlier(colnames(df)[3+x]))

tmp <- data.frame(Feature = names(df[,-c(1,2,3)]), Number = sapply(seq(ncol(df[,-c(1,2,3)])), function(x) sum(is.na(df[,3+x]))))


ggplot(df_melt, aes(x = value)) + geom_density() + theme_bw(base_size = 16) + facet_wrap(. ~ variable, scales = "free")
dev.print(pdf, "feature_density.pdf")

ggplot(tmp, aes(x = Feature, y = Number)) + geom_col() + theme_bw(base_size = 16) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + labs(y = "Outlier count")
dev.print(pdf, "outlier_count.pdf")

dev.print(pdf, "Results/exploratory_outlier_count.pdf")
df <- df[complete.cases(df),]

data <- (read.csv("Data/DOdf_10_12_22.csv"))
df <- read_data(data = data, summary_phenotypes = "", type = "DO")
df <- preprocess_data(df, type = "DO")$df

df[,-c(1,2,3)] <- scale(df[,-c(1,2,3)], center = TRUE, scale = TRUE)

#Predict FI
yname <- "score"
train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = 1, type = "")
traindata <- train_test$traindata
testdata <- train_test$testdata

Ytrain <- traindata[,paste0(yname)]
Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname)))]
Xtrain_pc <- prcomp(Xtrain)$x[,1:16]

enet <- caret::train(y=Ytrain,x=Xtrain, preProcess = c("BoxCox"), method="enet",trControl=trainControl(method="repeatedcv",repeats=1),tuneLength=10)
svm <- caret::train(y=Ytrain, x=Xtrain, preProcess = c("BoxCox"), method = "svmLinear",trControl = trainControl(method="repeatedcv",repeats=1))
rf <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = "all",honesty=FALSE)	
xgb <- caret::train(y=Ytrain, x=Xtrain, preProcess = c("BoxCox"), method='xgbTree', objective='reg:squarederror',trControl=trainControl(method='repeatedcv',repeats=1), verbosity = 0)
