source("Code/vFI_functions.R")
source("Code/vFI_utils.R")

synthetic_experiment <- function(nsim,expt,data, data_f = ""){

	df <- data
	if (expt == "I"){

		#Synthetic Experiment I: Test the performance of the video features in predicting FI and TestAge
		results_FI <- create_storage_matrix(nsim, ncols = 4, col_names = c("Enet","SVM","RF","XGB"))
		results_Age <- create_storage_matrix(nsim, ncols = 4, col_names = c("Enet","SVM","RF","XGB"))

		for (sim in seq(nsim)){

			cat("Simulation Run = ", sim, "\n")

			#Predict FI
			yname <- "score"
			train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = sim, type = "")
			traindata <- train_test$traindata
			testdata <- train_test$testdata

			models <- fit_models(traindata = traindata, yname = yname, expt = "I")
			res <- performance_measure(models, testdata = testdata, yname = yname, expt = "I")
	
			results_FI[["MAE"]][sim,] <- res$MAE
			results_FI[["RMSE"]][sim,] <- res$RMSE
			results_FI[["R2"]][sim,] <- res$R2

			#Predict Age
			yname <- "TestAge"
			train_test <- train_test_data(df, yname = yname, prop = 0.80,seed = sim, type = "")
			traindata <- train_test$traindata
			testdata <- train_test$testdata

			models <- fit_models(traindata = traindata, yname = yname, expt = "I")
			res <- performance_measure(models, testdata = testdata, yname = yname, expt = "I")

			results_Age[["MAE"]][sim,] <- res$MAE
			results_Age[["RMSE"]][sim,] <- res$RMSE
			results_Age[["R2"]][sim,] <- res$R2
		}
		return(list(results_FI = results_FI, results_Age = results_Age))

	} else if (expt == "II"){

		#Synthetic Experiment II: Test the performance of Age (alone) vs video features in predicting FI
		results_FI <- create_storage_matrix(nsim, ncols = 4, col_names = c("Age_L","Age_G","Video","All"))


		for (sim in seq(nsim)){

			cat("Simulation Run = ", sim, "\n")

			#Predict FI
			yname <- "score"
			train_test <- train_test_data(df, yname = "", prop = 0.80, seed = sim, type = "")
			traindata <- train_test$traindata
			testdata <- train_test$testdata

			models <- fit_models(traindata = traindata, yname = yname, expt = "II")
			res <- performance_measure(models, testdata = testdata, yname = yname, expt = "II")

			results_FI[["MAE"]][sim,] <- res$MAE
			results_FI[["RMSE"]][sim,] <- res$RMSE
			results_FI[["R2"]][sim,] <- res$R2

		} 
		return(list(results_FI = results_FI))

	} else if (expt == "III") {

		#Synthetic Experiment III: Test the effect of the train-test split ratio on prediction performance
		prop <- seq(0.6,0.90,by = 0.05)
		results_FI_p <- create_storage_matrix(nsim, ncols = length(prop), col_names = as.character(prop))
		results_Age_p <- create_storage_matrix(nsim, ncols = length(prop), col_names = as.character(prop))

		for (p in seq(prop)){

			#cat("Training proportion = ", prop[p], "\n")
	
			for (sim in seq(nsim)){

				cat("Simulation Run = ", sim, "\n")

				#Predict FI
				yname <- "score"
				train_test <- train_test_data(df, yname = yname, prop = prop[p], seed = sim, type = "")
				traindata <- train_test$traindata
				testdata <- train_test$testdata

				models <- fit_models(traindata = traindata, yname = yname, expt = "III")
				res <- performance_measure(models, testdata = testdata, yname = yname, expt = "III")
	
				results_FI_p[["MAE"]][sim,p] <- res$MAE
				results_FI_p[["RMSE"]][sim,p] <- res$RMSE
				results_FI_p[["R2"]][sim,p] <- res$R2

				#Predict Age
				yname <- "TestAge"
				train_test <- train_test_data(df, yname = yname, prop = prop[p],seed = sim, type = "")
				traindata <- train_test$traindata
				testdata <- train_test$testdata

				models <- fit_models(traindata = traindata, yname = yname, expt = "III")
				res <- performance_measure(models, testdata = testdata, yname = yname, expt = "III")

				results_Age_p[["MAE"]][sim,p] <- res$MAE
				results_Age_p[["RMSE"]][sim,p] <- res$RMSE
				results_Age_p[["R2"]][sim,p] <- res$R2
			}
		}

		return(list(results_FI = results_FI_p, results_Age = results_Age_p))
	}

	else if (expt == "IV") {

		#Synthetic Experiment IV: Compare prediction of Age from frailty parameters versus prediction of Age from video features
		results_Age_video <- create_storage_matrix(nsim, ncols = 4, col_names = c("Enet","SVM","RF","XGB"))
		results_Age <- create_storage_matrix(nsim, ncols = 4, col_names = c("Enet","SVM","RF","XGB"))

		for (sim in seq(nsim)){

			cat("Simulation Run = ", sim, "\n")

			#Predict Age from video features 
			yname <- "TestAge"
			train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = sim,type = "")
			traindata <- train_test$traindata
			testdata <- train_test$testdata

			models <- fit_models(traindata = traindata, yname = yname, expt = "IV")
			res <- performance_measure(models, testdata = testdata, yname = yname, expt = "IV")

			results_Age_video[["MAE"]][sim,] <- res$MAE
			results_Age_video[["RMSE"]][sim,] <- res$RMSE
			results_Age_video[["R2"]][sim,] <- res$R2

			#Predict Age from frailty parameters  
			yname <- "TestAge"
			train_test <- train_test_data(data_f, yname = yname, prop = 0.80, seed = sim, type = "frailty")
			traindata <- train_test$traindata
			testdata <- train_test$testdata

			models <- fit_models(traindata = traindata, yname = yname, expt = "IV")
			res <- performance_measure(models, testdata = testdata, yname = yname, expt = "IV")

			results_Age[["MAE"]][sim,] <- res$MAE
			results_Age[["RMSE"]][sim,] <- res$RMSE
			results_Age[["R2"]][sim,] <- res$R2

		} 
		return(list(results_Age_video = results_Age_video, results_Age = results_Age))

	} else {

		#Synthetic Experiment V: Predict frailty parameters from video features
		frailty_parameters <- c("Alopecia", "Body.condition","Coat.condition","Distended.abdomen","Gait.disorders","Kyphosis","Loss.of.fur.colour","Loss.of.whiskers","Piloerection")
		

		results_precision_0 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "precision", type = "frailty")
		results_precision_5 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "precision", type = "frailty")
		results_precision_1 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "precision", type = "frailty")

		results_recall_0 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "recall", type = "frailty")
		results_recall_5 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "recall", type = "frailty")
		results_recall_1 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "recall", type = "frailty")
		
		results_f1score_0 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "f1score", type = "frailty")
		results_f1score_5 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "f1score", type = "frailty")
		results_f1score_1 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "f1score", type = "frailty")

		results_acc_0 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "acc", type = "frailty")
		results_acc_5 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "acc", type = "frailty")
		results_acc_1 <- create_storage_matrix(nsim, ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "acc", type = "frailty")
		
		all_results_precision_0 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "precision", type = "frailty")) 
		all_results_precision_5 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "precision", type = "frailty")) 
		all_results_precision_1 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "precision", type = "frailty")) 

		all_results_recall_0 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "recall", type = "frailty")) 
		all_results_recall_5 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "recall", type = "frailty")) 
		all_results_recall_1 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "recall", type = "frailty")) 

		all_results_f1score_0 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "f1score", type = "frailty")) 
		all_results_f1score_5 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "f1score", type = "frailty")) 
		all_results_f1score_1 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "f1score", type = "frailty")) 

		all_results_acc_0 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "acc", type = "frailty")) 
		all_results_acc_5 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "acc", type = "frailty")) 
		all_results_acc_1 <- data.frame(create_storage_matrix(nsim = length(frailty_parameters), ncols = 2, col_names = c("OrdinalNet", "OrdinalForest"), l_names = "acc", type = "frailty")) 

		for (f in seq(frailty_parameters)){
			
			cat("Predicting", frailty_parameters[f], "\n")
			yname <- frailty_parameters[f]

			for (sim in seq(nsim)){

				#cat("Simulation Run = ", sim, "\n")
				train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = sim, type = "")
				traindata <- train_test$traindata
				testdata <- train_test$testdata

				models <- fit_models(traindata = traindata, yname = yname, expt = "V")
  				res <- performance_measure(models, testdata = testdata, yname = yname, expt = "V")

  				results_precision_0[["precision"]][sim,] <- as.numeric(data.frame(res$precision)[1,])
				results_precision_5[["precision"]][sim,] <- as.numeric(data.frame(res$precision)[2,])
				results_precision_1[["precision"]][sim,] <- as.numeric(data.frame(res$precision)[3,])

				results_recall_0[["recall"]][sim,] <- as.numeric(data.frame(res$recall)[1,])
				results_recall_5[["recall"]][sim,] <- as.numeric(data.frame(res$recall)[2,])
				results_recall_1[["recall"]][sim,] <- as.numeric(data.frame(res$recall)[3,])

				results_f1score_0[["f1score"]][sim,] <- as.numeric(data.frame(res$f1score)[1,])
				results_f1score_5[["f1score"]][sim,] <- as.numeric(data.frame(res$f1score)[2,])
				results_f1score_1[["f1score"]][sim,] <- as.numeric(data.frame(res$f1score)[3,])

				results_acc_0[["acc"]][sim,] <- as.numeric(data.frame(res$acc)[1,])
				results_acc_5[["acc"]][sim,] <- as.numeric(data.frame(res$acc)[2,])
				results_acc_1[["acc"]][sim,] <- as.numeric(data.frame(res$acc)[3,])

			}

			all_results_precision_0[f,] <- as.numeric(colMeans(results_precision_0$precision)) 
			all_results_precision_5[f,] <- as.numeric(colMeans(results_precision_5$precision))
			all_results_precision_1[f,] <- as.numeric(colMeans(results_precision_1$precision))

			all_results_recall_0[f,] <- as.numeric(colMeans(results_recall_0$recall))
			all_results_recall_5[f,] <- as.numeric(colMeans(results_recall_5$recall))
			all_results_recall_1[f,] <- as.numeric(colMeans(results_recall_1$recall))

			all_results_f1score_0[f,] <- as.numeric(colMeans(results_f1score_0$f1score))
			all_results_f1score_5[f,] <- as.numeric(colMeans(results_f1score_5$f1score))
			all_results_f1score_1[f,] <- as.numeric(colMeans(results_f1score_1$f1score))

			all_results_acc_0[f,] <- as.numeric(colMeans(results_acc_0$acc))
			all_results_acc_5[f,] <- as.numeric(colMeans(results_acc_5$acc))
			all_results_acc_1[f,] <- as.numeric(colMeans(results_acc_1$acc))

		}

		results <- list(precision_0 = all_results_precision_0, precision_5 = all_results_precision_5, precision_1 = all_results_precision_1, recall_0 = all_results_recall_0, recall_5 = all_results_recall_5, recall_1 = all_results_recall_1, f1score_0 = all_results_f1score_0, f1score_5 = all_results_f1score_5, f1score_1 = all_results_f1score_1, acc_0 = all_results_acc_0, acc_5 = all_results_acc_5, acc_1 = all_results_acc_1)
		results <- lapply(results, "rownames<-", frailty_parameters)

		precision_enet <- data.frame(results$precision_0[,1], results$precision_5[,1], results$precision_1[,1])
		colnames(precision_enet) <- c("0","0.5","1")
		precision_enet <- cbind(frailty_parameters = frailty_parameters, precision_enet)
		precision_rf <- data.frame(results$precision_0[,2], results$precision_5[,2], results$precision_1[,2])
		colnames(precision_rf) <- c("0","0.5","1") 
		precision_rf <- cbind(frailty_parameters = frailty_parameters, precision_rf)

		recall_enet <- data.frame(results$recall_0[,1], results$recall_5[,1], results$recall_1[,1])
		colnames(recall_enet) <- c("0","0.5","1")
		recall_enet <- cbind(frailty_parameters = frailty_parameters, recall_enet)
		recall_rf <- data.frame(results$recall_0[,2], results$recall_5[,2], results$recall_1[,2])
		colnames(recall_rf) <- c("0","0.5","1") 
		recall_rf <- cbind(frailty_parameters = frailty_parameters, recall_rf)

		f1score_enet <- data.frame(results$f1score_0[,1], results$f1score_5[,1], results$f1score_1[,1])
		colnames(f1score_enet) <- c("0","0.5","1")
		f1score_enet <- cbind(frailty_parameters = frailty_parameters, f1score_enet)
		f1score_rf <- data.frame(results$f1score_0[,2], results$f1score_5[,2], results$f1score_1[,2])
		colnames(f1score_rf) <- c("0","0.5","1") 
		f1score_rf <- cbind(frailty_parameters = frailty_parameters, f1score_rf)

		acc_enet <- data.frame(results$acc_0[,1], results$acc_5[,1], results$acc_1[,1])
		colnames(acc_enet) <- c("0","0.5","1")
		acc_enet <- cbind(frailty_parameters = frailty_parameters, acc_enet)
		acc_rf <- data.frame(results$acc_0[,2], results$acc_5[,2], results$acc_1[,2])
		colnames(acc_rf) <- c("0","0.5","1") 
		acc_rf <- cbind(frailty_parameters = frailty_parameters, acc_rf) 

		return(list(precision_enet, precision_rf, recall_enet, recall_rf, f1score_enet, f1score_rf, acc_enet, acc_rf))
	}
}


