synthetic_experiment_DO <- function(nsim, data, dataDO, expt){

	if (expt == "I"){

		#Synthetic Experiment I: Test the performance of the video features in predicting FI and TestAge
		results_FI <- create_storage_matrix(nsim, ncols = 4, col_names = c("Enet","SVM","RF","XGB"))
		results_Age <- create_storage_matrix(nsim, ncols = 4, col_names = c("Enet","SVM","RF","XGB"))

		#Predict FI
		yname <- "score"
		train_test <- train_test_data_DO(data, dataDO, yname = yname, type = "B6")
		traindata <- train_test$traindata
		testdata <- train_test$testdata

		models <- fit_models(traindata = traindata, yname = yname, expt = "I")
		res <- performance_measure(models, testdata = testdata, yname = yname, expt = "I")
		results_FI[["MAE"]][1,] <- res$MAE
		results_FI[["RMSE"]][1,] <- res$RMSE
		results_FI[["R2"]][1,] <- res$R2

		#Predict Age
		yname <- "TestAge"
		train_test <- train_test_data_DO(data, dataDO, yname = yname, type = "B6")
		traindata <- train_test$traindata
		testdata <- train_test$testdata

		models <- fit_models(traindata = traindata, yname = yname, expt = "I")
		res <- performance_measure(models, testdata = testdata, yname = yname, expt = "I")
		results_Age[["MAE"]][1,] <- res$MAE
		results_Age[["RMSE"]][1,] <- res$RMSE
		results_Age[["R2"]][1,] <- res$R2
	} else if (expt == "II"){

		#Synthetic Experiment II: Train an independent model for each diet group separately and test performance
		results_FI <- list()
		results_Age <- list()
		
		Diets <- unique(dfDO$Diet) 
		for (d in Diets){
			cat("Train and test on Diet group:", d, "\n")
			#Predict FI
			yname <- "score"
			traindata <- dfDO[dfDO$Diet %in% d, ]
			traindata$Diet <- droplevels(traindata$Diet)
			traindata <- traindata[, -which(names(traindata) %in% c("MouseID", "Diet", "AnimalStatus", "TestAge"))]
			res <- fit_models_DO(traindata = traindata, yname = yname, expt = "II") #LOOCV (results are contained in fit_models)
			results_FI[[paste0(d)]] <- res

			#Predict Age
			yname <- "TestAge"
			traindata <- dfDO[dfDO$Diet %in% d, ]
			traindata$Diet <- droplevels(traindata$Diet)
			traindata <- traindata[, -which(names(traindata) %in% c("MouseID", "Diet", "AnimalStatus", "score"))]
			res <- fit_models_DO(traindata = traindata, yname = yname, expt = "II") #LOOCV (results are contained in fit_models)
			results_Age[[paste0(d)]] <- res
		}
	}
	return(list(results_FI = results_FI, results_Age = results_Age))
}