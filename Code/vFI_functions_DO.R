read_data_DO <- function(data, dataDO, type){

	median_gait_measures_linear <- c("median_angular_velocity","median_base_tail_lateral_displacement","median_limb_duty_factor","median_nose_lateral_displacement","median_speed_cm_per_sec","median_step_length1","median_step_length2","median_step_width","median_stride_length","median_tip_tail_lateral_displacement")
	iqr_gait_measures_linear <- c("angular_velocity_iqr","base_tail_lateral_displacement_iqr","limb_duty_factor_iqr","nose_lateral_displacement_iqr","speed_cm_per_sec_iqr","step_length1_iqr","step_length2_iqr","step_width_iqr","stride_length_iqr","tip_tail_lateral_displacement_iqr")
	OFA_measures <- c("stride_count","Distance.cm.sc","center_time_secs","periphery_time_secs","corner_time_secs","center_distance_cm","periphery_distance_cm","corner_distance_cm","grooming_number_bouts","grooming_duration_secs")
	engineered_features_median <- c("dAC_median","dB_median","aABC_median","dAC_nongait_median","dB_nongait_median","aABC_nongait_median")
	engineered_features_stdev <- c("dAC_stdev","dB_stdev","aABC_stdev","dAC_nongait_stdev","dB_nongait_stdev","aABC_nongait_stdev")
	rearpaw_pose_measures <- c("median_rearpaw")
	rears_measures <- c("rear_count","rears_0_5")
	ellipsefit_measures <- c("median_width", "median_length")

	dataDO <- dataDO[, -which(names(dataDO) %in% c("NetworkFilename", "VideoDate", "DaysSurvival_vid","DOE", "FIdate", "Test.Difference"))]
	names(dataDO)[names(dataDO) == "CFI"] <- "score"
	names(dataDO)[names(dataDO) %in% c("AgeAtVid")] <- "TestAge"
	
	if (type == "B6"){
		dataDO <- dataDO[, which(names(dataDO) %in% c("MouseID", "Diet", "TestAge", "score", "AnimalStatus", names(data)))] 
	} else {
		dataDO <- dataDO
	}

	dataDO <- dataDO[complete.cases(dataDO),]
	dataDO$Diet <- as.factor(dataDO$Diet)
	dataDO$MouseID <- as.factor(dataDO$MouseID)
	dataDO$rear_count <- as.double(dataDO$rear_count)
	dataDO$rears_0_5 <- as.double(dataDO$rears_0_5)
	dataDO$grooming_number_bouts <- as.double(dataDO$grooming_number_bouts)
	dataDO$TestAge <- as.double(dataDO$TestAge)
	dataDO$stride_count <- as.double(dataDO$stride_count)
	return(dataDO)

}



train_test_data_DO <- function(data, dataDO, yname, type){

	#'''
	#Prepare data for training models and testing them. The test data is normalized using the traindata mean and sd. 
	#'''
	if (type == "B6"){
		df0 <- do.call(rbind, lapply(split(data, data$MouseID), function(x) x[sample(nrow(x), 1),]))
		train <- caret::createDataPartition(df0[,paste0(yname)], p = 1, list = FALSE)
		dfTrain <- data[data$MouseID %in% df0[train,'MouseID'],]
		if (yname == "score"){
			traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID","Diet","AnimalStatus","TestAge"))]
			} else {
			traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID","Diet","AnimalStatus","score"))]
			}
		traindata_mean <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], mean))
		traindata_sd <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], sd))
		traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))] <- scale(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], center = TRUE, scale = TRUE)

		testdata <- dataDO[,which(names(dataDO) %in% names(traindata))]
		testdata <- testdata[,order(names(testdata))]
		testdata <- testdata[complete.cases(testdata),]
		testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))] <- scale(testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))], center = traindata_mean, scale = traindata_sd)

	}

	rownames(traindata) <- 1:nrow(traindata)
	rownames(testdata) <- 1:nrow(testdata)

	return(list(traindata = traindata, testdata = testdata, traindata_mean = traindata_mean, traindata_sd = traindata_sd))
}

fit_models_DO <- function(traindata, yname, expt){
	#'''
	#Fit models based on seed, traindata, and the dependent variable yname
	#'''
	if (expt == "II"){
		Ytrain <- traindata[,paste0(yname)]
		Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname)))]

		enet <- caret::train(y = Ytrain, x = Xtrain, preProcess = c("center", "scale", "YeoJohnson"), method = "enet", trControl = trainControl(method = "LOOCV"), tuneLength = 10, metric = "Rsquared")
		enet <- enet$results[which.max(enet$results$Rsquared),3:5]
		svm <- caret::train(y=Ytrain, x=Xtrain, preProcess = c("center","scale","YeoJohnson"), method = "svmLinear",trControl = trainControl(method="LOOCV"), metric = "Rsquared")
		svm <- svm$results[which.max(svm$results$Rsquared),2:4]
		rf <- caret::train(x = Xtrain, y = Ytrain,  preProcess = c("center", "scale", "YeoJohnson"), method = "enet", trControl = trainControl(method = "LOOCV"), metric = "Rsquared")	
		rf <- rf$results[which.max(rf$results$Rsquared),3:5]
		xgb <- caret::train(y=Ytrain, x=Xtrain, preProcess = c("center","scale","YeoJohnson"), method='xgbTree', objective='reg:squarederror',trControl=trainControl(method="LOOCV"), verbosity = 0, metric = "Rsquared")
		xgb <- xgb$results[which.max(xgb$results$Rsquared),8:10]
	}
	return(list(enet = enet, svm = svm, rf = rf, xgb = xgb))
}