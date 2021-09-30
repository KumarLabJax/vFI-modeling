read_data <- function(data,summary_phenotypes,type){

	if(type == "frailty"){

		frailty_parameters <- c('Alopecia','Loss.of.fur.colour','Dermatitis','Loss.of.whiskers','Coat.condition','Piloerection','Cataracts','Eye.discharge.swelling','Microphthalmia','Corneal.opacity','Nasal.discharge','Rectal.prolapse','Vaginal.uterine.','Diarrhea','Vestibular.disturbance','Vision.loss..Visual.Placing.','Menace.reflex','Tail.stiffening','Gait.disorders','Tremor','Tumours','Distended.abdomen','Kyphosis','Body.condition','Breathing.rate.depth','Malocclusions','Righting.Reflex')
		names(data)[names(data) == "Age.at.Test"] <- "TestAge"
		df <- data[ ,names(data) %in% c("MouseID", "TestAge", "Tester", "Batch", "Sex", frailty_parameters)]
		return(df)


	} else {

		df <- data
		#df$Tester <- df$Collected.By
		#df$TestAge <- df$Age.at.Test
		avg_gait_measures_linear <- c("avg_angular_velocity","avg_base_tail_lateral_displacement",
		"avg_limb_duty_factor","avg_nose_lateral_displacement","avg_speed_cm_per_sec",
		"avg_step_length1","avg_step_length2","avg_step_width","avg_stride_length",
		"avg_tip_tail_lateral_displacement")
		median_gait_measures_linear <- c("median_angular_velocity","median_base_tail_lateral_displacement",
		"median_limb_duty_factor","median_nose_lateral_displacement","median_speed_cm_per_sec",
		"median_step_length1","median_step_length2","median_step_width","median_stride_length",
		"median_tip_tail_lateral_displacement")
		std_gait_measures_linear <- c("angular_velocity_stdev","base_tail_lateral_displacement_stdev",
		"limb_duty_factor_stdev","nose_lateral_displacement_stdev","speed_cm_per_sec_stdev",
		"step_length1_stdev","step_length2_stdev","step_width_stdev","stride_length_stdev",
		"tip_tail_lateral_displacement_stdev")
		iqr_gait_measures_linear <- c("angular_velocity_iqr","base_tail_lateral_displacement_iqr",
		"limb_duty_factor_iqr","nose_lateral_displacement_iqr","speed_cm_per_sec_iqr",
		"step_length1_iqr","step_length2_iqr","step_width_iqr","stride_length_iqr",
		"tip_tail_lateral_displacement_iqr")
		OFA_measures <- c("stride_count","Distance.cm.sc","center_time_secs","periphery_time_secs","corner_time_secs","center_distance_cm","periphery_distance_cm","corner_distance_cm","grooming_number_bouts","grooming_duration_secs")
		engineered_features_mean <- c("dAC_mean","dB_mean","aABC_mean","dAC_nongait_mean","dB_nongait_mean","aABC_nongait_mean")
		engineered_features_median <- c("dAC_median","dB_median","aABC_median","dAC_nongait_median","dB_nongait_median","aABC_nongait_median")
		engineered_features_stdev <- c("dAC_stdev","dB_stdev","aABC_stdev","dAC_nongait_stdev","dB_nongait_stdev","aABC_nongait_stdev")
		rearpaw_pose_measures <- c("median_rearpaw")
		rears_measures <- c("rear_count","rears_0_5","rears_0_10")
		ellipsefit_measures <- c("median_width", "median_length")

		if (summary_phenotypes == "mean"){

			df <- df[,names(df) %in% c("CFI","MouseID","TestAge","Tester","Batch",
			"Weight", "Sex",avg_gait_measures_linear, std_gait_measures_linear, iqr_gait_measures_linear,OFA_measures, engineered_features_mean, engineered_features_stdev, ellipsefit_measures, rears_measures, rearpaw_pose_measures)]


		} else if (summary_phenotypes == "median"){

			df <- df[,names(df) %in% c("CFI","MouseID","TestAge","Tester","Batch",
			"Weight", "Sex",median_gait_measures_linear, iqr_gait_measures_linear,OFA_measures,engineered_features_median, engineered_features_stdev, ellipsefit_measures, rears_measures, rearpaw_pose_measures)]

		} else {

			df <- df[,names(df) %in% c("CFI","MouseID","TestAge","Tester","Batch",
			"Weight", "Sex",avg_gait_measures_linear, median_gait_measures_linear, std_gait_measures_linear,iqr_gait_measures_linear,OFA_measures, engineered_features_mean,engineered_features_median, ellipsefit_measures, rears_measures, rearpaw_pose_measures)]

		}

		names(df)[names(df) == "CFI"] <- "score"
		df <- df[complete.cases(df),]
		df$MouseID <- as.factor(df$MouseID)
		df$rear_count <- as.double(df$rear_count)
		df$rears_0_5 <- as.double(df$rears_0_5)
		df$grooming_number_bouts <- as.double(df$grooming_number_bouts)
		df$TestAge <- as.double(df$TestAge)
		df$stride_count <- as.double(df$stride_count)
		df$Batch <- as.factor(df$Batch)
		df$Sex <- as.factor(df$Sex)
		df$Tester <- as.factor(df$Tester)
		df$score <- df$score/28

		return(df)
	}

}


preprocess_data <- function(df, type){

	#'''
	#The preprocess_data function adjusts frailty scores for tester effect in the raw dataset
	#'''

	if (type == "frailty"){

		df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		return(df)


	} else {
		mod.lmm <- lmer(score ~ TestAge + Weight + Sex + (1|Tester) + (1|Batch), data = df[!duplicated(df$MouseID),]) #mA
		mod.lmm1 <- update(mod.lmm, . ~ . - (1|Tester)) #m0
		m.tester <- update(mod.lmm, . ~ . - (1|Batch))
		RLRT <- c(RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$statistic,RLRT_stats <- RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$p.value)
		df$score <- ifelse(df$Tester == "Scorer1", df$score - ranef(mod.lmm)$Tester[1,],ifelse(df$Tester == "Scorer2", df$score - ranef(mod.lmm)$Tester[2,],
			ifelse(df$Tester == "Scorer3", df$score - ranef(mod.lmm)$Tester[3,], df$score - ranef(mod.lmm)$Tester[4,])))
		df$score <- ifelse(df$Batch == 'Batch1', df$score - ranef(mod.lmm)$Batch[1,], df$score - ranef(mod.lmm)$Batch[2,])
		df$score[df$score < 0] <- 0
		df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		df_tmp <- data.frame(Scorer = row.names(ranef(mod.lmm)$Tester), RE = ranef(mod.lmm)$Tester[,1]) 
		p <- ggplot(df_tmp, aes(x = Scorer, y = RE, fill = Scorer)) + geom_bar(stat = "identity", color = "black") + theme_bw(base_size = 16) + theme(legend.position = "none") + scale_fill_brewer(palette = "Set1") + xlab("Scorer") + ylab("Estimated random effects") 
		return(list(df = df, p = p, RLRT = RLRT))
	}

}




train_test_data <- function(df,yname,prop,seed,type){

	#'''
	#Prepare data for training models and testing them. The test data is normalized using the traindata mean and sd. 
	#'''
	set.seed(seed)
	frailty_parameters <- c('Alopecia','Loss.of.fur.colour','Dermatitis','Loss.of.whiskers','Coat.condition','Piloerection','Cataracts','Eye.discharge.swelling','Microphthalmia','Corneal.opacity','Nasal.discharge','Rectal.prolapse','Vaginal.uterine.','Diarrhea','Vestibular.disturbance','Vision.loss..Visual.Placing.','Menace.reflex','Tail.stiffening','Gait.disorders','Tremor','Tumours','Distended.abdomen','Kyphosis','Body.condition','Breathing.rate.depth','Malocclusions','Righting.Reflex')
	if (type == "frailty"){
		#nzv <- caret::nearZeroVar(df)
		df <- df[,-which(names(df) %in% c("Nasal.discharge","Rectal.prolapse","Vaginal.uterine.","Diarrhea"))]
		df0 <- do.call(rbind, lapply(split(df, df$MouseID), function(x) x[sample(nrow(x), 1),]))
		train <- caret::createDataPartition(df0[,paste0(yname)], p = prop, list = FALSE)
		test <- setdiff(1:nrow(df0),train)
		dfTrain <- df[df$MouseID %in% df0[train,'MouseID'],]
		dfTest <- df[df$MouseID %in% df0[test,'MouseID'],]
		traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID","score"))]
		testdata <- dfTest[,-which(names(dfTest) %in% c("MouseID","score"))]
		traindata_mean <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], mean))
		traindata_sd <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], sd))
		traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))] <- scale(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], center = TRUE, scale = TRUE)
		testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))] <- scale(testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))], center = traindata_mean, scale = traindata_sd)
	} else {

	if (yname == "score"){
		df0 <- do.call(rbind, lapply(split(df, df$MouseID), function(x) x[sample(nrow(x), 1),]))
		train <- caret::createDataPartition(df0[,paste0(yname)], p = prop, list = FALSE)
		test <- setdiff(1:nrow(df0),train)
		dfTrain <- df[df$MouseID %in% df0[train,'MouseID'],]
		dfTest <- df[df$MouseID %in% df0[test,'MouseID'],]
		traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID","TestAge"))]
		testdata <- dfTest[,-which(names(dfTest) %in% c("MouseID","TestAge"))]
		traindata_mean <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], mean))
		traindata_sd <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], sd))
		traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))] <- scale(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], center = TRUE, scale = TRUE)
		testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))] <- scale(testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))], center = traindata_mean, scale = traindata_sd)
	} else if (yname == "TestAge") { 
		df0 <- do.call(rbind, lapply(split(df, df$MouseID), function(x) x[sample(nrow(x), 1),]))
		train <- caret::createDataPartition(df0[,paste0(yname)], p = prop, list = FALSE)
		test <- setdiff(1:nrow(df0),train)
		dfTrain <- df[df$MouseID %in% df0[train,'MouseID'],]
		dfTest <- df[df$MouseID %in% df0[test,'MouseID'],]
		traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID","score"))]
		testdata <- dfTest[,-which(names(dfTest) %in% c("MouseID","score"))]
		traindata_mean <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], mean))
		traindata_sd <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], sd))
		traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))] <- scale(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], center = TRUE, scale = TRUE)
		testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))] <- scale(testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))], center = traindata_mean, scale = traindata_sd)
	} else if (yname %in% frailty_parameters) {
		df0 <- do.call(rbind, lapply(split(df, df$MouseID), function(x) x[sample(nrow(x), 1),]))
		train <- caret::createDataPartition(df0[,paste0(yname)], p = prop, list = FALSE)
		test <- setdiff(1:nrow(df0),train)
		dfTrain <- df[df$MouseID %in% df0[train,'MouseID'],]
		dfTest <- df[df$MouseID %in% df0[test,'MouseID'],]
		traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID","score","TestAge",setdiff(frailty_parameters,yname)))]
		testdata <- dfTest[,-which(names(dfTest) %in% c("MouseID","score","TestAge",setdiff(frailty_parameters,yname)))]
		traindata_mean <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], mean))
		traindata_sd <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], sd))
		traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))] <- scale(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], center = TRUE, scale = TRUE)
		testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))] <- scale(testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))], center = traindata_mean, scale = traindata_sd)	
	} else {
		yname <- "score"
		df0 <- do.call(rbind, lapply(split(df, df$MouseID), function(x) x[sample(nrow(x), 1),]))
		train <- caret::createDataPartition(df0[,paste0(yname)], p = prop, list = FALSE)
		test <- setdiff(1:nrow(df0),train)
		dfTrain <- df[df$MouseID %in% df0[train,'MouseID'],]
		dfTest <- df[df$MouseID %in% df0[test,'MouseID'],]
		dfTrain$Age <- dfTrain$TestAge
		dfTest$Age <- dfTest$TestAge
		traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID"))]
		testdata <- dfTest[,-which(names(dfTest) %in% c("MouseID"))]
		traindata_mean <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), c(paste0(yname),"Age"))], mean))
		traindata_sd <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), c(paste0(yname),"Age"))], sd))
		traindata[,setdiff(names(sapply(traindata,is.numeric)), c(paste0(yname),"Age"))] <- scale(traindata[,setdiff(names(sapply(traindata,is.numeric)), c(paste0(yname),"Age"))], center = TRUE, scale = TRUE)
		testdata[,setdiff(names(sapply(testdata,is.numeric)), c(paste0(yname),"Age"))] <- scale(testdata[,setdiff(names(sapply(testdata,is.numeric)), c(paste0(yname),"Age"))], center = traindata_mean, scale = traindata_sd)
	
	}
	}

	rownames(traindata) <- 1:nrow(traindata)
	rownames(testdata) <- 1:nrow(testdata)

	return(list(traindata = traindata, testdata = testdata, traindata_mean = traindata_mean, traindata_sd = traindata_sd))

}


train_test_young_old <- function(df,yname,prop,seed,young,thresh,num){

	set.seed(seed)
	if (young == TRUE){
		trIndex <- sample(which(df$TestAge < thresh), num)
		trIDs <- df[sample(which(df$TestAge < thresh), num), "MouseID"]
	} else {
		trIndex <- sample(which(df$TestAge > thresh), num)
		trIDs <- df[sample(which(df$TestAge > thresh), num), "MouseID"]
	}
	df0 <- do.call(rbind, lapply(split(df[!df$MouseID %in% trIDs,], df[!df$MouseID %in% trIDs,]$MouseID), function(x) x[sample(nrow(x), 1),]))
	train <- caret::createDataPartition(df0[,paste0(yname)], p = prop, list = FALSE)
	test <- setdiff(1:nrow(df0),train)
	dfTrain <- df[df$MouseID %in% c(trIDs,df0[train,'MouseID']),]
	dfTest <- df[df$MouseID %in% df0[test,'MouseID'],]
	traindata <- dfTrain[,-which(names(dfTrain) %in% c("MouseID"))]
	testdata <- dfTest[,-which(names(dfTest) %in% c("MouseID"))]
	traindata_mean <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], mean))
	traindata_sd <- as.numeric(sapply(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], sd))
	traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))] <- scale(traindata[,setdiff(names(sapply(traindata,is.numeric)), paste0(yname))], center = TRUE, scale = TRUE)
	testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))] <- scale(testdata[,setdiff(names(sapply(testdata,is.numeric)), paste0(yname))], center = traindata_mean, scale = traindata_sd)

	rownames(traindata) <- 1:nrow(traindata)
	rownames(testdata) <- 1:nrow(testdata)

	return(list(traindata = traindata, testdata = testdata, traindata_mean = traindata_mean, traindata_sd = traindata_sd))
}


fit_models <- function(traindata,yname,expt = ""){

	#'''
	#Fit models based on seed, traindata, and the dependent variable yname
	#'''

	if (expt == "II"){

		quantiles <- c(0.025,0.50,0.975)
		Ytrain <- traindata[,paste0(yname)]
		Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname), "TestAge","Age"))]
		Xtrain_age <- traindata[,-which(names(traindata) %in% c(paste0(yname),"Age"))]
		Xtrain_age <- as.data.frame(scale(Xtrain_age, scale = TRUE, center = TRUE))

		dfTrain <- data.frame(score = Ytrain, Xtrain)
		dfTrain_age <- data.frame(score = Ytrain, Xtrain_age)

		age_linear <- lm(score ~ TestAge, data = dfTrain_age)
		age_nonlinear <- mgcv::gam(score ~ s(TestAge), data = dfTrain_age)
		rf <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = "all",honesty=FALSE)	
		rf_age <- grf::regression_forest(Xtrain_age, Ytrain, tune.parameters = "all",honesty=FALSE)

		age_linear_Q1 <- quantreg::rq(score ~ TestAge, tau = quantiles[1], data = dfTrain_age)
		age_nonlinear_Q1 <- quantreg::nlrq(score ~ SSlogis(TestAge, Asym, mid, scal), data = dfTrain_age, tau = quantiles[1])
		rf_Q1 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[1], honesty=FALSE)
		rf_age_Q1 <- grf::quantile_forest(Xtrain_age, Ytrain, quantiles = quantiles[1], honesty=FALSE)

		age_linear_Q3 <- quantreg::rq(score ~ TestAge, tau = quantiles[1], data = dfTrain_age)
		age_nonlinear_Q3 <- quantreg::nlrq(score ~ SSlogis(TestAge, Asym, mid, scal), data = dfTrain_age, tau = quantiles[1])
		rf_Q3 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[1], honesty=FALSE)
		rf_age_Q3 <- grf::quantile_forest(Xtrain_age, Ytrain, quantiles = quantiles[1], honesty=FALSE)

		return(list(age_L = age_linear, age_G = age_nonlinear, rf = rf, rf_age = rf_age,
					age_L_Q1 = age_linear_Q1, age_G_Q1 = age_nonlinear_Q1, rf_Q1 = rf_Q1, rf_age_Q1 = rf_age_Q1,age_L_Q3 = age_linear_Q3, age_G_Q3 = age_nonlinear_Q3, rf_Q3 = rf_Q3, rf_age_Q3 = rf_age_Q3)) 

	} else if (expt == "III"){

		Ytrain <- traindata[,paste0(yname)]
		Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname)))]
		rf <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = "all",honesty=FALSE)
		return(list(rf = rf))

	} else if (expt == "I"){

		Ytrain <- traindata[,paste0(yname)]
		Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname)))]
	
		enet <- caret::train(y=Ytrain,x=Xtrain, preProcess = c("BoxCox"), method="enet",trControl=trainControl(method="repeatedcv",repeats=1),tuneLength=10)
		svm <- caret::train(y=Ytrain, x=Xtrain, preProcess = c("BoxCox"), method = "svmLinear",trControl = trainControl(method="repeatedcv",repeats=1))
		rf <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = "all",honesty=FALSE)	
		xgb <- caret::train(y=Ytrain, x=Xtrain, preProcess = c("BoxCox"), method='xgbTree', objective='reg:squarederror',trControl=trainControl(method='repeatedcv',repeats=1))

		return(list(enet = enet, svm = svm, rf = rf, xgb = xgb))
	} else if (expt == "IV") {

		Ytrain <- traindata[,paste0(yname)]
		Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname)))]

		enet <- caret::train(y=Ytrain,x=Xtrain, method="enet",trControl=trainControl(method="repeatedcv",repeats=1),tuneLength=10)
		svm <- caret::train(y=Ytrain, x=Xtrain, method = "svmLinear",trControl = trainControl(method="repeatedcv",repeats=1))
		rf <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = "all",honesty=FALSE)	
		xgb <- caret::train(y=Ytrain, x=Xtrain, method='xgbTree', objective='reg:squarederror',
				trControl=trainControl(method='repeatedcv',repeats=1))

		return(list(enet = enet, svm = svm, rf = rf, xgb = xgb))

	} else {

		traindata[,yname] <- as.factor(traindata[,yname])
		Ytrain <- traindata[,paste0(yname)]
		Xtrain <- as.matrix(traindata[,-which(names(traindata) %in% c(paste0(yname)))])

		enet <- ordinalNet::ordinalNet(Xtrain, Ytrain, family = "cumulative", link = "logit", parallelTerms = TRUE, nonparallelTerms = FALSE)
		rf <- ordinalForest::ordfor(depvar = yname, data=traindata, nsets = 1000, nbest=5, ntreeperdiv=100, ntreefinal=1000)

		return(list(enet = enet, rf = rf))

	}


	
}


performance_measure <- function(models,yname,testdata, expt = ""){

	if (expt == "II"){

		quantiles <- c(0.025,0.50,0.975)
		Ytest <- testdata[,paste0(yname)]
		Xtest <- testdata[,-which(names(testdata) %in% c(paste0(yname),"TestAge","Age"))]
		Xtest_age <- testdata[,-which(names(testdata) %in% c(paste0(yname),"Age"))]

		young_ids <- which(testdata$Age < 50)
		middle_ids <- which(50 <= testdata$Age & testdata$Age <= 100)
		old_ids <- which(testdata$Age > 100)

		age_linear_hat <- as.numeric(predict(models[["age_L"]], newdata = data.frame(TestAge = Xtest_age$TestAge)))
		age_nonlinear_hat <- as.numeric(predict(models[["age_G"]], newdata = data.frame(TestAge = Xtest_age$TestAge)))
		rfhat <- predict(models[["rf"]],Xtest)$predictions
		rf_age_hat <- predict(models[["rf_age"]],Xtest_age)$predictions

		age_linear_hat_Q1 <- predict(models[["age_L_Q1"]], newdata = data.frame(TestAge = Xtest_age$TestAge))
		age_nonlinear_hat_Q1 <- predict(models[["age_G_Q1"]], newdata = data.frame(TestAge = Xtest_age$TestAge))
		rfhat_Q1 <- predict(models[["rf_Q1"]],Xtest)$predictions
		rf_age_hat_Q1 <- predict(models[["rf_age_Q1"]],Xtest_age)$predictions

		age_linear_hat_Q3 <- predict(models[["age_L_Q3"]], newdata = data.frame(TestAge = Xtest_age$TestAge))
		age_nonlinear_hat_Q3 <- predict(models[["age_G_Q3"]], newdata = data.frame(TestAge = Xtest_age$TestAge))
		rfhat_Q3 <- predict(models[["rf_Q3"]],Xtest)$predictions
		rf_age_hat_Q3 <- predict(models[["rf_age_Q3"]],Xtest_age)$predictions

		mae <- c(mean(abs(Ytest-age_linear_hat)),mean(abs(Ytest-age_nonlinear_hat)),mean(abs(Ytest-rfhat)),mean(abs(Ytest-rf_age_hat)))
		rmse <- c(sqrt(mean((Ytest-age_linear_hat)^2)),sqrt(mean((Ytest-age_nonlinear_hat)^2)),sqrt(mean((Ytest-rfhat)^2)),sqrt(mean((Ytest-rf_age_hat)^2)))
		R2 <- c(cor(Ytest,age_linear_hat)^2,cor(Ytest,age_nonlinear_hat)^2,cor(Ytest,rfhat)^2,cor(Ytest,rf_age_hat)^2)

		mae_Q1 <- c(mean(abs(Ytest-age_linear_hat_Q1)),mean(abs(Ytest-age_nonlinear_hat_Q1)),mean(abs(Ytest-rfhat_Q1)),mean(abs(Ytest-rf_age_hat_Q1)))
		rmse_Q1 <- c(sqrt(mean((Ytest-age_linear_hat_Q1)^2)),sqrt(mean((Ytest-age_nonlinear_hat_Q1)^2)),sqrt(mean((Ytest-rfhat_Q1)^2)),sqrt(mean((Ytest-rf_age_hat_Q1)^2)))
		R2_Q1 <- c(cor(Ytest,age_linear_hat_Q1)^2,cor(Ytest,age_nonlinear_hat_Q1)^2,cor(Ytest,rfhat_Q1)^2,cor(Ytest,rf_age_hat_Q1)^2)

		mae_Q3 <- c(mean(abs(Ytest-age_linear_hat_Q3)),mean(abs(Ytest-age_nonlinear_hat_Q3)),mean(abs(Ytest-rfhat_Q3)),mean(abs(Ytest-rf_age_hat_Q3)))
		rmse_Q3 <- c(sqrt(mean((Ytest-age_linear_hat_Q3)^2)),sqrt(mean((Ytest-age_nonlinear_hat_Q3)^2)),sqrt(mean((Ytest-rfhat_Q3)^2)),sqrt(mean((Ytest-rf_age_hat_Q3)^2)))
		R2_Q3 <- c(cor(Ytest,age_linear_hat_Q3)^2,cor(Ytest,age_nonlinear_hat_Q3)^2,cor(Ytest,rfhat_Q3)^2,cor(Ytest,rf_age_hat_Q3)^2)

		mae_young <- c(mean(abs(Ytest[young_ids]-age_linear_hat[young_ids])),mean(abs(Ytest[young_ids]-age_nonlinear_hat[young_ids])),mean(abs(Ytest[young_ids]-rfhat[young_ids])),mean(abs(Ytest[young_ids]-rf_age_hat[young_ids])))
		rmse_young <- c(sqrt(mean((Ytest[young_ids]-age_linear_hat[young_ids])^2)),sqrt(mean((Ytest[young_ids]-age_nonlinear_hat[young_ids])^2)),sqrt(mean((Ytest[young_ids]-rfhat[young_ids])^2)),sqrt(mean((Ytest[young_ids]-rf_age_hat[young_ids])^2)))
		R2_young <- c(cor(Ytest[young_ids],age_linear_hat[young_ids])^2,cor(Ytest[young_ids],age_nonlinear_hat[young_ids])^2,cor(Ytest[young_ids],rfhat[young_ids])^2,cor(Ytest[young_ids],rf_age_hat[young_ids])^2)

		mae_middle <- c(mean(abs(Ytest[middle_ids]-age_linear_hat[middle_ids])),mean(abs(Ytest[middle_ids]-age_nonlinear_hat[middle_ids])),mean(abs(Ytest[middle_ids]-rfhat[middle_ids])),mean(abs(Ytest[middle_ids]-rf_age_hat[middle_ids])))
		rmse_middle <- c(sqrt(mean((Ytest[middle_ids]-age_linear_hat[middle_ids])^2)),sqrt(mean((Ytest[middle_ids]-age_nonlinear_hat[middle_ids])^2)),sqrt(mean((Ytest[middle_ids]-rfhat[middle_ids])^2)),sqrt(mean((Ytest[middle_ids]-rf_age_hat[middle_ids])^2)))
		R2_middle <- c(cor(Ytest[middle_ids],age_linear_hat[middle_ids])^2,cor(Ytest[middle_ids],age_nonlinear_hat[middle_ids])^2,cor(Ytest[middle_ids],rfhat[middle_ids])^2,cor(Ytest[middle_ids],rf_age_hat[middle_ids])^2)

		mae_old <- c(mean(abs(Ytest[old_ids]-age_linear_hat[old_ids])),mean(abs(Ytest[old_ids]-age_nonlinear_hat[old_ids])),mean(abs(Ytest[old_ids]-rfhat[old_ids])),mean(abs(Ytest[old_ids]-rf_age_hat[old_ids])))
		rmse_old <- c(sqrt(mean((Ytest[old_ids]-age_linear_hat[old_ids])^2)),sqrt(mean((Ytest[old_ids]-age_nonlinear_hat[old_ids])^2)),sqrt(mean((Ytest[old_ids]-rfhat[old_ids])^2)),sqrt(mean((Ytest[old_ids]-rf_age_hat[old_ids])^2)))
		R2_old <- c(cor(Ytest[old_ids],age_linear_hat[old_ids])^2,cor(Ytest[old_ids],age_nonlinear_hat[old_ids])^2,cor(Ytest[old_ids],rfhat[old_ids])^2,cor(Ytest[old_ids],rf_age_hat[old_ids])^2)


		return(list(MAE = mae, RMSE = rmse, R2 = R2, MAE_Q1 = mae_Q1, RMSE_Q1 = rmse_Q1, R2_Q1 = R2_Q1,
			MAE_Q3 = mae_Q3, RMSE_Q3 = rmse_Q3, R2_Q3 = R2_Q3, MAE_young = mae_young, RMSE_young = rmse_young, R2_young = R2_young, MAE_middle = mae_middle, RMSE_middle = rmse_middle, R2_middle = R2_middle,MAE_old = mae_old, RMSE_old = rmse_old, R2_old = R2_old))

	} else if (expt == "III") {

		Xtest <- testdata[,-which(names(testdata) %in% c(paste0(yname)))]
		Ytest <- testdata[,paste0(yname)]
		rfhat <- predict(models[["rf"]],Xtest)$predictions

		return(list(MAE = mean(abs(Ytest-rfhat)), RMSE = sqrt(mean((Ytest-rfhat)^2)), R2 = cor(Ytest,rfhat)^2))

	} else if (expt == "I") {

		Xtest <- testdata[,-which(names(testdata) %in% c(paste0(yname)))]
		Ytest <- testdata[,paste0(yname)]

		enethat <- predict(models[["enet"]],newdata = Xtest)
		svmhat <- predict(models[["svm"]], newdata = Xtest)
		rfhat <- predict(models[["rf"]],Xtest)$predictions
		xgbhat <- predict(models[["xgb"]],newdata=Xtest)

		mae <- c(mean(abs(Ytest-enethat)),mean(abs(Ytest-svmhat)),mean(abs(Ytest-rfhat)),mean(abs(Ytest-xgbhat)))
		rmse <- c(sqrt(mean((Ytest-enethat)^2)),sqrt(mean((Ytest-svmhat)^2)),sqrt(mean((Ytest-rfhat)^2)),sqrt(mean((Ytest-xgbhat)^2)))
		R2 <- c(cor(Ytest,enethat)^2,cor(Ytest,svmhat)^2,cor(Ytest,rfhat)^2,cor(Ytest,xgbhat)^2)
	
		return(list(MAE = mae, RMSE = rmse, R2 = R2))

	} else if (expt == "IV") {

		Xtest <- testdata[,-which(names(testdata) %in% c(paste0(yname)))]
		Ytest <- testdata[,paste0(yname)]

		enethat <- predict(models[["enet"]],newdata = Xtest)
		svmhat <- predict(models[["svm"]], newdata = Xtest)
		rfhat <- predict(models[["rf"]],Xtest)$predictions
		xgbhat <- predict(models[["xgb"]],newdata=Xtest)

		mae <- c(mean(abs(Ytest-enethat)),mean(abs(Ytest-svmhat)),mean(abs(Ytest-rfhat)),mean(abs(Ytest-xgbhat)))
		rmse <- c(sqrt(mean((Ytest-enethat)^2)),sqrt(mean((Ytest-svmhat)^2)),sqrt(mean((Ytest-rfhat)^2)),sqrt(mean((Ytest-xgbhat)^2)))
		R2 <- c(cor(Ytest,enethat)^2,cor(Ytest,svmhat)^2,cor(Ytest,rfhat)^2,cor(Ytest,xgbhat)^2)
	
		return(list(MAE = mae, RMSE = rmse, R2 = R2))


	} else {

		testdata[,yname] <- as.factor(testdata[,yname])
		Xtest <- as.matrix(testdata[,-which(names(testdata) %in% c(paste0(yname)))])
		Ytest <- testdata[,paste0(yname)]
		levels(Ytest) <- c("0","0.5","1")

		enethat <- predict(models[["enet"]],newx = Xtest, type = "class")
		enethat <- as.factor(ifelse(enethat == 1, 0, ifelse(enethat == 2, 0.5, 1)))
		levels(enethat) <- c("0","0.5","1")
		rfhat <- predict(models[["rf"]], newdata = Xtest)$ypred
		levels(rfhat) <- c("0","0.5","1")

		enet_cm <- confusionMatrix(data = enethat, reference = Ytest)$byClass
		rf_cm <- confusionMatrix(data = rfhat, reference = Ytest)$byClass

		
		precision <- data.frame(enet = confusionMatrix(data = enethat, reference = Ytest)$byClass[,"Precision"], rf = confusionMatrix(data = rfhat, reference = Ytest)$byClass[,"Precision"])
		recall <- data.frame(enet = confusionMatrix(data = enethat, reference = Ytest)$byClass[,"Recall"], rf = confusionMatrix(data = rfhat, reference = Ytest)$byClass[,"Recall"])
		f1score <- data.frame(enet = confusionMatrix(data = enethat, reference = Ytest)$byClass[,"F1"], rf = confusionMatrix(data = rfhat, reference = Ytest)$byClass[,"F1"])
		acc <- data.frame(enet = confusionMatrix(data = enethat, reference = Ytest)$byClass[,"Balanced Accuracy"], rf = confusionMatrix(data = rfhat, reference = Ytest)$byClass[,"Balanced Accuracy"])
			
	
		return(list(precision = precision, recall = recall, f1score = f1score, acc = acc))

	}
	
}

plot_results <- function(mae, rmse, r2, precision, recall, f1score, acc, yname, expt = ""){

	if (expt == "I"){
        tmp1 <- read.csv(paste0(mae),header=TRUE)
        tmp2 <- read.csv(paste0(rmse),header=TRUE)
        tmp3 <- read.csv(paste0(r2),header=TRUE)

        MAE.melt <- reshape::melt(tmp1)
        RMSE.melt <- reshape::melt(tmp2)
        R2.melt <- reshape::melt(tmp3)

        df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
        df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=200)
        df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
        levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
        levels(df.metrics$variable) <- c('LR*','SVM','RF','XGB')

        if (yname == "Age"){

            p <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(fill = '#e41a1c', alpha = 0.7) + geom_point(alpha = 0.5) + labs(x='Model',y='Metric (weeks)') + theme_bw(base_size=28) + 
                 facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=19), axis.text.y = element_text(size=21), axis.title.x = element_text(size = 21), 
                 axis.title.y=element_text(size=21))

        } else {

            p <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(fill = '#377eb8', alpha = 0.7) + geom_point(alpha = 0.5) + labs(x='Model',y='Metric') + theme_bw(base_size=28) + facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=19), 
                axis.text.y = element_text(size=21), axis.title.x = element_text(size = 21), axis.title.y=element_text(size=21))
        }

        return(p)

		
	} else if (expt == "II"){

		tmp1 <- read.csv(paste0(mae),header=TRUE)
        tmp2 <- read.csv(paste0(rmse),header=TRUE)
        tmp3 <- read.csv(paste0(r2),header=TRUE)

        MAE.melt <- reshape::melt(tmp1)
        RMSE.melt <- reshape::melt(tmp2)
        R2.melt <- reshape::melt(tmp3)

        df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
        df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=200)
        df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
        levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
        levels(df.metrics$variable) <- c('Age_L','Age_G','Video_RF','All_RF')

        p <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(alpha = 0.7, fill = '#377eb8') + geom_point(alpha = 0.5) + labs(x='Features_Model',y='Metric') + theme_bw(base_size=28) + 
                 facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=21, angle = 90, hjust = 1, vjust = 0.5), axis.text.y = element_text(size=21), axis.title.x = element_text(size = 21), 
                 axis.title.y=element_text(size=21))

        return(p)


	} else if (expt == "III"){

		tmp1 <- read.csv(paste0(mae),header=TRUE)
		tmp2 <- read.csv(paste0(rmse),header=TRUE)
		tmp3 <- read.csv(paste0(r2),header=TRUE)

		MAE.melt <- reshape::melt(tmp1)
		RMSE.melt <- reshape::melt(tmp2)
		R2.melt <- reshape::melt(tmp3)

		df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
		df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=350)
		df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
		levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
		levels(df.metrics$variable) <- c("0.6","0.65","0.7","0.75","0.8","0.85","0.9")

		if (yname == "Age"){

			p <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(fill = '#e41a1c', alpha = 0.7) + geom_point(alpha = 0.5) + labs(x='Train %',y='Metric (weeks)') + theme_bw(base_size=28) + facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=21,angle = 90,hjust = 1, vjust = 0.6), axis.text.y = element_text(size=21), axis.title.x = element_text(size = 21), axis.title.y=element_text(size=21))

		} else {

			p <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(fill = '#377eb8', alpha = 0.7) + geom_point(alpha = 0.5) + labs(x='Train %',y='Metric') + theme_bw(base_size=28) + facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=21,angle = 90,hjust = 1, vjust = 0.6), axis.text.y = element_text(size=21), axis.title.x = element_text(size = 21), axis.title.y=element_text(size=21))
		}


		return(p)

	}

	else if (expt == "IV") {

		tmp1 <- read.csv(paste0(mae[1]),header=TRUE)
        tmp2 <- read.csv(paste0(rmse[1]),header=TRUE)
        tmp3 <- read.csv(paste0(r2[1]),header=TRUE)

     	tmp1v <- read.csv(paste0(mae[2]),header=TRUE)
        tmp2v <- read.csv(paste0(rmse[2]),header=TRUE)
        tmp3v <- read.csv(paste0(r2[2]),header=TRUE)


        df_mae <- data.frame(Frailty = tmp1[,3],Video = tmp1v[,3])
        df_rmse <- data.frame(Frailty = tmp2[,3],Video = tmp2v[,3])
        df_r2 <- data.frame(Frailty = tmp3[,3],Video = tmp3v[,3])
        
        MAE.melt <- reshape::melt(df_mae)
        RMSE.melt <- reshape::melt(df_rmse)
        R2.melt <- reshape::melt(df_r2)

        df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
        df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=100)
        df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
        levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')

        p <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(fill = '#e41a1c', alpha = 0.7, width = 0.35) + geom_point(alpha = 0.5) + labs(x='Features',y='Metric (weeks)') + theme_bw(base_size=28) + facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=21), axis.text.y = element_text(size=21), axis.title.x = element_text(size = 21), axis.title.y=element_text(size=21))

        return(p)


	} else {
		frailty_parameters <- c("Alopecia", "Body.condition","Coat.condition","Distended.abdomen","Gait.disorders","Khyphosis","Loss.of.fur.colour","Loss.of.whiskers","Piloerection")
		
		precision <- read.csv(paste0(precision),header=TRUE)
		tmp <- reshape2::melt(precision, id = "frailty_parameters")
		p1 <- ggplot(tmp[1:nrow(tmp)*0.5,], aes(x = frailty_parameters, y = value, fill = variable)) + geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2",labels = c("0","0.5","1")) + theme_bw(base_size = 18) + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(), legend.position = "none") + labs(x = NULL, y = "Precision") 
		
		recall <- read.csv(paste0(recall),header=TRUE)
		tmp <- reshape2::melt(recall, id = "frailty_parameters")
		p2 <- ggplot(tmp[1:nrow(tmp)*0.5,], aes(x = frailty_parameters, y = value, fill = variable)) + geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2",labels = c("0","0.5","1")) + theme_bw(base_size = 18) + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) + labs(x = NULL, y = "Recall") + guides(fill = guide_legend(title = "Value"))
		
		f1score <- read.csv(paste0(f1score),header=TRUE)
		tmp <- reshape2::melt(f1score, id = "frailty_parameters")
		p3 <- ggplot(tmp[1:nrow(tmp)*0.5,], aes(x = frailty_parameters, y = value, fill = variable)) + geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2",labels = c("0","0.5","1")) + theme_bw(base_size = 18) + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(), legend.position = "none") + labs(x = NULL, y = "F1 score") 
		acc <- read.csv(paste0(acc),header=TRUE)
		tmp <- reshape2::melt(acc, id = "frailty_parameters")
		p4 <- ggplot(tmp[1:nrow(tmp)*0.5,], aes(x = frailty_parameters, y = value, fill = variable)) + geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2",labels = c("0","0.5","1")) + theme_bw(base_size = 18) + theme(axis.text.x = element_text(angle = 90, size =16, vjust=0.4,hjust=1.01), legend.position = "none") + labs(x = "Frailty parameters", y = "Accuracy") 

		return(list(p1,p2,p3,p4))
	}


}

interpretable_ML_plot <- function(yname,mae,type,phenotype,phenoname,phenotypes,phenonames,frailty = FALSE){

	if (frailty == TRUE){

		frailty_parameters_red <- c('Alopecia','Loss.of.fur.colour','Dermatitis','Loss.of.whiskers','Coat.condition','Piloerection','Cataracts','Eye.discharge.swelling','Microphthalmia','Corneal.opacity','Vestibular.disturbance','Vision.loss..Visual.Placing.','Menace.reflex','Tail.stiffening','Gait.disorders','Tremor','Tumours','Distended.abdomen','Kyphosis','Body.condition','Breathing.rate.depth','Malocclusions','Righting.Reflex')

		tmp1 <- read.csv(paste0(mae[1]))
		ind <- with(tmp1, which.min(RF != quantile(RF, .5, type = 1))[1])

		train_test <- train_test_data(data_f, yname = yname, prop = 0.80, seed = sim, type = "frailty")
		traindata <- train_test$traindata
		testdata <- train_test$testdata

		Ytrain <- traindata[,paste0(yname)]
		Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname)))]

		model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = 0.5, honesty=FALSE)
		vi_frailty <- grf::variable_importance(model)
		df_fi <- data.frame(Feature = frailty_parameters_red, Importance = vi_frailty)

		p <- ggplot(df_fi, aes(x = Feature, y = Importance)) + geom_bar(stat='identity',position='dodge') +  theme_bw(base_size=18) + labs(x = 'Feature', y = 'Importance') + theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1)) + theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18))

		

		return(p)

	} else {

	Features <- c('Center Time','Periphery Time','Corner Time','Center Distance','Periphery Distance','Corner Distance','Grooming Bouts','Grooming Duration','Angular Velocity','Base Tail LD','Limb Duty Factor','Nose LD','Speed','Step Length1','Step Length2','Step Width','Stride Length','Tip Tail LD','Stride Count','Distance','Angular Velocity IQR','Base Tail LD IQR','Limb Duty Factor IQR','Nose LD IQR','Speed IQR','Step Length1 IQR','Step Length2 IQR','Step Width IQR','Stride Length IQR','Tip Tail LD IQR','dAC','dB','aABC','Width','Length','Rearpaw','Rearcount','Rears_0_5','dAC_nongait','dAC_nongait_stdev','dB_nongait','dB_nongait_stdev','aABC_nongait','aABC_nongait_stdev')

	tmp1 <- read.csv(paste0(mae),header=TRUE)
	ind <- with(tmp1, which.min(RF != quantile(RF, .5, type = 1))[1])

	train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = ind, type = "")
	traindata <- train_test$traindata
	testdata <- train_test$testdata
	traindata_mean <- train_test$traindata_mean
	traindata_sd <- train_test$traindata_sd

	Ytrain <- traindata[,paste0(yname)]
	Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname)))]
	
	quantiles <- c(0.025,0.50,0.975)

	if (type == 'feature_importance'){

		vi_list <- list()
		modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[1], honesty=FALSE)
		vi_list[[1]] <- grf::variable_importance(modelGRF60)
		modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[2], honesty=FALSE)
		vi_list[[2]] <- grf::variable_importance(modelGRF60)
		modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[3], honesty=FALSE)
		vi_list[[3]] <- grf::variable_importance(modelGRF60)
		df.tmp <- data.frame(importance = do.call(rbind,vi_list)) 
		df.tmp$feature <- rep(Features,3)
		df.tmp$Quantile <- factor(rep(c('Q025','M','Q975'), each=44)) 
		df.tmp$Quantile <- factor(df.tmp$Quantile, levels = c('Q025','M','Q975'))

		p <- ggplot(df.tmp, aes(x=feature,y=importance,fill=Quantile)) + geom_bar(stat='identity',position='dodge') +  theme_bw(base_size=18) + labs(x = 'Feature', y = 'Importance') + scale_fill_manual(name = "Quantile", values = c('#4daf4a','#377eb8','#e41a1c'),labels = expression(Q[1], M, Q[3])) + theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1)) + theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18))

		return(p)

	} else if (type == 'marginal'){

		ff <- which(names(Xtrain) == paste0(phenotype))
		mn <- traindata_mean[ff]
		sd <- traindata_sd[ff]
		k <- 50 #A numeric scalar that specifies the number of intervals into which the predictor range is divided when calculating the ALE plot effects.

		modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, honesty=FALSE)
		modelRF60 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
		Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata,type='raw')$predictions)
		ALERF <- ALEPlot::ALEPlot(Xtrain,modelRF60,pred.fun=Yhat,J=ff,K=k)
		ALEGRF <- lapply(seq(quantiles), function(x) {Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions[,x]);
		ALEPlot::ALEPlot(Xtrain,modelGRF60,pred.fun=Yhat,J=ff,K=k)$f.values})
		df.ALE60 <- data.frame(x = mn + sd*ALERF$x.values)
		df.ALE60 <- cbind(df.ALE60, do.call(cbind,ALEGRF))
		colnames(df.ALE60) <- c('x','Q025','Median','Q975')

		df.ALE.melt60 <- invisible(reshape::melt(df.ALE60[,-1]))
		df.ALE.melt60$x <- rep(df.ALE60$x,ncol(df.ALE60[,-1]))
		df.ALE.melt60$variable <- factor(df.ALE.melt60$variable, levels=c("Q025","Median","Q975"))

		p <- ggplot(df.ALE.melt60, aes(x = x, y = value, col = variable)) + geom_point() + geom_line() + theme_bw(base_size=18) + scale_color_manual(name = "Quantile", values = c('#4daf4a','#377eb8','#e41a1c'),labels = expression(Q[1], M, Q[3])) + labs(x = paste0(phenoname), y = 'ALE') + theme(legend.position='none') + theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18))


		return(p)

	} else if (type == "interaction"){

		modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[2], honesty=FALSE)
		Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
		predictor <- iml::Predictor$new(model = modelGRF60, data = as.data.frame(Xtrain), y = Ytrain, predict.fun = Yhat)
		interact <- iml::Interaction$new(predictor)
		tmp <- data.frame(interact$results)
		colnames(tmp) <- c('Feature','Interaction')
		tmp$Feature <- as.factor(Features) 
		tmp <- tmp[with(tmp, order(Interaction)),]
		tmp$Feature <- factor(tmp$Feature, levels = tmp$Feature)
	
		p <- ggplot(tmp, aes(y = Feature, x = Interaction)) + geom_point(color = '#377eb8') + theme_bw(base_size=18) + geom_segment(aes(yend = Feature, x = 0, xend = Interaction),color='#377eb8') + scale_x_continuous("Overall interaction strength") + scale_y_discrete("Features") + theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16), axis.title.x = element_text(size = 16), axis.title.y=element_text(size=16))

		return(p)

	} else {

		phenotypes <- phenotypes
		phenonames <- phenonames
		require(viridis)
		tmp <- sapply(seq(44), function(x) (traindata_sd[x]*Xtrain[,x]) + traindata_mean[x])
		colnames(tmp) <- colnames(Xtrain)
		Xtrain <- tmp
		modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[2], honesty=FALSE)
		pred.fi <- iml::Predictor$new(modelGRF60, data = as.data.frame(Xtrain), y = Ytrain)
		p <- suppressMessages(iml::FeatureEffect$new(pred.fi, feature = phenotypes, method = "ale", grid.size = 50)$plot() + theme_bw(base_size=18) + scale_fill_gradient("ALE", low = "red", high = "yellow") + scale_x_continuous(phenonames[1]) + scale_y_continuous(phenonames[2]) + scale_fill_viridis(option = "D") + theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18)))
		return(p)


	}
	}

}

plot_pred_int <- function(mae, yname, type){

	if (yname == "TestAge"){

		tmp1 <- read.csv(paste0(mae),header=TRUE)
		ind <- with(tmp1, which.min(RF != quantile(RF, .5, type = 1))[1])

		quantiles <- c(0.025,0.5,0.975)
		train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = ind, type = "")
		traindata <- train_test$traindata
		testdata <- train_test$testdata
		
		output <- rfinterval::rfinterval(TestAge ~., train_data = traindata, test_data = testdata, method = c("oob","quantreg"), symmetry = TRUE, alpha = 0.1)
		df_tmp <- data.frame(True = testdata$TestAge, Pred = output$testPred, oob = output$oob, quant_reg = output$quantreg_interval)


		if (type == "oob"){

			p <- ggplot(df_tmp, aes(x = seq(nrow(df_tmp)), y = Pred, ymin=oob.lower, ymax=oob.upper)) + geom_point(aes(y=True), size = 1.5, color='#e41a1c') + geom_point(color = '#969696') + geom_pointrange(color = '#969696',alpha = 0.8) + theme_bw(base_size=16) + labs(x='Test Set Index',y='Age (weeks)') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))
		} else {

			p <- ggplot(df_tmp, aes(x = seq(nrow(df_tmp)), y = Pred, ymin=quant_reg.lower, ymax=quant_reg.upper)) + geom_point(aes(y=True), size = 1.5, color='#e41a1c') + geom_point(color = '#969696') + geom_pointrange(color = '#969696',alpha = 0.8) + theme_bw(base_size=16) + labs(x='Test Set Index',y='Age (weeks)') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))
		}
		

		return(p)

	} else {

		tmp1 <- read.csv(paste0(mae),header=TRUE)
		ind <- with(tmp1, which.min(RF != quantile(RF, .5, type = 1))[1])

		quantiles <- c(0.025,0.5,0.975)
		train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = ind, type = "")
		traindata <- train_test$traindata
		testdata <- train_test$testdata
		
		output <- rfinterval::rfinterval(score ~., train_data = traindata, test_data = testdata, method = c("oob","quantreg"), symmetry = TRUE, alpha = 0.1)
		df_tmp <- data.frame(True = testdata$score, Pred = output$testPred, oob = output$oob, quant_reg = output$quantreg_interval) 

		if (type == "oob"){

			p <- ggplot(df_tmp, aes(x = seq(nrow(df_tmp)), y = Pred, ymin=oob.lower, ymax=oob.upper)) + geom_point(aes(y=True), size = 1.5, color='#377eb8') + geom_point(color = '#969696') + geom_pointrange(color = '#969696',alpha = 0.8) + theme_bw(base_size=16) + labs(x='Test Set Index',y='vFI') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))
		} else {

			p <- ggplot(df_tmp, aes(x = seq(nrow(df_tmp)), y = Pred, ymin=quant_reg.lower, ymax=quant_reg.upper)) + geom_point(aes(y=True), size = 1.5, color='#377eb8') + geom_point(color = '#969696') + geom_pointrange(color = '#969696',alpha = 0.8) + theme_bw(base_size=16) + labs(x='Test Set Index',y='vFI') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))
		}

		return(p)

	}	
}

predint_width <- function(mae,yname){

	if (yname == "TestAge"){

		tmp1 <- read.csv(paste0(mae),header=TRUE)
		ind <- with(tmp1, which.min(RF != quantile(RF, .5, type = 1))[1])

		quantiles <- c(0.025,0.5,0.975)
		train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = ind, type = "")
		traindata <- train_test$traindata
		testdata <- train_test$testdata

		output <- rfinterval::rfinterval(TestAge ~., train_data = traindata, test_data = testdata, method = c("quantreg"), symmetry = TRUE, alpha = 0.1)
		df_tmp <- data.frame(Age = testdata$TestAge, Pred = output$testPred, quant_reg = output$quantreg_interval)
		df_tmp$width <- df_tmp$quant_reg.upper - df_tmp$quant_reg.lower
		df_tmp$Group <- ifelse(df_tmp$Age < as.numeric(summary(df_tmp$Age)[2]), 'Q1', ifelse(df_tmp$Age > as.numeric(summary(df_tmp$Age)[3]), 'Q3', 'M'))
		

		C1 <- ggplot(df_tmp,aes(x=Age,y=width)) + geom_point(color = '#e41a1c',size=3,alpha=0.8) + stat_smooth(method='loess',color="#000000") + labs(x = 'Age (weeks)', y = 'PI width (Age)') + theme_bw(base_size=22) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

		C2 <- ggplot(df_tmp,aes(x = width, color = Group)) + geom_density(lwd=1.2) + geom_rug() + scale_color_manual(values=c('#006d2c','#e7298a','#525252')) + theme_bw(base_size=22) + labs(x='PI width', y='Density') + theme(legend.position='top') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20)) 

		p <- C1|C2
		return(p)

	} else {

		tmp1 <- read.csv(paste0(mae),header=TRUE)
		ind <- with(tmp1, which.min(RF != quantile(RF, .5, type = 1))[1])

		quantiles <- c(0.025,0.5,0.975)
		train_test <- train_test_data(df, yname = "", prop = 0.80, seed = ind, type = "")
		traindata <- train_test$traindata
		testdata <- train_test$testdata
		Age <- testdata$Age

		traindata <- traindata[,-which(names(traindata) %in% c("TestAge","Age"))]
		testdata <- testdata[,-which(names(testdata) %in% c("TestAge","Age"))]

		output <- rfinterval::rfinterval(score ~., train_data = traindata, test_data = testdata, method = c("quantreg"), symmetry = TRUE, alpha = 0.1)
		df_tmp <- data.frame(Age = Age, Pred = output$testPred, quant_reg = output$oob_interval)
		df_tmp$width <- df_tmp$quant_reg.upper - df_tmp$quant_reg.lower
		df_tmp$Group <- ifelse(df_tmp$Age < as.numeric(summary(df_tmp$Age)[2]), 'Q1', ifelse(df_tmp$Age > as.numeric(summary(df_tmp$Age)[3]), 'Q3', 'M'))
		

		C1 <- ggplot(df_tmp,aes(x=Age,y=width)) + geom_point(color = '#377eb8',size=3,alpha=0.8) + stat_smooth(method='loess',color="#000000") + labs(x = 'Age (weeks)', y = 'PI width (vFI)') + theme_bw(base_size=22) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

		C2 <- ggplot(df_tmp,aes(x = width, color = Group)) + geom_density(lwd=1.2) + geom_rug() + scale_color_manual(values=c('#006d2c','#e7298a','#525252')) + theme_bw(base_size=22) + labs(x='PI width', y='Density') + theme(legend.position='top') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20)) 

		p <- C1|C2

		return(p)

	}


}

feature_importance_age_vs_video <- function(mae){

	vi_list <- list()
	Features <- c('Center Time','Periphery Time','Corner Time','Center Distance','Periphery Distance','Corner Distance','Grooming Bouts','Grooming Duration','Angular Velocity','Base Tail LD','Limb Duty Factor','Nose LD','Speed','Step Length1','Step Length2','Step Width','Stride Length','Tip Tail LD','Stride Count','Distance','Angular Velocity IQR','Base Tail LD IQR','Limb Duty Factor IQR','Nose LD IQR','Speed IQR','Step Length1 IQR','Step Length2 IQR','Step Width IQR','Stride Length IQR','Tip Tail LD IQR','dAC','dB','aABC','Width','Length','Rearpaw','Rearcount','Rears_0_5',"dAC_nongait_median","dAC_nongait_stdev","dB_nongait_median","dB_nongait_stdev","aABC_nongait_median","aABC_nongait_stdev")

	median_gait_measures_linear <- c("Angular Velocity","Base Tail LD",
	"Limb Duty Factor","Nose LD","Speed","Step Length1","Step Length2","Step Width","Stride Length",
	"Tip Tail LD")

	iqr_gait_measures_linear <- c("Angular Velocity IQR","Base Tail LD IQR",
	"Limb Duty Factor IQR","Nose LD IQR","Speed IQR","Step Length1 IQR","Step Length2 IQR","Step Width IQR","Stride Length IQR","Tip Tail LD IQR")

	OFA_measures <- c("Stride Count","Distance","Center Time","Periphery Time","Corner Time","Center Distance","Periphery Distance","Corner Distance","Grooming Bouts","Grooming Duration")
	

	tmp1 <- read.csv(paste0(mae[1]),header=TRUE)
	ind <- with(tmp1, which.min(RF != quantile(RF, .5, type = 1))[1])

	yname <- "score"
	train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = ind, type = "")
	traindata <- train_test$traindata
	testdata <- train_test$testdata
	traindata_mean <- train_test$traindata_mean
	traindata_sd <- train_test$traindata_sd

	Ytrain <- traindata[,paste0(yname)]
	Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname)))]


	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = 0.50, honesty=FALSE)
	vi_list[[1]] <- grf::variable_importance(modelGRF60)


	tmp1 <- read.csv(paste0(mae[2]),header=TRUE)
	ind <- with(tmp1, which.min(RF != quantile(RF, .5, type = 1))[1])
	yname <- "TestAge"
	train_test <- train_test_data(df, yname = yname, prop = 0.80, seed = ind, type = "")
	traindata <- train_test$traindata
	testdata <- train_test$testdata
	traindata_mean <- train_test$traindata_mean
	traindata_sd <- train_test$traindata_sd

	Ytrain <- traindata[,paste0(yname)]
	Xtrain <- traindata[,-which(names(traindata) %in% c(paste0(yname)))]


	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = 0.50, honesty=FALSE)
	vi_list[[2]] <- grf::variable_importance(modelGRF60)

	df_tmp <- data.frame(importance = do.call(cbind,vi_list))
	names(df_tmp) <- c("vFI", "Age")
	df_tmp <- cbind(Features, df_tmp)
	df_tmp$Type <- ifelse(df_tmp$Features %in% c(median_gait_measures_linear, iqr_gait_measures_linear), 'Gait',ifelse(df_tmp$Features %in% c(OFA_measures), 'OFA', 'Engineered'))
	df_tmp$Outlier <- ifelse(df_tmp$vFI > 0.04 | df_tmp$Age > 0.04, 1, 0)

	p <- ggplot(df_tmp, aes(x = Age, y = vFI, color = Type, label = Features)) + geom_point(size = 4, alpha = 0.5) + scale_color_manual(values=c('#4daf4a','#984ea3','#ff7f00')) + ggrepel::geom_text_repel(aes(label=ifelse(Outlier %in% c(1),as.character(Features),'')),size=6,box.padding=1,max.overlaps = Inf) + geom_abline(intercept = 0, slope = 1, size = 1, linetype = "dashed") + theme_bw(base_size = 18) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

	return(p)


}

plot_data <- function(df, type = ""){

	if (type == "frailty"){

		tmp <- data.frame(matrix(0,length(frailty_parameters),3))
		invisible(lapply(seq(length(frailty_parameters)), function(x) {

			tmp[x,] <<- table(df_merged[,frailty_parameters[x]]);
			if (tmp[x,1] == nrow(df_merged)){
				tmp[x,c(2,3)] <<- c(0,0)
			} else {
				tmp[x,] <<- table(df_merged[,frailty_parameters[x]])
			}
		}))
		colnames(tmp) <- c("0","0.5","1")
		tmp <- cbind(Parameters = frailty_parameters, tmp)
		tmp_melt <- reshape2::melt(tmp, ids= Parameters)
		p <- ggplot(tmp_melt, aes(x = Parameters, y = value, fill = variable)) + geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2",labels = c("0","0.5","1")) + theme_bw(base_size = 18) + theme(axis.text.x = element_text(angle = 90, size =16, vjust=0.4,hjust=1.01), legend.position = "none") + labs(x = "Frailty parameters", y = "Count") 

		return(p)


	} else {

		median_gait_measures_linear <- c("median_angular_velocity","median_base_tail_lateral_displacement",
		"median_limb_duty_factor","median_nose_lateral_displacement","median_speed_cm_per_sec",
		"median_step_length1","median_step_length2","median_step_width","median_stride_length",
		"median_tip_tail_lateral_displacement")

		iqr_gait_measures_linear <- c("angular_velocity_iqr","base_tail_lateral_displacement_iqr",
		"limb_duty_factor_iqr","nose_lateral_displacement_iqr","speed_cm_per_sec_iqr",
		"step_length1_iqr","step_length2_iqr","step_width_iqr","stride_length_iqr",
		"tip_tail_lateral_displacement_iqr")

		OFA_measures <- c("stride_count","Distance.cm.sc","center_time_secs","periphery_time_secs","corner_time_secs","center_distance_cm","periphery_distance_cm","corner_distance_cm","grooming_number_bouts","grooming_duration_secs")
		engineered_features_mean <- c("dAC_mean","dB_mean","aABC_mean","dAC_nongait_mean","dB_nongait_mean","aABC_nongait_mean")
		engineered_features_median <- c("dAC_median","dB_median","aABC_median","dAC_nongait_median","dB_nongait_median","aABC_nongait_median")
		engineered_features_stdev <- c("dAC_stdev","dB_stdev","aABC_stdev","dAC_nongait_stdev","dB_nongait_stdev","aABC_nongait_stdev")
	
		rearpaw_pose_measures <- c("median_rearpaw")
		rears_measures <- c("rear_count","rears_0_5","rears_0_10")
		ellipsefit_measures <- c("median_width", "median_length")




		p1 <- ggplot(df,aes(x=TestAge)) + geom_histogram(fill="#e41a1c",bins = 40, color = "black") + labs(x = 'Age (weeks)', y = 'Frequency') + theme_bw(base_size=18) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))
		p3 <- ggplot(df,aes(x=score)) + geom_histogram(fill='#377eb8',bins = 40, color = "black") + labs(x = TeX('Manual $FI_{adj}$ score'), y = 'Frequency') + theme_bw(base_size=18) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

		corr.df <- data.frame(feature = names(df[,-which(names(df) %in% c('MouseID','score','TestAge'))]), corr = as.numeric(apply(df[,-which(names(df) %in% c('MouseID','score','TestAge'))], 2, function(x) cor(df$score,x))))
		corr.df$Type <- ifelse(corr.df$feature %in% c(median_gait_measures_linear, iqr_gait_measures_linear), 'Gait',ifelse(corr.df$feature %in% c(OFA_measures), 'OFA', 'Engineered'))
		corr.df$Type <- factor(corr.df$Type,levels=c('Gait','OFA','Engineered'))
		p4 <- ggplot(corr.df, aes(x = seq(1,44), y = abs(corr), color = Type)) + geom_point(size = 3, stroke = 1, alpha = 0.8) + labs(x = 'Feature', y = 'Absolute Correlation') + scale_color_manual(values=c('#4daf4a','#984ea3','#ff7f00')) + theme_bw(base_size = 18) + theme(legend.position = 'none') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

		corr.df <- data.frame(feature = names(df[,-which(names(df) %in% c('MouseID','score','TestAge'))]), corr = as.numeric(apply(df[,-which(names(df) %in% c('MouseID','score','TestAge'))], 2, function(x) cor(df$TestAge,x))))
		corr.df$Type <- ifelse(corr.df$feature %in% c(median_gait_measures_linear, iqr_gait_measures_linear), 'Gait',ifelse(corr.df$feature %in% c(OFA_measures), 'OFA', 'Engineered'))
		corr.df$Type <- factor(corr.df$Type,levels=c('Gait','OFA','Engineered'))
		p2 <- ggplot(corr.df, aes(x = seq(1,44), y = abs(corr), color = Type)) + geom_point(size = 3, stroke = 1, alpha = 0.8) + labs(x = 'Feature', y = 'Absolute Correlation') + scale_color_manual(values=c('#4daf4a','#984ea3','#ff7f00')) + theme_bw(base_size = 18) + theme(legend.position = 'none') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

		return(p1|p2|p3|p4)


	}
	
}

plot_frailty_re <- function(data){

	df <- data
	frailty_parameters <- c("Alopecia","Loss.of.fur.colour","Dermatitis","Loss.of.whiskers","Coat.condition","Piloerection","Cataracts","Eye.discharge.swelling","Microphthalmia","Corneal.opacity","Nasal.discharge","Rectal.prolapse","Vaginal.uterine.","Diarrhea","Vestibular.disturbance","Vision.loss..Visual.Placing.","Menace.reflex","Tail.stiffening","Gait.disorders","Tremor","Tumours","Distended.abdomen","Kyphosis","Body.condition","Breathing.rate.depth","Malocclusions","Righting.Reflex")
	tmp <- numeric()
	for (x in seq(length(frailty_parameters))){
		#cat('frailty_parameter = ', frailty_parameters[x], "\n"); 
		df[,names(df) %in% frailty_parameters[x]] <- factor(df[,names(df) %in% frailty_parameters[x]]);
		if (length(table(df[,names(df) %in% frailty_parameters[x]])) > 1){
			mod_glmer <- clmm(paste(frailty_parameters[x], " ~ ", "Weight + TestAge + Sex + (1|Tester)", collapse = ""), data = df[!duplicated(df$MouseID),]);
			tmp[x] <- mod_glmer$ST$Tester[1]^2;
		} else {
			next
		}}


	df_plot <- data.frame(Parameter = frailty_parameters, Effect = tmp)
	p <- ggplot(df_plot, aes(x = Parameter, y = Effect)) + geom_bar(stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = 'Set2') + theme_bw(base_size = 16) + theme(axis.text.x = element_text(angle = 90, hjust=1, vjust =0.4,size = 20),axis.text.y = element_text(size = 20)) + labs(y = 'Tester Effect')
	return(p)

}


model_comparisons <- function(mae,rmse,r2,type){

	if (type == "frailty"){

		tmp1 <- read.csv(paste0(mae[1]),header=TRUE)[,3]
		tmp2 <- read.csv(paste0(rmse[1]),header=TRUE)[,3]
		tmp3 <- read.csv(paste0(r2[1]),header=TRUE)[,3]

		tmp1v <- read.csv(paste0(mae[2]),header=TRUE)[,3]
		tmp2v <- read.csv(paste0(rmse[2]),header=TRUE)[,3]
		tmp3v <- read.csv(paste0(r2[2]),header=TRUE)[,3]

		df_mae <- data.frame(frailty = tmp1, video = tmp1v)
		df_rmse <- data.frame(frailty = tmp2, video = tmp2v)
		df_r2 <- data.frame(frailty = tmp3, video = tmp3v)

		MAE.melt <- reshape::melt(df_mae)	
		RMSE.melt <- reshape::melt(df_rmse)
		R2.melt <- reshape::melt(df_r2)

		MAE.melt$sim <- rep(1:50,2)
		RMSE.melt$sim <- rep(1:50,2)
		R2.melt$sim <- rep(1:50,2)


		comp1 <- lmer(value ~ variable + (1|sim), data = MAE.melt)
		comp2 <- lmer(value ~ variable + (1|sim), data = RMSE.melt)
		comp3 <- lmer(value ~ variable + (1|sim), data = R2.melt)

		return(list(Anova(comp1,test = "F"), Anova(comp2,test = "F"), Anova(comp3,test = "F")))


	} else {

		tmp1 <- read.csv(paste0(mae),header=TRUE)
		tmp2 <- read.csv(paste0(rmse),header=TRUE)	
		tmp3 <- read.csv(paste0(r2),header=TRUE)

		MAE.melt <- reshape::melt(tmp1)	
		RMSE.melt <- reshape::melt(tmp2)
		R2.melt <- reshape::melt(tmp3)

		MAE.melt$sim <- rep(1:50,4)
		RMSE.melt$sim <- rep(1:50,4)
		R2.melt$sim <- rep(1:50,4)

		comp1 <- lmer(value ~ variable + (1|sim), data = MAE.melt)
		comp2 <- lmer(value ~ variable + (1|sim), data = RMSE.melt)
		comp3 <- lmer(value ~ variable + (1|sim), data = R2.melt)

		return(list(Anova(comp1,test = "F"), Anova(comp2,test = "F"), Anova(comp3,test = "F")))

	}

}

