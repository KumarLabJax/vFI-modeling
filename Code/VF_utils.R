create_storage_matrix <- function(nsim,ncols,col_names, l_names, type = ""){

	if (type == "frailty"){
		storage <- list(data.frame(matrix(0,nsim,ncols)))
		colnames(storage[[1]]) <- col_names
		names(storage) <- l_names

	} else {
		storage <- list(MAE = data.frame(matrix(0,nsim,ncols)), RMSE = data.frame(matrix(0,nsim,ncols)), R2 = data.frame(matrix(0,nsim,ncols)))
		colnames(storage$MAE) <- col_names
		colnames(storage$RMSE) <- col_names
		colnames(storage$R2) <- col_names
	}
	return(storage)
}

save_results <- function(results, expt){

	if (expt == "I"){

		FI_MAE <- results$results_FI$MAE
		FI_RMSE <- results$results_FI$RMSE
		FI_R2 <- results$results_FI$R2

		Age_MAE <- results$results_Age$MAE
		Age_RMSE <- results$results_Age$RMSE
		Age_R2 <- results$results_Age$R2

		write.csv(x=FI_MAE,file='FI_MAE_I.csv',row.names=FALSE)
		write.csv(x=FI_RMSE,file='FI_RMSE_I.csv',row.names=FALSE)
		write.csv(x=FI_R2,file='FI_R2_I.csv',row.names=FALSE)

		write.csv(x=Age_MAE,file='Age_MAE_I.csv',row.names=FALSE)
		write.csv(x=Age_RMSE,file='Age_RMSE_I.csv',row.names=FALSE)
		write.csv(x=Age_R2,file='Age_R2_I.csv',row.names=FALSE)

	} else if (expt == "II"){

		FI_MAE <- results$results_FI$MAE
		FI_RMSE <- results$results_FI$RMSE
		FI_R2 <- results$results_FI$R2

		write.csv(x=FI_MAE,file='FI_MAE_II.csv',row.names=FALSE)
		write.csv(x=FI_RMSE,file='FI_RMSE_II.csv',row.names=FALSE)
		write.csv(x=FI_R2,file='FI_R2_II.csv',row.names=FALSE)

	} else if (expt == "III"){

		FI_MAE <- results$results_FI$MAE
		FI_RMSE <- results$results_FI$RMSE
		FI_R2 <- results$results_FI$R2

		Age_MAE <- results$results_Age$MAE
		Age_RMSE <- results$results_Age$RMSE
		Age_R2 <- results$results_Age$R2

		write.csv(x=FI_MAE,file='FI_MAE_III.csv',row.names=FALSE)
		write.csv(x=FI_RMSE,file='FI_RMSE_III.csv',row.names=FALSE)
		write.csv(x=FI_R2,file='FI_R2_III.csv',row.names=FALSE)

		write.csv(x=Age_MAE,file='Age_MAE_III.csv',row.names=FALSE)
		write.csv(x=Age_RMSE,file='Age_RMSE_III.csv',row.names=FALSE)
		write.csv(x=Age_R2,file='Age_R2_III.csv',row.names=FALSE)

	} else if (expt == "IV") {

		Age_MAE <- results$results_Age$MAE
		Age_RMSE <- results$results_Age$RMSE
		Age_R2 <- results$results_Age$R2

		Age_MAE_video <- results$results_Age_video$MAE
		Age_RMSE_video <- results$results_Age_video$RMSE
		Age_R2_video <- results$results_Age_video$R2

		write.csv(x=Age_MAE,file='Age_MAE_IV.csv',row.names=FALSE)
		write.csv(x=Age_RMSE,file='Age_RMSE_IV.csv',row.names=FALSE)
		write.csv(x=Age_R2,file='Age_R2_IV.csv',row.names=FALSE)

		write.csv(x=Age_MAE_video,file='Age_MAE_video_IV.csv',row.names=FALSE)
		write.csv(x=Age_RMSE_video,file='Age_RMSE_video_IV.csv',row.names=FALSE)
		write.csv(x=Age_R2_video,file='Age_R2_video_IV.csv',row.names=FALSE)


	} else {

		precision_enet <- results[[1]]
		precision_rf <- results[[2]]
		precision <- merge(precision_enet,precision_rf, by = "frailty_parameters")		
		
		recall_enet <- results[[3]]
		recall_rf <- results[[4]]
		recall <- merge(recall_enet,recall_rf, by = "frailty_parameters")		
		
		f1score_enet <- results[[5]]
		f1score_rf <- results[[6]]
		f1score <- merge(f1score_enet,f1score_rf, by = "frailty_parameters")		
		
		acc_enet <- results[[7]]
		acc_rf <- results[[8]]
		acc <- merge(acc_enet,acc_rf, by = "frailty_parameters")		
		
		
		write.csv(x=precision,file='precision_V.csv',row.names=FALSE)
		write.csv(x=recall,file='recall_V.csv',row.names=FALSE)
		write.csv(x=f1score,file='f1score_V.csv',row.names=FALSE)
		write.csv(x=acc,file='acc_V.csv',row.names=FALSE)

		
	}

	return("Results saved!")

}


