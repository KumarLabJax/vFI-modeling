plot_results <- function(results_list, expt){

	if (expt == "II"){
		Diet <- as.factor(c("2D", "1D", "40", "20", "AL"))
		results_FI <- results_list$results_FI
		results_Age <- results_list$results_Age
		tmpp_FI <- list()
		tmpp_Age <- list()
		for (d in Diets){
			tmp <- data.frame(matrix(as.numeric(data.frame(results_FI[[paste0(d)]])), nrow = 4, ncol = 3, byrow = T))
			colnames(tmp) <- c("RMSE", "R2", "MAE")
			tmp$Diet <- rep(as.factor(paste0(d)), 4)
			tmp$Model <- as.factor(c("Enet", "SVM", "RF", "XGB"))
			tmpp_FI[[d]] <- tmp

			tmp <- data.frame(matrix(as.numeric(data.frame(results_Age[[paste0(d)]])), nrow = 4, ncol = 3, byrow = T))
			colnames(tmp) <- c("RMSE", "R2", "MAE")
			tmp$Diet <- rep(as.factor(paste0(d)), 4)
			tmp$Model <- as.factor(c("Enet", "SVM", "RF", "XGB"))
			tmpp_Age[[d]] <- tmp
		}
		resultsFI <- data.frame(do.call(rbind, tmpp_FI))
		rownames(resultsFI) <- NULL
		resultsAge <- do.call(rbind, tmpp_Age)
		rownames(resultsAge) <- NULL

		#Plot results
		resultsFI_melt <- reshape2::melt(resultsFI)
		p <- ggplot(resultsFI_melt, aes(x = Model, y = value, color = Diet)) + geom_point(stroke = 1, alpha = 0.8, size = 6) + facet_wrap(. ~ variable, scales = "free") + theme_bw(base_size = 16) + ggtitle("LOOCV Results")
		return(p)
	}
	
}