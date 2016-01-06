if (!'AUC' %in% rownames(installed.packages())) {install.packages('AUC')}
library('AUC')

MSE <- function(real, preds){
	vect <- (real - preds)^2
	diff <- mean(vect)
	return(list(vect_err=vect, val_err=diff))
}

AUC <- function(real, preds){
	return(auc(roc(preds,real)))
}

ACC <- function(real, preds){
	return(sum(real == preds)/length(real))
}

tfpn <- function(real, preds){
	tp <- sum(real == 1 && preds == 1)
	tn <- sum(real == 0 && preds == 0)
	fp <- sum(real == 0 && preds == 1)
	fn <- sum(real == 1 && preds == 0)
}

validate <- function(test_set, tmo, error_func=MSE()){
	
	real <- test_set[, ncols(test_set)]
	preds <- predict(tmo, test_set)

	error_eval <- error_func(real, preds)
	return(error_eval)
}
