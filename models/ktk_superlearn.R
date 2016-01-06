library(parallel)

cv_combiner <- function(fold_object){
	#print(dim(fold_object$fold_matrix))
	max_folds <- max(fold_object$fold_master)
	hold_cl <- rep(0, nrow(fold_object$fold_matrix))
	for (i in 1:max_folds){
		are_rows <- which(fold_object$fold_master == i)
		hold_cl[are_rows] <- fold_object$fold_matrix[are_rows, i]
	}
	
	return(hold_cl)
}

single_predict <- function(model, to_be_scored){
	return(predict(model, to_be_scored))
}

cv_aggregator_max <- function(to_be_scored, model_master){
	max_models <- lapply(model_master, function(x){return(x$model_object[[x$max_round]])})
	scored <- lapply(max_models, single_predict, to_be_scored)
	scored_matrix <- do.call(cbind, scored)
}

super_learner_train <- function(sl_vector, outcome, phi='glm'){
	# model_vector is a vector of scored data and fold_master. This will be output of the form 
	# list(fold_matrix=ktk_<MOD>_score(<CV MODE TRAINED>, data), fold_master = <CV MODE TRAINED>$FOLD MASTER

	# Combine the fold predictions for each cadidate learner
	z_vects <- mclapply(sl_vector, cv_combiner, mc.cores=6)
	z_matrix <- do.call(cbind, z_vects)

	if (phi == 'glm'){
		super <- glm(z_matrix, outcome, family = binomial())
	}

	return(list(z_matrix=z_matrix, learner=super))
}


super_learner_score <- function(to_be_scored, candidate_models, super, candidate_fold='max'){

	if (candidate_fold == 'max'){
		scored <- cv_aggregator_max(as.matrix(to_be_scored), candidate_models)
		sll <- predict(super$learner, scored)
		return(sll)
	}

}