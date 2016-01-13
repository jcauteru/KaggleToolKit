glm_predict <- function (object, newdata, type = c("link", "response", "terms"), bias=TRUE, 
    se.fit = FALSE, dispersion = NULL, terms = NULL, na.action = na.pass, ...){

    if (bias){
        newdata <- cbind(1, newdata)
    }

    if (missing(newdata)) {
        pred <- switch(type, link = object$linear.predictors, 
            response = object$fitted.values, terms = predict.lm(object, 
              se.fit = se.fit, scale = 1, type = "terms", 
              terms = terms))
        if (!is.null(na.act)){
            pred <- napredict(na.act, pred)
        }
    }
    
    else {
        pred <- predict.lm(object, newdata, se.fit, scale = 1, 
            type = ifelse(type == "link", "response", type), 
            terms = terms, na.action = na.action)
        switch(type, response = {
            pred <- family(object)$linkinv(pred)
        }, link = , terms = )
    }

    return(pred)
}

glm_predict_lt <- function(object, newdata, bias=TRUE){
    if (bias){
        newdata <- cbind(1, newdata)
    }

    lm_preds <- odject$coefficients %*% newdata
    probs <- object$family$linkinv(pred)

}