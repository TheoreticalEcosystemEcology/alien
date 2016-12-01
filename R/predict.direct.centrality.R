#' @name predict.direct.centrality
#'
#' @title Predict direct matching centrality
#'
#' @description It generates predicted interactions based on new data.
#'
#' @param newdata a new dataframe with a first column named I (the interaction) and extra columns with covariates (same name as used to fit)
#' @param model the output of fit.direct.centrality
#' @param replicates Number of interactions to predict based on the interaction probability values
#'
#' @author
#' Dominique Gravel
#'
#' @references
#'
#' @rdname predict.direct.centrality
#' @export
predict.direct.centrality = function(newdata, model, replicates) {
    
    p = predict(model, newdata, type = "response")
    
    I = list()
    for (n in 1:replicates) I[[n]] = rbinom(nrow(newdata), size = 1, prob = p)
    
    return(list(p = p, I = I))
    
}
