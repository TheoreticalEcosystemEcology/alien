#' @name fitDirectCentrality
#'
#' @title Direct matching centrality
#'
#' @description Currently deals with logistic and poisson regression, and random forest
#'
#' @param data a dataframe with a first column named I (the interaction) and extra columns with covariates (can be named at will)
#' @param algorithm one of: logistic, poisson, RF
#' @param ... other params passed to algorithm
#'
#' @author
#' Dominique Gravel
#'
#' @references
#'
#' @export
fitDirectCentrality <- function(data, algorithm, ...) {
    
    if (algorithm == "logistic") 
        model <- stats::glm(I ~ . * ., data, family = "binomial") else if (algorithm == "poisson") 
        model <- stats::glm(I ~ . * ., data, family = "poisson") else if (algorithm == "RF") {
        model <- randomForest::randomForest(I ~ . * ., data, ...)
    }
    
    return(model)
}
