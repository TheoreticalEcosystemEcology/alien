#' @title Extract Log-Likelihood for \code{alienfit} object
#'
#' @description Calculate a Log-Likelihood for and object class \code{alienfit}
#'
#' @param object an object of class \code{alienfit}.
#' @param \dots Some methods for this function require additional arguments.
#' 
#' @details 
#' 
#' For objects of class \code{fitKNN}, log-likelihoods are only available for presence-absence data. An error will be sent otherwise.
#' 
#' @return 
#' 
#' A numerical value presenting a log-likelihood.
#' 
#' @author F. Guillaume Blanchet and Dominique Gravel
#' 
#' @keywords univar
#' @export
logLik.alienFit <- function(object, ...) {
  # KNN
#  if(any(class(object) == "fitKNN")){
    # Extract model and data
    model <- object
    dat <- attributes(object)$alienData$adjMat
    
    # Check
    if(!all(is.na(dat) | dat == 0 | dat == 1)){
      stop("For this model, only presence-absence data should be used")
    }
    
    # Result object
    mat <- matrix(NA, nrow = nrow(dat), ncol = ncol(dat))
    
    # Calculate loglikelihood for presences and absences
    pos1 <- which(dat == 1)
    pos0 <- which(dat == 0)

    mat[pos1] <- log(model[pos1])
    mat[pos0] <- log(1-model[pos0])
    
    # Replace infinite by NA
    mat[which(is.infinite(mat))] <- NA
    
    # Return result
    res <- sum(mat, na.rm=TRUE)
    return(res)
#  }
}
