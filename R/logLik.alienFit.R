#' @title Extract Log-Likelihood for \code{alienfit} object
#'
#' @description Calculate a Log-Likelihood for and object class \code{alienfit}
#'
#' @param object An object of class \code{alienfit}.
#' @param error a numeric value defining the level of uncertainty in the data. It needs to be between 0 and 1 and is usually small.
#' @param \dots Some methods for this function require additional arguments.
#' 
#' @details 
#' 
#' Currently, the function only calculates log-likelihood for presence-absence data. An error will be sent otherwise.
#' 
#' When defining \code{error}, the value given should represent how much uncertainty there is in the probability obtained from the model. In other words, below which probability value is there a lost of confidence in the results. The threshold value given in \code{error} will be used for all probability values obtained from the model that are smaller than \code{error} or larger than 1 -  \code{error.
#' 
#' @return 
#' 
#' A numerical value presenting a log-likelihood.
#' 
#' @author F. Guillaume Blanchet and Dominique Gravel
#' 
#' @keywords univar
#' @export
logLik.alienFit <- function(object, error, ...) {
    if(min(object) < 0 | max(object) > 1){
      stop("The values in 'object' should range between 0 and 1")
    }
    
    if(error > 1 | error < 0){
      stop("The error value should range between 0 and 1")
    }
  
    # Extract model and data
    model <- object
    dat <- attributes(object)$alienData$adjMat
    
    # Check
    if(!all(is.na(dat) | dat == 0 | dat == 1)){
      stop("For this model, only presence-absence data should be used")
    }
    
    # Replace model values below error threshold by error value
    model[which(model < error)] <- error
    model[which(model > (1 - error))] <- 1 - error
    
    # Result object
    mat <- matrix(NA, nrow = nrow(dat), ncol = ncol(dat))
    
    # Calculate loglikelihood for presences and absences
    pos1 <- which(dat == 1)
    pos0 <- which(dat == 0)

    mat[pos1] <- log(model[pos1])
    mat[pos0] <- log(1-model[pos0])
    
    # Return result
    res <- sum(mat, na.rm=TRUE)
    return(res)
}
