#' @title Calculate Tjur's D
#' 
#' @description Calculate Tjur's D for an object of class \code{alienfit}.
#'
#' @param object An object of class \code{alienfit}.
#' 
#' @details
#' 
#' This function is designed for presence-absence data and will send an error when other data type are used.
#'
#' @return
#' 
#' A numerical value presenting a Tjur's D.
#' 
#' @author F. Guillaume Blanchet and Kate Wootton
#' 
#' @keywords univar
#' @export
tjur <- function(object) {
  # Extract model and data
  model <- object
  dat <- attributes(object)$alienData$adjMat
  
  # Check
  if(!all(is.na(dat) | dat == 0 | dat == 1)){
    stop("For this model, only presence-absence data should be used")
  }
  
  # Calculate Tjur's D
  pos0 <- which(dat==0)
  pos1 <- which(dat==1)
  
  res <- mean(model[pos1]) - mean(model[pos0])
  
  # Return result
  return(res)
}
