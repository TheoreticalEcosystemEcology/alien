#' @name rsquare
#' 
#' @title Pseudo-\eqn{R^2}{R2}
#'
#' @ Calculates Efron's pseudo-\eqn{R^2}{R2} for \code{alienFit} objects
#' 
#' @param alientFit An object of class \code{alienFit}.
#'
#' @details 
#' 
#' Efron's pseudo-\eqn{R^2}{R2} is calculated as:
#'
#' \deqn{R^2 = 1 - \frac{(y_i-\hat{\pi})^2}{(y_i-\bar{y})^2}{R2=1-((y-pihat)^2)/((y-ybar)^2)}}.
#'
#' where \eqn{\hat{\pi}}{pihat} is the model predicted values calculated on the scale of the response variable.
#'
#' This version of pseudo-\eqn{R^2}{R2} was chosen over the others because it can be calculated for all types of models implemented in this package.
#'
#' @references
#'
#' Efron, B. (1978) Regression and ANOVA with Zero-One Data: Measures of Residual Variation., \emph{Journal of the American Statistical Association} \strong{73}, 113-121.
#'
#' @export
rsquare <- function(alienFit){
  # General check
  stopifnot(class(alienFit) == "alienFit")
  
  # Extract response (adjacency) matrix
  Y <- attributes(alienFit)$adjMat
  
  # Extract prediction matrix
  Ypred <- alienFit
  
  #------------------------
  # Calculate the pseudo-R2
  #------------------------
  # Total sums of squares per species
  ssY <- colSums(sweep(Y,2,colMeans(Y),FUN="-")^2)
  
  # Residual sums of squares per species
  ssRes <- colSums((Y-Ypred)^2)
  
  # Calculate R2
  R2 <- 1 - ssRes/ssY
  R2 <- mean(R2)
  
  # Return result
  return(R2)

}