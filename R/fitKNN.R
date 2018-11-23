#' @name fitKNN
#'
#' @title Fit using K-nearest neighbour
#'
#' @description Model adegency matrix using K-nearest neighbour approach
#'
#' @param data An object of the class alienData, see \code{\link{alienData}}.
#' @param distFrom Character string defining which distance (or dissimilarity) to apply on the "From" species.
#' @param distTo Character string defining which distance (or dissimilarity) to apply on the "To" species.
#' @param nNeig Integer defining how many neighbours to consider.
#' 
#' @author
#' F. Guillaume Blanchet and Dominique Gravel
#'
#' @importFrom stats aggregate as.formula
#' @importFrom utils type.convert
#' @import randomForest
#'
#' @export
fitKNN <- function(data){
  
}