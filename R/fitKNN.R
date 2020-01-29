#' @name fitKNN
#'
#' @title Fit using K-nearest neighbour
#'
#' @description Model adegency matrix using K-nearest neighbour approach
#'
#' @param data An object of the class alienData, see \code{\link{alienData}}.
#' @param binary Logical. Whether the adjacency matrix is binary or not. Default is TRUE.
#' @param distFrom Character string defining which distance (or dissimilarity) to apply on the "From" species. Check \code{\link[vegan]{vegdist}} for the distances to choose from. Default is "jaccard".
#' @param distTo Character string defining which distance (or dissimilarity) to apply on the "To" species. Check \code{\link[vegan]{vegdist}} for the distances to choose from. Default is "jaccard".
#' @param distTraitFrom Character string defining which distance (or dissimilarity) to apply on traits of the "From" species. Check \code{\link[vegan]{vegdist}} for the distances to choose from. Default is "euclidean".
#' @param distTraitTo Character string defining which distance (or dissimilarity) to apply on traits of the "To" species. Check \code{\link[vegan]{vegdist}} for the distances to choose from. Default is "euclidean".
#' @param traitWeight Numeric. Defines the contribution of the traits in the analysis. Must be between 0 and 1. Default is 0.5.
#' @param nNeig Integer defining how many neighbours to consider.
#'
#' @details
#'
#' This function should only be used for bipartite adjency matrice.
#'
#' The argument \code{traitWeight} defines the important of traits in the analysis. If a weight of 0 is given, the traits are assumes to have no importance. Conversely, if \code{traitWeight} is 1 the traits are given their full importance in the analysis.
#'
#' When ranking the species to find the \code{nNeig} nearest neighbour, in case of ties the argument \code{tie.method} in the \code{\link{rank}} function is set to "first". Also, when ranking the species, it is assumed that the species is not a neighbour of itself.
#'
#' NAs were removed in the calculation of the distances whenever they were present, but also in the calculation of the interaction probability. For species where all distance values are NAs, the returned interactions probability will be 0.
#'
#' @author
#' 
#' F. Guillaume Blanchet and Dominique Gravel
#'
#' @return
#'
#' An object of class alienFit.
#'
#' @export
fitKNN <- function(data, binary = TRUE, distFrom = "jaccard", 
                   distTo = "jaccard",
                   distTraitFrom = "euclidean",
                   distTraitTo = "euclidean", traitWeight, nNeig){
  # Check
  stopifnot(class(data) == "alienData")
  if(traitWeight > 1 | traitWeight < 0){
    stop("'traitWeight' needs to be between 0 and 1")
  }

  # Get adjacency matrix
  adjMat <- getAdjacencyMatrix(data, bipartite = TRUE,
                               binary = binary)

  # Basic information
  nFromSp <- ncol(adjMat)
  nToSp <- nrow(adjMat)

  # Get trait matrix
  traits <- getTrait(data)
  traitsFrom <- stats::model.matrix(~ -1 +., data = traits$from)
  traitsTo <- stats::model.matrix(~ -1 +., data = traits$to)

  # Distance species
  distFromSp <- as.matrix(vegan::vegdist(t(adjMat), method = distFrom, na.rm = TRUE))
  distToSp <- as.matrix(vegan::vegdist(adjMat, method = distTo, na.rm = TRUE))

  # Distance traits
  distFromTr <- as.matrix(vegan::vegdist(traitsFrom, method = distTraitFrom, na.rm = TRUE))
  distToTr <- as.matrix(vegan::vegdist(traitsTo, method = distTraitTo, na.rm = TRUE))

  # Trait weighted distance
  wDistFromSp <- (1 - traitWeight) * distFromSp + traitWeight * distFromTr
  wDistToSp <-  (1 - traitWeight) * distToSp + traitWeight * distToTr

  # Result object
  res <- matrix(NA, nrow = nFromSp, ncol = nToSp)
  dimnames(res) <- list(rownames(traits$from), rownames(traits$to))

  for(i in 1:nFromSp) {
    for(j in 1:nToSp) {
      # Order species distance for the focal species
      KNNFromSpOrd <- order(wDistFromSp[,i])[-1]
      KNNToSpOrd <- order(wDistToSp[,j])[-1]

      # Sort all the interacting species
      interFrom <- adjMat[KNNToSpOrd, i]
      interTo <- adjMat[j, KNNFromSpOrd]
      
      # From the sorted interacting species (that are not NAs), select the nNeig ones
      interFromNeig <- which(!is.na(interFrom))[1:nNeig]
      interToNeig <- which(!is.na(interTo))[1:nNeig]
      
      # Check in there are nNeig species interacting
      interFromNeigNA <- is.na(interFromNeig)
      interToNeigNA <- is.na(interToNeig)
      
      # Send warning if there are less than nNeig species interacting
      if(any(interFromNeigNA)){
        warning(paste("There are not", nNeig, "neighbours for",
                      colnames(adjMat)[i],
                      " - Calculation were done with", 
                      sum(!interFromNeigNA), "neighbours"))
      }
      if(any(interToNeigNA)){
        warning(paste("There are not", nNeig, "neighbours for",
                      rownames(adjMat)[j],
                      " - Calculation were done with", 
                      sum(!interToNeigNA), "neighbours"))
      }
      
      # Calculate KNN values
      res[i, j] <- sum(interFrom[interFromNeig], na.rm = TRUE) / nNeig / 2 +
                   sum(interTo[interToNeig], na.rm = TRUE)/ nNeig / 2
    }
  }
  
  # Add model as attribute
  baseAttr <- attributes(res)
  attributes(res) <- list(dim = baseAttr$dim,
                          dimnames = baseAttr$dimnames,
                          adjMat = adjMat)

  # Define object class
  class(res) <- "alienFit"
  
  # Return result
  return(res)
}
