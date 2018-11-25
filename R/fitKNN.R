#' @name fitKNN
#'
#' @title Fit using K-nearest neighbour
#'
#' @description Model adegency matrix using K-nearest neighbour approach
#'
#' @param data An object of the class alienData, see \code{\link{alienData}}.
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
#' When ranking the species to find the \code{nNeig} nearest neighbour, in case of ties the argument   \code{tie.method} in the \code{\link{rank}} function is set to "first". 
#' 
#' @author
#' F. Guillaume Blanchet and Dominique Gravel
#'
#' @return 
#' 
#' A matrix giving the the probability of interaction among each pairs of species
#'
#' @importFrom stats aggregate as.formula
#' @importFrom utils type.convert
#'
#' @export
fitKNN <- function(data, distFrom = "jaccard", distTo = "jaccard",
                   distTraitFrom = "euclidean",
                   distTraitTo = "euclidean", traitWeight, nNeig){
  # Check
  stopifnot(class(data) == "alienData")
  if(traitWeight > 1 | traitWeight < 0){
    stop("'traitWeight' needs to be between 0 and 1")
  }
  
  # Get adjacency matrix
  adjMat <- getAdjacencyMatrix(data, bipartite = TRUE)

  # Basic information
  nFromSp <- nrow(adjMat)
  nToSp <- ncol(adjMat)
  
  # Get trait matrix
  traitMat <- getTraitMatrix(data)
  
  # Distance species
  distFromSp <- as.matrix(vegan::vegdist(adjMat, method = distFrom))
  distToSp <- as.matrix(vegan::vegdist(t(adjMat), method = distTo))
  
  # Distance traits
  distFromTr <- as.matrix(vegan::vegdist(traitMat, method = distFrom))
  distToTr <- as.matrix(vegan::vegdist(t(traitMat), method = distTo))
  
  # Trait weighted distance
  wDistFromSp <- (1 - traitWeight) * distFromSp + traitWeight * distFromTr
  wDistToSp <-  (1 - traitWeight) * distToSp + traitWeight * distToTr
  
  # Result object
  res <- matrix(nr = nFromSp, nc = nToSp)
  for(i in 1:nFromSp) {
    for(j in 1:nToSp) {
      # Find the most "nNeig" similar species for the To species
      KNNFromSp <- which(rank(wDistFromSp[,i], ties.method = "first") <= nNeig)
      
      # Find the most "nNeig" similar species for the From species
      KNNToSp <- which(rank(wDistToSp[,j], ties.method = "first") <= nNeig)

      # Compute the interaction probability
      res[i, j] <- sum(adjMat[i, KNNToSp]) / nNeig / 2 +
                   sum(adjMat[j, KNNFromSp])/ nNeig / 2 
    }
  }
  
  return(res)
}

### Question Dominique
# 1 - diag() = 1 : Pourquoi ? Ça entraine beaucoup de problème conceptuel
# 2 - dist_traits_bottom / max(dist_traits_bottom) : Pourquoi ? C'est quoi cette standardization ?
