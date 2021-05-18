#' @name fitKNN
#'
#' @title Fit using K-nearest neighbour
#'
#' @description Model adegency matrix using K-nearest neighbour approach
#'
#' @param data An object of the class alienData, see \code{\link{alienData}}.
#' @param distFrom Character string defining which distance (or dissimilarity) to apply on the "From" species. Check \code{\link[vegan]{vegdist}} for the distances to choose from. Default is "jaccard".
#' @param distTo Character string defining which distance (or dissimilarity) to apply on the "To" species. Check \code{\link[vegan]{vegdist}} for the distances to choose from. Default is "jaccard".
#' @param distTraitFrom Character string defining which distance (or dissimilarity) to apply on the traits of the "From" species. Check \code{\link[vegan]{vegdist}} for the distances to choose from. If the value is left to NULL, \code{traitDistFrom} (or \code{phyloDistFrom}) in the data (\code{\link{alienData}}) is used directly. Default is NULL.
#' @param distTraitTo Character string defining which distance (or dissimilarity) to apply on the "To" species. Check \code{\link[vegan]{vegdist}} for the distances to choose from. If the value is left to NULL, \code{traitDistTo} (or \code{phyloDistTo}) in the data (\code{\link{alienData}}) is used directly. Default is NULL. 
#' @param weight Numeric. Defines the contribution of the traits (or phylogeny) in the analysis. Must be between 0 and 1. Default is 0.5.
#' @param nNeig Integer  defining how many neighbours to consider.
#' @param phylo Logical. Whether phylogenetic information should be used instead of traits to measure neighbourhood. Default is FALSE.
#'
#' @details
#'
#' This function should only be used for bipartite adjacency matrices.
#'
#' The function is designed in such a way that if the argument \code{distTraitFrom} is defined it will build a distance matrix using \code{traitFrom} in the \code{\link{alienData}} object even if \code{distTraitFrom} is available in \code{\link{alienData}} object. The same is true for the argument \code{distTraitTo}.
#' 
#' The argument \code{weight} defines the important of traits (or phylogeny) in the analysis. If a weight of 0 is given, the traits (or phylogeny) are assumed to have no importance. Conversely, if \code{weight} is 1 the traits (or phylogeny) are given their full importance in the analysis.
#'
#' If \code{phylo} is TRUE, the cophenetic distance is used to calculate the distance between pairs of species.
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
#' An object with a class alienFit and a class fitKNN.
#'
#' @importFrom vegan vegdist
#' @importFrom stats model.matrix as.dist
#' @importFrom ape cophenetic.phylo
#'
#' @export

fitKNN <- function(data, distFrom = "jaccard", 
                   distTo = "jaccard",
                   distTraitFrom = NULL,
                   distTraitTo = NULL, 
                   weight = 0.5, nNeig, phylo = FALSE){
 
  # Check
  stopifnot(class(data) == "alienData")
  if(weight > 1 | weight < 0){
    stop("'weight' needs to be between 0 and 1")
  }

  # Get adjacency matrix
  adjMat <- data$adjMat

  # Basic information
  nFromSp <- nrow(adjMat)
  nToSp <- ncol(adjMat)

  # Distance species
  distFromSp <- as.matrix(vegan::vegdist(adjMat,
                                         method = distFrom, na.rm = TRUE))
  distToSp <- as.matrix(vegan::vegdist(t(adjMat),
                                       method = distTo, na.rm = TRUE))
  
  #================
  # Distance traits
  #================
  if(!phylo){
    # From traits
    if(is.null(distTraitFrom)){
      if(is.null(data$traitDistFrom)){
        distFromTr <- matrix(0, nrow = nFromSp, ncol = nFromSp)
        rownames(distFromTr) <- rownames(distFromSp)
        colnames(distFromTr) <- colnames(distFromSp)
      }else{
        distFromTr <- data$traitDistFrom
      }
    }else{
      # Get trait matrix
      traitFrom <- stats::model.matrix(~ -1 +.,
                                       data = data$traitFrom)
      
      # Distance traits
      distFromTr <- as.matrix(vegan::vegdist(traitFrom,
                                             method = distTraitFrom,
                                             na.rm = TRUE))
    }
    
    # To traits
    if(is.null(distTraitTo)){
      if(is.null(data$traitDistTo)){
        distToTr <- matrix(0, nrow = nToSp, ncol = nToSp)
        rownames(distToTr) <- rownames(distToSp)
        colnames(distToTr) <- colnames(distToSp)
      }else{
        distToTr <- data$traitDistTo
      }
    }else{
      # Get trait matrix
      traitTo <- stats::model.matrix(~ -1 +., 
                                     data = data$traitTo)
      
      # Distance traits
      distToTr <- as.matrix(vegan::vegdist(traitTo,
                                           method = distTraitTo, 
                                           na.rm = TRUE))
    }
    #===============
    # Distance phylo
    #===============
  }else{
    # From phylo
    if(is.null(data$phyloDistFrom)){
      distFromTr <- matrix(0, nrow = nFromSp, ncol = nFromSp)
      rownames(distFromTr) <- rownames(distFromSp)
      colnames(distFromTr) <- colnames(distFromSp)
    }else{
      # Cophenetic phylogenetic distance
      distFromTr <- data$phyloDistFrom
    }

    # To phylo
    if(is.null(data$phyloDistTo)){
      distToTr <- matrix(0, nrow = nToSp, ncol = nToSp)
      rownames(distToTr) <- rownames(distToSp)
      colnames(distToTr) <- colnames(distToSp)
    }else{
      # Cophenetic phylogenetic distance
      distToTr <- data$phyloDistTo
    }
  }
  
  # Make sure distFromTr is a matrix
  distFromTr <- as.matrix(distFromTr)
  
  # Make sure distToTr is a matrix
  distToTr <- as.matrix(distToTr)
  
  # Trait weighted distance
  wDistFromSp <- (1 - weight) * distFromSp + weight * distFromTr
  wDistToSp <-  (1 - weight) * distToSp + weight * distToTr

  # Result object
  res <- matrix(NA, nrow = nFromSp, ncol = nToSp)
  dimnames(res) <- list(rownames(data$traitFrom),
                        rownames(data$traitTo))

  for(i in 1:nFromSp) {
    for(j in 1:nToSp) {
      # Order species distance for the focal species
      KNNFromSpOrd <- order(wDistFromSp[,i])[-1]
      KNNToSpOrd <- order(wDistToSp[,j])[-1]

      # Sort all the interacting species
      interFrom <- adjMat[i,KNNToSpOrd]
      interTo <- adjMat[KNNFromSpOrd,j]
      
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
                          alienData = data,
                          distFrom = distFrom,
                          distTo = distTo,
                          distTraitFrom = as.dist(distFromTr),
                          distTraitTo = as.dist(distToTr),
                          nNeig = nNeig,
                          phylo = phylo)

  # Define object class
  class(res) <- c("alienFit", "fitKNN")
  
  # Return result
  return(res)
}
