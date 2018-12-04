#' @name fitDMC
#'
#' @title Fit direct matching centrality
#'
#' @description Fit direct matching centrality model
#'
#' @param formula formula to define the model.
#' @param data an object of the class \code{\link{alienData}}
#' @param class Method to use to estimate the model. Either "randomForest" (\link[randomForest]{randomForest}) or "glm" (\code{\link[stats]{glm}}). 
#' @param family The family of the response variable. See \link[stats]{family}, or the choices available.
#' @param \dots Other parameters passed to either \link[randomForest]{randomForest} or \link[stats]{glm}.
#' 
#' @author
#' 
#' Dominique Gravel, Steve Vissault, F. Guillaume Blanchet
#'
#' @importFrom stats glm
#' @importFrom randomForest randomForest
#' @importFrom stats aggregate as.formula
#' @importFrom utils type.convert
#'
#' @export
fitDMC <- function(formula, data, class = NULL, family = NULL, 
                   traits = NULL, step = FALSE, ...) {

  stopifnot(class(data) == "alienData")

  # Construct adjencency matrix
  adjMat <- getAdjacencyMatrix(data, bipartite = TRUE)
  nFromSp <- ncol(adjMat)
  nToSp <- nrow(adjMat)
  
  # Construct trait matrix
  traits <- getTrait(data, bipartite = TRUE)
  
  # Check for NAs in traits
  if(any(sapply(traits, function(x) any(is.na(x))))){
    stop("There is at least one NA in the traits. Use getTrait() to investigate.")
  }
  
  # Unfold adjMat into a vector
  adjVec <- as.vector(adjMat)
  
  # Organize trait$to to match the size of adjMat
  traitsTo <- do.call(rbind, replicate(nFromSp, traits$to, simplify=FALSE))
  # Organize trait$to to match the size of adjMat
  traitsFrom <- do.call(rbind, replicate(nToSp, traits$from, simplify=FALSE))

}
