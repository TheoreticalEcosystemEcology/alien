#' @name fitDMC
#'
#' @title Fit direct matching centrality
#'
#' @description Fit direct matching centrality model
#'
#' @param formula A one-sided formula specifying how the different traits from both sets of species should be used to estimate species interactions.
#' @param data an object of the class \code{\link{alienData}}
#' @param binary Logical. Whether the adjacency matrix is binary or not. Default is TRUE.
#' @param type Method to use to estimate the model. Either "randomForest" (\link[randomForest]{randomForest}) or "glm" (\code{\link[stats]{glm}}). 
#' @param family The family of the response variable. See \link[stats]{family}, or the choices available. This is argument is only active when type = "glm".
#' @param \dots Other parameters passed to either \link[randomForest]{randomForest} or \link[stats]{glm}.
#' 
#' @details 
#' 
#' The direct matching centrality models are designed to be used on bipartite network where traits are available for both sets of species interacting in the network. It should not be used otherwise.
#' 
#' This function builds the adjacency matrix, unfold it and uses it as the response variables. As explanatory variables, the traits for each sets of species are repeated to match the length of the unfolded adjacency matrix but also the position.
#' 
#' @return 
#' 
#' An object of class alienFit.
#' 
#' @author
#' 
#' Dominique Gravel, Steve Vissault, F. Guillaume Blanchet
#'
#' @export

fitDMC <- function(formula, data, binary = TRUE, type = NULL,
                   family = NULL, ...) {

  stopifnot(class(data) == "alienData")

  # Construct adjencency matrix
  adjMat <- getAdjacencyMatrix(data, bipartite = TRUE, binary = TRUE)
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
  
  # Organize trait$to to match the size and organization of adjMat
  traitsTo <- as.data.frame(traits$to[rep(seq_len(nToSp), nFromSp),])
  colnames(traitsTo) <- colnames(traits$to)
  
  # Organize trait$from to match the size and organization of adjMat
  traitsFrom <- as.data.frame(traits$from[rep(seq_len(nFromSp), each = nToSp),])
  colnames(traitsFrom) <- colnames(traits$from)
  
  # Organize data into a single object
  dat <- cbind(adjVec, traitsTo, traitsFrom)
  
  # Reorganize formula
  Formula <- adjVec ~ x + y # Bogus formula
  Formula[[1]] <- formula[[1]]
  Formula[[3]] <- formula[[2]]
  
  # GLM
  if(type == "glm"){
    model <- stats::glm(Formula, data = dat, family = family, ...)

    pred <- predict(model, type = "response")
    
    # Organise result into a matrix
    res <- matrix(pred, nrow = nToSp, ncol = nFromSp)
  }

  # Random forest
  if(type == "randomForest"){
    if(binary){
      dat$adjVec <- as.factor(dat$adjVec)
    }
    model <- randomForest::randomForest(Formula, data = dat, ...)

    # Prediction
    if(binary){
      pred <- predict(model, type = "prob")
      
      # Organise result into a matrix
      res <- matrix(pred[,2], nrow = nToSp, ncol = nFromSp) # Focuses only on 1s
    }else{
      pred <- predict(model, type = "response")
      
      # Organise result into a matrix
      res <- matrix(pred, nrow = nToSp, ncol = nFromSp)
    }
  }
  rownames(res) <- rownames(adjMat)
  colnames(res) <- colnames(adjMat)
  
  # Add model as attribute
  baseAttr <- attributes(res)
  attributes(res) <- list(dim = baseAttr$dim,
                          dimnames = baseAttr$dimnames,
                          model = model,
                          adjMat = adjMat)
  
  # Define object class
  class(res) <- "alienFit"
  
  # Return results
  return(res)
}
