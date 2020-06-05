#' @name fitDMC
#'
#' @title Fit direct matching centrality
#'
#' @description Fit direct matching centrality model
#'
#' @param data an object of the class \code{\link{alienData}}
#' @param formula A one-sided formula specifying how the different traits from both sets of species should be used to estimate species interactions. Default is to include all terms additively, with quadratics for quantitative terms, and all From-by-To traits interactions.
#' @param type Method to use to estimate the model. Either "randomForest" (\link[randomForest]{randomForest}) or "glm" (\code{\link[stats]{glm}}). 
#' @param family The family of the response variable. See \link[stats]{family}, or the choices available. This is argument is only active when type = "glm".
#' @param \dots Other parameters passed to either \link[randomForest]{randomForest} or \link[stats]{glm}.
#' 
#' @details 
#' 
#' The direct matching centrality models are designed to be used on bipartite network where traits are available for both sets of species interacting in the network. It should not be used otherwise.
#' 
#' This function unfold the adjacency matrix and uses it as the response variables. As explanatory variables, the traits for each sets of species are repeated to match the length of the unfolded adjacency matrix but also the position.
#' 
#' @return 
#' 
#' An object with a class alienFit and a class fitDMC. If the type of model used to perform the fitting is "glm", a class "glm" is also given to the object. Similarly, if the type of model used to perform the fitting is "randomForest", a class "randomForest" is also given to the object.
#' 
#' @author
#' 
#' Dominique Gravel, Steve Vissault, F. Guillaume Blanchet
#'
#' @export

fitDMC <- function(data, formula = NULL, type = NULL,
                   family = NULL, ...) {

  stopifnot(class(data) == "alienData")

  # Adjacency matrix
  adjMat <- data$adjMat
  nFromSp <- ncol(adjMat)
  nToSp <- nrow(adjMat)
  
  # Check if adjMat is a binary or not
  if(all(unique(as.vector(adjMat)) %in% c(0,1))){
    binary <- TRUE
  }else{
    binary <- FALSE
  }
  
  # Trait matrix
  traitFromBase <- data$traitFrom
  traitToBase <- data$traitTo
  
  # Check for NAs in traits
  if(any(is.na(traitFromBase))){
    stop("There is at least one NA in the data$traitFrom.")
  }
  
  if(any(is.na(traitToBase))){
    stop("There is at least one NA in the data$traitTo.")
  }
  
  # Unfold adjMat into a vector
  adjVec <- as.vector(adjMat)
  
  # Organize trait$to to match the size and organization of adjMat
  traitTo <- as.data.frame(traitToBase[rep(seq_len(nToSp),
                                           nFromSp),])
  colnames(traitTo) <- colnames(traitToBase)
  
  # Organize trait$from to match the size and organization of adjMat
  traitFrom <- as.data.frame(traitFromBase[rep(seq_len(nFromSp),
                                             each = nToSp),])
  colnames(traitFrom) <- colnames(traitFromBase)
  
  # Organize data into a single object
  dat <- cbind(adjVec, traitTo, traitFrom)
  
  # Reorganize formula
  if(is.null(formula)){
    mvabund:::get.polys()
  }else{
    Formula <- adjVec ~ x + y # Bogus formula
    Formula[[1]] <- formula[[1]]
    Formula[[3]] <- formula[[2]]
  }
  
  # GLM
  if(type == "glm"){
    model <- stats::glm(Formula, data = dat, family = family, ...)

    # Prediction
    pred <- predict(model, newdata = dat, type = "response")
    
    # Organise result into a matrix
    res <- matrix(pred, nrow = nToSp, ncol = nFromSp)
  }

  # Random forest
  if(type == "randomForest"){
    if(binary){
      dat$adjVec <- as.factor(dat$adjVec)
    }
    
    model <- randomForest::randomForest(Formula,
                                        data = dat,
                                        na.action = na.omit,
                                        ...)

    # Prediction
    if(binary){
      pred <- predict(model, newdata = dat, type = "prob")
      
      # Organise result into a matrix
      res <- matrix(pred[,2],
                    nrow = nToSp,
                    ncol = nFromSp) # Focuses only on 1s
    }else{
      pred <- predict(model, newdata = dat, type = "response")
      
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
                          model = model)
  
  # Define object class
  if(type == "glm"){
    class(res) <- c("alienFit", "fitDMC", "glm")
  }
  
  if(type == "randomForest"){
    class(res) <- c("alienFit", "fitDMC", "randomForest")
  }
  
  # Return results
  return(res)
}
