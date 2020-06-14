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
#' @param spRandom Logical. Whether species are used as a random effect. This argument is only active when \code{type = "glm"}. Default is FALSE.
#' @param \dots Other parameters passed to either \link[randomForest]{randomForest} or \link[stats]{glm}.
#' 
#' @details 
#' 
#' The direct matching centrality models are designed to be used on bipartite network where traits are available for both sets of species interacting in the network. It should not be used otherwise.
#' 
#' This function unfold the adjacency matrix and uses it as the response variables. As explanatory variables, the traits for each sets of species are repeated to match the length of the unfolded adjacency matrix but also the position.
#' 
#' If there are NAs in the adjacency matrix, the function will omit these values in the estimation of the model.
#' 
#' Also, the default \code{formula} option results in many explanatory variables being constructed, which may result in overparamerisation for \code{type = "glm"}. In this respect, the default option is complete but often too much. 
#' 
#' @return 
#' 
#' An object with a class alienFit and a class fitDMC. If the type of model used to perform the fitting is "glm", a class "glm" is also given to the object. Similarly, if the type of model used to perform the fitting is "randomForest", a class "randomForest" is also given to the object.
#' 
#' @author
#' 
#' F. Guillaume Blanchet, Dominique Gravel, Steve Vissault
#'
#' @export

fitDMC <- function(data, formula = NULL, type = NULL,
                   family = NULL, spRandom = FALSE, ...) {

  stopifnot(class(data) == "alienData")

  # Adjacency matrix
  adjMat <- data$adjMat
  nFromSp <- nrow(adjMat)
  nToSp <- ncol(adjMat)
  
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
  
  # Organize trait$from to match the size and organization of adjMat
  traitFrom <- as.data.frame(traitFromBase[rep(seq_len(nFromSp),
                                               each = nToSp),])
  colnames(traitFrom) <- colnames(traitFromBase)

  # Organize trait$to to match the size and organization of adjMat
  traitTo <- as.data.frame(traitToBase[rep(seq_len(nToSp),
                                           nFromSp),])
  colnames(traitTo) <- colnames(traitToBase)
  
  # Reorganize data for the formula
  if(is.null(formula)){
    traitFromGP <- mvabund:::get.polys(traitFrom)
    traitFrom <-  cbind(traitFromGP$X, traitFromGP$X.squ)
    
    traitToGP <- mvabund:::get.polys(traitTo)
    traitTo <-  cbind(traitToGP$X, traitToGP$X.squ)
  }

  # Organize data into a single object
  dat <- cbind(adjVec, traitTo, traitFrom)
  
  # Row names for dat
  nameBase <- expand.grid(colnames(adjMat), rownames(adjMat))
  rNames <- paste(nameBase[,1], nameBase[,2],sep = "_")
  rownames(dat) <- rNames
  
  # Column names for dat
  colnames(dat)[1] <- c("adj")

  # Organize formula
  if(is.null(formula)){
    # Terms
    formTerm <- terms(adj ~ .*., data = dat)
    
    # formula
    Formula <- update(formTerm, ~ .)
  }else{
    formulaBase <- update(formula, adj ~ .)
    
    # Terms
    formTerm <- terms(formulaBase, data = dat)
    
    # formula
    Formula <- update(formTerm, ~ .)
  }
    
  # GLM
  if(type == "glm"){
    if(spRandom){
      # Add species to dat
      dat <- cbind(dat, sp = as.factor(rNames))
      
      # Terms
      formTerm <- terms(Formula, data = dat)
      
      # Update formula
      Formula <- update(formTerm, ~ . + (1|sp))
      
      model <- lme4::glmer(Formula, data = dat, family = family,
                           na.action = na.omit, ...)
    }else{
      # Without random effect 
      model <- stats::glm(Formula, data = dat, family = family,
                          na.action = na.omit, ...)
    }

    # Prediction
    pred <- predict(model, newdata = dat, type = "response")
    
    # Organise result into a matrix
    res <- matrix(pred, nrow = nFromSp, ncol = nToSp)
  }

  # Random forest
  if(type == "randomForest"){
    if(binary){
      dat$adj <- as.factor(dat$adj)
    }
    
    model <- randomForest::randomForest(formula = Formula,
                                        data = dat,
                                        na.action = na.omit,
                                        ...)

    # Prediction
    if(binary){
      pred <- predict(model, newdata = dat, type = "prob")
      
      # Organise result into a matrix
      res <- matrix(pred[,2],
                    nrow = nFromSp,
                    ncol = nToSp) # Focuses only on 1s
    }else{
      pred <- predict(model, newdata = dat, type = "response")
      
      # Organise result into a matrix
      res <- matrix(pred,
                    nrow = nFromSp,
                    ncol = nToSp)
    }
  }
  
  rownames(res) <- rownames(adjMat)
  colnames(res) <- colnames(adjMat)
  
  # Add model as attribute
  baseAttr <- attributes(res)
  attributes(res) <- list(dim = baseAttr$dim,
                          dimnames = baseAttr$dimnames,
                          model = model,
                          formula = Formula)
  
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
