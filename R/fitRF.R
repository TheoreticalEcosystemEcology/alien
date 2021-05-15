#' @name fitRF
#'
#' @title Fit direct matching centrality using Random Forest
#'
#' @description Fit direct matching centrality model using Random Forest
#'
#' @param data an object of the class \code{\link{alienData}}
#' @param formula A one-sided formula specifying how the different traits from both sets of species should be used to estimate species interactions. 
#' @param \dots Other parameters passed to \link[randomForest]{randomForest}.
#'
#' @details
#'
#' The Random Forest model is designed to be used on bipartite network where traits are available for both sets of species interacting in the network. It should not be used otherwise.
#'
#' This function unfold the adjacency matrix and uses it as the response variable. As explanatory variables, the traits for each sets of species are repeated to match the length of the unfolded adjacency matrix but also the position.
#'
#' If there are NAs in the adjacency matrix, the function will omit these values in the estimation of the model.
#'
#' @return
#'
#' An object with a class alienFit and a class fitRF.
#'
#' @author
#'
#' F. Guillaume Blanchet, Dominique Gravel, Steve Vissault
#'
# @importFrom mvabund get.polys
#' @importFrom stats terms update predict na.omit
#' @importFrom randomForest randomForest
#' 
#' @export
fitRF <- function(data, formula, ...) {

  stopifnot(class(data) == "alienData")

  # Adjacency matrix
  adjMat <- data$adjMat
  nFromSp <- nrow(adjMat)
  nToSp <- ncol(adjMat)

  # Check if adjMat is a binary or not
  adjMatUnique <- unique(as.vector(adjMat))
  if(any(is.na(adjMatUnique))){
    # Remove NAs for check
    adjMatUnique <- adjMatUnique[-which(is.na(adjMatUnique))]
  }
  
  # Check if binary
  if(all(adjMatUnique %in% c(0,1))){
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

  # Organize data into a single object
  dat <- cbind(adjVec, traitTo, traitFrom)

  # Row names for dat
  nameBase <- expand.grid(colnames(adjMat), rownames(adjMat))
  rNames <- paste(nameBase[,1], nameBase[,2],sep = "_")
  rownames(dat) <- rNames

  # Column names for dat
  colnames(dat)[1] <- c("adj")

  # Organize formula
  formulaBase <- update(formula, adj ~ ., data = dat)
  
  # Terms
  formTerm <- terms(formulaBase, data = dat)
  
  # formula
  Formula <- update(formTerm, ~ .)
  
  # Random forest
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
  
  rownames(res) <- rownames(adjMat)
  colnames(res) <- colnames(adjMat)

  # Add model as attribute
  baseAttr <- attributes(res)
  attributes(res) <- list(dim = baseAttr$dim,
                          dimnames = baseAttr$dimnames,
                          model = model,
                          formula = Formula)

  # Define object class
  class(res) <- c("alienFit", "fitRF")

  # Return results
  return(res)
}
