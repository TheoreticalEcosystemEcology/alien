#' @name fitGLM
#'
#' @title Fit direct matching centrality using generalized linear model
#'
#' @description Fit direct matching centrality model using generalized linear model
#'
#' @param data an object of the class \code{\link{alienData}}
#' @param formula A one-sided formula specifying how the different traits from both sets of species should be used to estimate species interactions. 
#' @param family The family of the response variable. See \link[stats]{family}, or the choices available.
#' @param spRandom Logical. Whether species are used as a random effect. Default is FALSE.
#' @param \dots Other parameters passed to \link[stats]{glm}.
#'
#' @details
#'
#' This function unfold the adjacency matrix and uses it as the response variable. As explanatory variables, the traits for each sets of species are repeated to match the length of the unfolded adjacency matrix but also the position.
#'
#' If there are NAs in the adjacency matrix, the function will omit these values in the estimation of the model.
#'
#' Although not specified by default formula is proposed here, in the ecological literature focusing on modeling species interactions, all variables are considered additively. In addition, quadratic relations are included for quantitative terms. Lastly, interactions between traits are considered across all traits within and across trophic levels. This is different than from the fourth corner approach where interactions is considered solely between traits of different trophic levels.
#'
#' @return
#'
#' An object with a class alienFit and a class fitGLM.
#'
#' @author
#'
#' F. Guillaume Blanchet, Dominique Gravel, Steve Vissault
#'
#' @importFrom stats terms update glm predict na.omit
#' @importFrom lme4 glmer
#' 
#' @export

fitGLM <- function(data, formula,
                   family = NULL, spRandom = FALSE, ...) {

  stopifnot(class(data) == "alienData")

  # Adjacency matrix
  adjMat <- data$adjMat
  nFromSp <- nrow(adjMat)
  nToSp <- ncol(adjMat)

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
#  if(is.null(formula)){
#    traitFromGP <- mvabund:::get.polys(traitFrom)
#    traitFrom <-  cbind(traitFromGP$X, traitFromGP$X.squ)
#
#    traitToGP <- mvabund:::get.polys(traitTo)
#    traitTo <-  cbind(traitToGP$X, traitToGP$X.squ)
#  }

  # Organize data into a single object
  dat <- cbind(adjVec, traitTo, traitFrom)

  # Row names for dat
  nameBase <- expand.grid(colnames(adjMat), rownames(adjMat))
  rNames <- paste(nameBase[,1], nameBase[,2],sep = "_")
  rownames(dat) <- rNames

  # Column names for dat
  colnames(dat)[1] <- c("adj")

  # Organize formula
#  if(is.null(formula)){
#    # Terms
#    formTerm <- terms(adj ~ .*., data = dat)

#    # formula
#    Formula <- update(formTerm, ~ .)
#  }else{
    formulaBase <- update(formula, adj ~ .)

    # Terms
    formTerm <- terms(formulaBase, data = dat)

    # formula
    Formula <- update(formTerm, ~ .)
#  }

  # GLM
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
  
  rownames(res) <- rownames(adjMat)
  colnames(res) <- colnames(adjMat)

  # Add model as attribute
  baseAttr <- attributes(res)
  attributes(res) <- list(dim = baseAttr$dim,
                          dimnames = baseAttr$dimnames,
                          model = model,
                          formula = Formula)

  # Define object class
  class(res) <- c("alienFit", "fitGLM")

  # Return results
  return(res)
}
