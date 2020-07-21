#' @name fitPNB
#'
#' @title Fit using Probabilistic niche model
#'
#' @description Model adegency matrix using probabilistic niche model
#'
#' @param data An object of the class alienData, see \code{\link{alienData}}.
#' @param type Method to be used to estimate the model. Either 'P' (presence-only) or 'PA' (presence-absence), respectively.
#' @param optimum A vector of two values defining the optimum intercept (first value) and slope (second value). 
#' @param optimumMin A vector of two values giving the minimum values the optimum intercept and slope can have, respectively. 
#' @param optimumMax A vector of two values giving the maximum values the optimum intercept and slope can have, respectively. 
#' @param range A vector of two values defining the range intercept (first value) and slope (second value). 
#' @param rangeMin A vector of two values giving the minimum values the range intercept and slope can have, respectively. 
#' @param rangeMax A vector of two values giving the maximum values the range intercept and slope can have, respectively. 
#' @param verbose Logical. Whether messages from the algorithm are shown (TRUE) or not (FALSE). Default is TRUE.
#'
#' @details
#'
#' This function is only designed to handle presence-only and presence-absence data. In addition, the function can only handle a single continuous trait for each species.
#' 
#' If there are any NAs in the species interaction data (the adjacency matrix), they will be automatically removed to estimate the presence-absence (PA) model parameters. If there are NAs in the traits, an error message will be sent.
#'
#' @author
#' 
#' Dominique Gravel and F. Guillaume Blanchet
#'
#' @return
#'
#' An object with a class alienFit and a class fitPNB.
#'
#' @importFrom GenSA GenSA
#' 
#' @export
fitPNB <- function(data, type, optimum, optimumMin, optimumMax,
                   range, rangeMin, rangeMax, verbose = TRUE){

  stopifnot(class(data) == "alienData")
  
  # Construct adjencency matrix
  adjMat <- data$adjMat
  nFromSp <- ncol(adjMat)
  nToSp <- nrow(adjMat)
  
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
  
  # Check number of traits for From and To
  if(ncol(traitsTo) > 1){
    stop("For this analysis there should be only a single 'To' trait")
  }
  colnames(traitsTo) <- "To"
  
  if(ncol(traitsFrom) > 1){
    stop("For this analysis there should be only a single 'From' trait")
  }
  colnames(traitsFrom) <- "From"
  
  # Choose the probabilitic model to use
  if(type == "P"){
    # Organize data into a single object
    dat <- data.frame(To = traitsTo, From = traitsFrom)
    
    # Keep only the data on where an interaction was found
    dat <- dat[adjVec == 1,]
    
    minFunc <- nicheFuncPres
    
    # Estimate parameters using simulated annealing
    estimPars <- GenSA::GenSA(par = c(optimum, range), fn = minFunc, 
                              lower = c(optimumMin, rangeMin), 
                              upper = c(optimumMax, rangeMax), 
                              control = list(verbose = verbose, smooth=FALSE), 
                              traitsFrom = dat$From, traitsTo = dat$To)
  }
  if(type == "PA"){
    # Remove NAs in adjVec and the associated trait data
    noNALoc <- which(!is.na(adjVec))
    adjVecNoNA <- adjVec[noNALoc]

    # Define the function to use
    minFunc <- nicheFuncPresAbs

    # Estimate parameters using simulated annealing
    estimPars <- GenSA::GenSA(par = c(optimum, range), fn = minFunc, 
                              lower = c(optimumMin, rangeMin), 
                              upper = c(optimumMax, rangeMax), 
                              control = list(verbose = verbose, smooth=FALSE), 
                              traitsFrom = traitsFrom, traitsTo = traitsTo,
                              adjVec = adjVecNoNA)
  }
  
  # Prediction
  optimumPred <- estimPars[1] + estimPars[2] * traitsTo  
  rangePred <- estimPars[3] + estimPars[4] * traitsTo  
  
  res <- exp(-(optimumPred - traitsFrom)^2 / (2 * rangePred^2))
  
  # Add model as attribute
  baseAttr <- attributes(res)
  attributes(res) <- list(dim = baseAttr$dim,
                          dimnames = baseAttr$dimnames,
                          adjMat = adjMat)
  
  # Define object class
  class(res) <- c("alienFit", "fitPNB")
  
  # Return result
  return(res)
}

# Presence-only data
nicheFuncPres <- function(pars, traitsFrom, traitsTo) {

  # Optimum and range
  Optimum <- pars[1] + pars[2] * traitsTo  
  Range <- pars[3] + pars[4] * traitsTo
  
  # Compute the conditional
  pLM <- exp(-(Optimum - traitsFrom)^2 /( 2 * Range^2))

  # Compute the marginal
  pM <- 1/(max(traitsFrom) - min(traitsFrom))
  
  #  Integrate the denominator
  pL <- Range / sqrt(pi)
  
  # Compute the posterior probability
  pML <- pLM * pM / pL
  pML[pML <= 0] <- .Machine$double.xmin # Control to avoid computing issues
  
  return(-sum(log(pML)))
}

# Presence-absence data
nicheFuncPresAbs <- function(pars, traitsFrom, traitsTo, adjVec, na.rm = TRUE) {
  
  # Optimum and range
  Optimum <- pars[1] + pars[2] * traitsTo  
  Range <- pars[3] + pars[4] * traitsTo
  
  # Compute the interaction probility
  pL <- exp(-(Optimum - traitsFrom)^2 / ( 2 * Range^2))
  
  # Compute the log-likelihood
  ll <- adjVec*0
  ll[adjVec == 1] <- log(pL[adjVec == 1])
  ll[adjVec == 0] <- log(1 - pL[adjVec == 0])
  
  return(-sum(ll))		
}
