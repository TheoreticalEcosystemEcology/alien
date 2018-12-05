#' @name fitPNB
#'
#' @title Fit using Probabilistic niche model
#'
#' @description Model adegency matrix using K-nearest neighbour approach
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
#' For now this function is only designed to handle presence-only and presence-absence data. 
#'
#' @author
#' 
#' Dominique Gravel and F. Guillaume Blanchet
#'
#' @return
#'
#' A matrix giving the the probability of interaction among each pairs of species.
#'
#' @export
fitPNB <- function(data, type, optimum, optimumMin, optimumMax,
                   range, rangeMin, rangeMax, verbose = TRUE){

  
  
  ## Doit être retravailler... Je crois qu'il y a un bug dans le code ou du moins elle doit être généralisé
  ## Doit être retravailler... Je crois qu'il y a un bug dans le code ou du moins elle doit être généralisé
  ## Doit être retravailler... Je crois qu'il y a un bug dans le code ou du moins elle doit être généralisé
  ## Doit être retravailler... Je crois qu'il y a un bug dans le code ou du moins elle doit être généralisé
  ## Doit être retravailler... Je crois qu'il y a un bug dans le code ou du moins elle doit être généralisé
  ## Doit être retravailler... Je crois qu'il y a un bug dans le code ou du moins elle doit être généralisé
  ## Doit être retravailler... Je crois qu'il y a un bug dans le code ou du moins elle doit être généralisé
  ## Doit être retravailler... Je crois qu'il y a un bug dans le code ou du moins elle doit être généralisé
  ## Doit être retravailler... Je crois qu'il y a un bug dans le code ou du moins elle doit être généralisé
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
  
  # Choose the probabilitic model to use
  if(type == "P"){
    # Organize data into a single object
    dat <- cbind(adjVec, traitsTo, traitsFrom)
    
    # Keep only the data on where an interaction was found
    dat <- dat[dat[,1] == 1,]
    
    minFunc <- nicheFuncPres
  }
  if(type == "PA"){
    # Organize data into a single object
    dat <- cbind(adjVec, traitsTo, traitsFrom)
    
    minFunc <- nicheFuncPresAbs
  }
}

# Presence-only data
nicheFuncPres <- function(pars, Tlevel1, Tlevel2) {

  # Optimum and range
  Optimum <- pars[1] + pars[2] * Tlevel2  
  Range <- pars[3] + pars[4] * Tlevel2
  
  # Compute the conditional
  pLM <- exp(-(Optimum - Tlevel1)^2 / 2 / Range^2)
  
  # Compute the marginal
  pM <- 1/(max(Tlevel1) - min(Tlevel1))
  
  #  Integrate the denominator
  pL <- Range / sqrt(pi)
  
  # Compute the posterior probability
  pML <- pLM * pM / pL
  pML[pML <= 0] <- .Machine$double.xmin # Control to avoid computing issues
  
  return(-sum(log(pML)))
}

# Presence-absence data
nicheFuncPresAbs <- function(pars, Tlevel1, Tlevel2, L) {
  
  a0 = pars[1]
  a1 = pars[2]
  b0 = pars[3]
  b1 = pars[4]
  
  # Optimum and range
  o = a0 + a1*Tlevel2  
  r = b0 + b1*Tlevel2
  
  # Compute the interaction probility
  pL = exp(-(o-Tlevel1)^2/2/r^2)
  
  # Compute the log-likelihood
  ll = L*0
  ll[L == 1] = log(pL[L == 1])
  ll[L == 0] = log(1-pL[L == 0])
  
  return(-sum(ll))		
}
