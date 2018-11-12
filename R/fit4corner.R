#' @name fit4corner
#'
#' @title Fit fourth corner prediction model
#'
#' @description Fit fourth corner prediction model using either the approach proposed by Brown et al. (2014) or Ovaskainen et al. (2017)
#'
#' @param data an object of the class alienData, see as.alienData function.
#' @param class method to be applied on the data. Either 'mvabund' or 'HMSC' for the implementation of the 'mvabund' (\link[mvabund]{traitglm}) or the 'HMSC' (\link[HMSC]{hmsc}) R package, respectively.
#' @param family For 'mvabund', the family of the response variable can be defined using \link[stats]{family}. The negative binomial with unknown overdispersion and a log-link can be specified as "negative.binomial", and it is the default. For 'HMSC', use either 'probit', 'logit', 'gaussian', 'poisson', 'overPoisson'.
#' @param \dots Other parameters passed to either \link[mvabund]{traitglm} or \link[HMSC]{hmsc}.
#' 
#' @details 
#' 
#' The fourth corner prediction model is designed to be used on bipartite network where traits are available for both sets of species interacting in the network. It should not be used otherwise.
#' 
#' @author
#' F. Guillaume Blanchet
#' 
#' @importFrom mvabund traitglm
#' @importFrom HMSC hmsc
#'
#' @export


fit4corner <- function(data, class = "mvabund", family, ...){
  # General check
  stopifnot(class(data) == "alienData")
  
  # Format data
  bipartData <- getAdjacencyMatrix(data, bipartite = TRUE)
  
  traitBi1Raw <- data$dfNodes[]
  # mvabund
  if(class == "mvabund"){
    
  }

  # HMSC
  if(class == "HMSC"){
    
  }
  
}
  