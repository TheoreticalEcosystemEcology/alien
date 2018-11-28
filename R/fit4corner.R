#' @name fit4corner
#'
#' @title Fit fourth corner prediction model
#'
#' @description Fit fourth corner prediction model using either the approach proposed by Brown et al. (2014) or Ovaskainen et al. (2017)
#'
#' @param data an object of the class alienData, see as.alienData function.
#' @param formulaFrom formula to construct the trait variables associated to the "From" organisms. Default is "~ .".
#' @param formulaTo formula to construct the trait variables associated to the "To" organisms. Default is "~ .".
#' @param level Either "species" or "individuals". Whether the analysis should be done at the species or the individual levels. Default is "individual" (FOR NOW!). (See details)
#' @param class method to be applied on the data. Either 'mvabund' or 'HMSC' for the implementation of the 'mvabund' (\link[mvabund]{traitglm}) or the 'HMSC' (\link[HMSC]{hmsc}) R package, respectively.
#' @param family For 'mvabund', the family of the response variable can be defined using \link[stats]{family}. The negative binomial with unknown overdispersion and a log-link can be specified as "negative.binomial", and it is the default. For 'HMSC', use either 'probit', 'logit', 'gaussian', 'poisson', 'overPoisson'.
#' @param priors An object of class \code{\link[HMSC]{HMSCpriors}}. If NULL, the function will generate flat priors to estimate the model. This argument is active only when "HMSC" is used.
#' @param iniParam An object of class \code{\link[HMSC]{HMSCparam}}. If NULL, the function will generate initial parameters ramdomly. This argument is active only when "HMSC" is used.
#' @param niter A positive integer defining the total number of iterations to be carried out in the analysis. Default is 2000. This argument is active only when "HMSC" is used.
#' @param nburn A positive integer defining the number of iterations to be used in the burning (first) phase of the algorithm. The burning iterations are a fraction of \code{niter}. This argument is active only when "HMSC" is used.
#' @param thin A positive integer defining thinning. Default is 1 (see details in \link[HMSC]{hmsc}).
#' @param \dots Other parameters passed to either \link[mvabund]{traitglm} or \link[HMSC]{hmsc}.
#' 
#' @details 
#' 
#' The fourth corner models are designed to be used on bipartite network where traits are available for both sets of species interacting in the network. It should not be used otherwise.
#' 
#' The fourth corner models assume that adjacency matrix (species by species matrix) used to perform the analysis has as rows the "From" species and as columns the "To" species.
#' 
#' The arguments \code{formulaFrom} and \code{formulaTo} should take the form \code{~ x + y * z}, that is, the left side of the equation should not be given. Also, note that the default formulas always include an intercept.
#' 
#' The argument \code{level} takes into account species if the data considered has multiple measures for the same species. In this case the mean (numeric variable) or the dominant level (factor) will be considered when gathering the data by species.
#' 
#' @author
#' F. Guillaume Blanchet
#' 
#'
#' @export


fit4corner <- function(data, formulaFrom = "~ .", formulaTo = "~ .",
                       level = "individual", class = "HMSC",
                       family = NULL, priors = NULL, iniParam = NULL, ...){
  # General check
  stopifnot(class(data) == "alienData")
  
  #############
  # Format data
  #############
  # Adjacency matrix
  adjData <- getAdjacencyMatrix(data, level = level, bipartite = TRUE)
  
  # "Raw " trait data
  traits <- getTraitMatrix(data, level = level, bipartite = TRUE)
  
  # Trait data from
  traitsFrom <- stats::model.matrix(as.formula(formulaFrom), data = traits$from)
  
  # Trait data to
  traitsTo <- stats::model.matrix(as.formula(formulaTo), data = traits$to)
  
  # mvabund
  if(class == "mvabund"){
#    res <- mvabund::traitglm(adjData, traitsFrom, traitsTo,
#                             family = family, ...)
    stop("The implementation with 'mvabund' is not yet implemented... it will come soon... I hope")
  }

  # HMSC
  if(class == "HMSC"){
    Data <- HMSC::as.HMSCdata(Y = adjData, X = traitsFrom,
                              Tr = t(traitsTo), scaleX = FALSE,
                              scaleTr = FALSE,
                              interceptX = FALSE,
                              interceptTr = FALSE)
    if(is.null(priors)){
      priors <- HMSC::as.HMSCprior(Data, family = family)
    }
    if(is.null(iniParam)){
      iniParam <- HMSC::as.HMSCparam(Data, priors)
    }
      
    res <- HMSC::hmsc(Data, param = iniParam, priors = priors,
                      family = family, niter = niter,
                      nburn = nburn, thin = thin, ...)
  }
  
  return(res)
}
  