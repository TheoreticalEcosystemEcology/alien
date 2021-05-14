#' @name fit4corner
#'
#' @title Fit fourth corner prediction model
#'
#' @description Fit fourth corner prediction model using the approach proposed by Brown et al. (2014). This function is essentially a wrapper around \code{\link[mvabund]{traitglm}} for it to be used in the context of species interaction data.
#'
#' @param data an object of the class \code{\link{alienData}}.
#' @param formula A one-sided formula specifying exactly how to model the adjacency matrix as a function of the "From" and "To" traits variables of both sets of species.
#' @param family an object of class \code{\link[stats]{family}} with the caveat that the negative binomial with unknown overdispersion and a log-link can be specified as "negative.binomial", and it is the default..
#' @param \dots Other parameters passed to \code{\link[mvabund]{traitglm}}.
#'
#' @details
#'
#' Although not specified by default here, in the ecological literature focusing on modeling species interactions, all variables are considered additively. In addition, quadratic relations are included for quantitative terms. Lastly, interactions between traits are considered across traits from different trophic levels.
#'
#' @return
#'
#' An object with a class alienFit and a class fit4corner.
#'
#' @author
#' F. Guillaume Blanchet
#'
# @importFrom mvabund traitglm
# @export

fit4corner <- function(data, formula = NULL, family = NULL, ...){
  # General check
  stopifnot(class(data) == "alienData")

  # Adjacency matrix
  adjMat <- data$adjMat

  # Trait
  traitsFrom <- data$traitFrom
  traitsTo <- data$traitTo

  # Perform traitglm analysis
  model <- mvabund::traitglm(adjMat, traitsFrom, traitsTo,
                             family = family, formula = formula)

  # Prediction
  res <- predict(model)
  rownames(res) <- rownames(adjMat)
  colnames(res) <- colnames(adjMat)

  # Add model as attribute
  baseAttr <- attributes(res)
  attributes(res) <- list(dim = baseAttr$dim,
                          dimnames = baseAttr$dimnames,
                          model = model)

  # Define object class
  class(res) <- c("alienFit", "fit4corner")

  return(res)
}
