#' @name fit4corner
#'
#' @title Fit fourth corner prediction model
#'
#' @description Fit fourth corner prediction model using the approach proposed by Brown et al. (2014). This function is essentially a wrapper around \code{\link[mvabund]{traitglm}} for it to be used in the context of species interaction data.
#'
#' @param data an object of the class \code{\link{alienData}}.
#' @param formula A one-sided formula specifying exactly how to model the adjacency matrix as a function of the "From" and "To" traits variables of both sets of species. Default is to include all terms additively, with quadratics for quantitative terms, and all From-by-To traits interactions.
#' @param family an object of class \code{\link[stats]{family}} with the caveat that the negative binomial with unknown overdispersion and a log-link can be specified as "negative.binomial", and it is the default..
#' @param \dots Other parameters passed to \code{\link[mvabund]{traitglm}}.
#'
#' @details
#'
#' Fourth corner models are designed to be used on bipartite network where traits are available for both sets of species interacting in the network. It can not be used otherwise.
#'
#' Fourth corner models assume that adjacency matrix (species by species matrix) used to perform the analysis has as rows the "From" species and as columns the "To" species.
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
