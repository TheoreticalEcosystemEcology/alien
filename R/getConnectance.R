#' @title Compute the connectance of a probabilistic network.
#'
#' @description Compute the connectance of a probabilistic network and its variance.
#'
#' @param netEstim a probabilistic network.
#'
#' @return
#' An object of the class \code{alienPredict} is returned .
#'
#' @author Kevin Cazelles & Steve Vissault
#'
#' @references
#' Poisot, T. et al. The structure of probabilistic networks. Methods Ecol. Evol. 7, 303–312 (2016).
#'
#' @keywords connectance
#' @export
#'
#' @examples
#' getConnectance(rep(.5,10))

getConnectance <- function(netEstim) {
    list(expectation = sum(netEstim), variance = sum(netEstim * (1 - netEstim)))
}
