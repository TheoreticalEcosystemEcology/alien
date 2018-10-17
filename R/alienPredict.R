#' @title Format alien output.
#'
#' @description Format alien output and return an object of class \code{alienPredict}.
#'
#' @param logLik Likelihood log-transformed.
#' @param netEstim a probabilistic network.
#' @param ... other arguments to be appended to the \code{alienPredict} object.
#'
#' @details
#' \code{alienPredict} aims at structuring the output of alien package to ease
#' cross-comparison among methods.
#'
#' @return
#' An object of the class \code{alienPredict} is returned.
#'
#' @author Kevin Cazelles & Steve Vissault
#'
#'
#' @keywords manip
#' @keywords classes
#' @export


alienPredict <- function(logLik, netEstim, ...) {
    out <- list()
    out$logLik <- logLik
    ##--
    stopifnot(all(netEstim <= 1) & all(netEstim >= 0))
    out$netEstim <- netEstim
    out$connec <- getConnectance(netEstim)
    ##--
    out$methodsSpecific <- list(...)
    ##--
    class(out) <- "alienPredict"
    out
}
