#' @title Format IMC output.
#'
#' @description Format IMC output and return an object of class \code{IMCPredict}.
#'
#' @param logLik Likelihood log-transformed.
#' @param netEstim a probabilistic network.
#' @param ... other arguments to be appended to the \code{alienPredict} object.
#'
#' @details
#' \code{IMCPredict} aims at structuring the output of the IMC function.
#'
#' @return
#' An object of the class \code{IMCPredict} is returned.
#'
#' @author Kevin Cazelles & Steve Vissault
#'
#'
#' @keywords internal
#' 


IMCPredict <- function(logLik, netEstim, ...) {
    out <- list()
    out$logLik <- logLik
    ##--
    stopifnot(all(netEstim <= 1) & all(netEstim >= 0))
    out$netEstim <- netEstim
    out$connec <- getConnectance(netEstim)
    ##--
    out$methodsSpecific <- list(...)
    ##--
    class(out) <- "IMCPredict"
    out
}
