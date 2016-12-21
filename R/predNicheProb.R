#' @name predNicheProb
#'
#' @title Predict new data points using traitmatch function generated with fit.niche.prob
#'
#' @description It generates predicted interactions based on new data.
#'
#' @param pars Parameters of the model obtained with fit.niche.prob function (alpha0, alpha 1, beta 0 and beta 1)
#' @param Tlevel1 Vector of trait values of the first interaction partner.
#' @param Tlevel2 Vector of trait values of the second interaction partner.
#' @param replicates Number of interactions to predict based on the interaction probability values
#'
#' @return A list. First item is the probability for each predicted interactions.
#' The next elements are as many predicted interaction as replicates.
#'
#' @note
#' See more here: \url{https://github.com/ibartomeus/trait_match}
#'
#' @author
#' Dominique Gravel and Ignasi Bartomeus
#'
#' @export
predNicheProb <- function(pars, Tlevel1, Tlevel2, replicates = 100) {
    out <- list()
    # Optimum and range
    o <- pars[1L] + pars[2L] * Tlevel2
    r <- pars[3L] + pars[4L] * Tlevel2
    
    # Compute the conditional
    pLM <- exp(-(o - Tlevel1)^2/2/r^2)
    
    # save outout
    out[[1L]] <- pLM
    # out[[2]] <- data.frame(Tlevel1, Tlevel2, pLM)
    for (i in 1:replicates + 1) {
        out[[i]] <- rbinom(length(Tlevel1), size = 1, prob = pLM)
    }
    return(out)
}
