#' @name residualsNicheProb
#'
#' @title get the residuals from a fitted nicheProb
#'
#' @description get the residuals from a fitted nicheProb
#'
#' @param pars Parameters of the model obtained with fitNicheProb function (alpha0, alpha 1, beta 0 and beta 1)
#' @param Tlevel1 Original vector of trait values of the first interaction partner.
#' @param Tlevel2 Original vector of trait values of the second interaction partner.
#'
#' @return a vector with the residual values for each interaction.
#'
#'
#' @author
#' Ignasi Bartomeus and Dominique Gravel
#'
#' @export
residualsNicheProb <- function(pars, Tlevel1, Tlevel2) {
    pred <- predNicheProb(pars, Tlevel1, Tlevel2, replicates = 1)[[1L]]
    return(1 - pred)
}
