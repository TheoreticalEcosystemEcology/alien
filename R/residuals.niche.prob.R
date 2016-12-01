#' @name residuals.niche.prob
#'
#' @title get the residuals from a fitted niche.prob
#'
#' @description get the residuals from a fitted niche.prob
#'
#' @param pars Parameters of the model obtained with fit.niche.prob function (alpha0, alpha 1, beta 0 and beta 1)
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
residuals.niche.prob <- function(pars,Tlevel1, Tlevel2){
  pred <- predict.niche.prob(pars, Tlevel1, Tlevel2, replicates = 1)[[1]]
  1-pred
}

