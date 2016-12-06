#' @name extract.bayesreg
#' @title Extract posterior distributions from Bayesian Regression Model
#'
#' @description A helper function for getting the chains from a JAGS model object outputted from fit.bayesreg
#'
#' @param x A JAGS model file from fit.bayesreg()
#' @author Ben Weinstein
#'
#' @references
#' Bartomeus et al. 2016. Functional Ecology.
#'
#' @rdname extract.bayesreg
#' @export

extract.bayesreg <- function(x) {
    parsO <- reshape2::melt(x$BUGSoutput$sims.array)
    colnames(parsO) <- c("Draw", "Chain", "parameter", "estimate")
    
    # label species and plants
    l <- levels(parsO$parameter)
    
    # parameters to saveC
    totrack <- x$parameters.to.save
    
    # assign species index to ragged frame.
    sp_pl <- data.frame(parameter = l, Index = as.numeric(stringr::str_match(l, pattern = "\\[(\\d+)]")[, 
        2L]), par = stringr::str_extract(l, "\\w+"))
    
    # merge levels
    pars <- merge(parsO, sp_pl)
    pars <- pars[!pars$par %in% "deviance", ]
    return(pars)
}
