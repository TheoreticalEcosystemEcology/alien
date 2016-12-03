#' @name predict.bayesreg
#' @aliases prediction bayesian regression
#'
#' @title Predict species interaction probabilities based on a fitted hierarchical bayesian regression 
#'
#' @description A helper functional for generating the probability of interaction among hypothetical species based on their traits.
#' The trait-matching function is currently |Trait_i - Trait_j|
#' @param x A JAGS model file return from fit.bayesreg
#' @param newdata A data frame with columns 'I','J' and 'Traitmatch'
#' @details The newdata object gives the trait-matching between species I (eg. pollinator) and species J (eg. plant)
#' @return A vector of probability of interaction values
#' @author
#' Ben Weinstein
#'
#' @references
#' Weinstein et al. 2016. Oikos
#'
#' @rdname predict.bayesreg
#' @export

predict.bayesreg <- function(x, newdata) {
    parsm1 <- extract_par(x)
    
    # matching function
    if (x$Algorithm == "Binomial") {
        # Trait-matching functions
        predfun <- function(alpha, beta, newdata) {
            data.frame(newdata, value = inv.logit(alpha + beta * newdata[["Traitmatch"]]))
        }
    }
    
    df <- parsm1 %>% filter(par %in% c("alpha_mu", "beta_mu")) %>% select(Draw, Chain, 
        par, estimate) %>% dcast(., Draw + Chain ~ par, value.var = "estimate") %>% 
        group_by(Draw, Chain) %>% do((predfun(alpha = .$alpha_mu, beta = .$beta_mu, 
        newdata))) %>% group_by(I, J) %>% summarize(mean = mean(value), lower = quantile(value, 
        0.95), higher = quantile(value, 0.05))
    
    return(df)
}

