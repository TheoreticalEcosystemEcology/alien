#' @name sim.traitmatch.R
#' @aliases trait-matching
#'
#' @title Simulating trait-matching among species
#'
#' @description A helper functional for generating the probability of interaction among hypothetical species based on their traits.
#' The trait-matching function is currently |Trait_i - Trait_j|
#' @param size_x The number of species in the lower level (eg. plants)
#' @param size_y The number of species in the upper level (eg. pollinators)
#' @param traits_x Vector of trait values for species in the lower level.
#' @param traits_y Vector of trait values for species in the lower level.
#' @param beta_sigma Variance among species in trait-matching slope.
#' @param alpha_sigma Variance among species in trait-matching intercept.
#'
#' @author
#' Ben Weinstein
#'
#' @references
#' Bartomeus et al. 2016. Functional Ecology.
#'
#'
#' @rdname sim.traitmatch
#' @export

sim.traitmatch <- function(size_x, size_y, traits_x, traits_y, beta_sigma = 0.4, 
    alpha_sigma = 0) {
    
    # Subtract both and take absolute value, convert cm
    traitmatch <- abs(sapply(traits_y, function(x) x - traits_x))
    
    # regression slope traits
    beta_mu <- -1
    
    # species variance in slopes
    beta_sigma <- 0.1
    
    # Species alpha_mu
    alpha_mu <- 3
    alpha_sigma <- 0
    
    # species level
    beta1 <- rnorm(size_x, beta_mu, beta_sigma)
    alpha <- rnorm(size_x, alpha_mu, alpha_sigma)
    
    # for each species loop through and create a replicate dataframe
    obs <- array(dim = c(size_x, size_y))
    N <- array(dim = c(size_x, size_y))
    
    # create intensities
    for (x in 1:size_x) {
        for (y in 1:size_y) {
            
            # intensity
            N[x, y] <- boot::inv.logit(alpha[x] + beta1[x] * traitmatch[x, y])
            
            # draw one state
            obs[x, y] <- rbinom(1, 1, N[x, y])
        }
    }
    
    
    # draw intensity
    tx <- data.frame(I = 1:length(traits_x), TraitI = traits_x)
    ty <- data.frame(J = 1:length(traits_y), TraitJ = traits_y)
    
    # view trait matching
    dat <- reshape2::melt(obs)
    colnames(dat) <- c("I", "J", "Interactions")
    
    dat <- merge(dat, tx)
    dat <- merge(dat, ty)
    
    # define data types, make species
    dat$I <- letters[dat$I]
    dat$J <- letters[dat$J]
    
    return(dat)
}
