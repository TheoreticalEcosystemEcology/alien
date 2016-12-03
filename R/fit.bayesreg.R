#' @name fit.bayesreg
#' @aliases bayesian regression
#' @author Ben Weinstein
#' @title Hierarchical Bayesian Regression for estimating trait-matching in species interaction data
#' @param dat A data frame with columns named 'I','J','Interactions','TraitI','TraitJ'.
#' I: The identity of the species on the upper level (eg. pollinators)
#' J: The identity of the species on the lower level (eg. plants)
#' Interactions: 1 (Observed) or 0 (non-detected)
#' @param algorithm A string argument, either 'Intercept' or 'Poisson', see details
#' Must have JAGS v 4.0 > to run. Install jags [here](http://mcmc-jags.sourceforge.net/)
#' #' @details
#' Intercept Model
#' For each pair of species i interaction with species j
#' $$ Obs_{i,j} \sim Binom(\rho_{i,j})$$
#' $$ logit(\rho_{i,j}) = \alpha_{i,j} $$
#'
#' With a hierarchical relationship among species i (eg. pollinators)
#' $$\alpha_{i,j} \sim Normal(\alpha_\mu,\alpha_\sigma)$$
#'#' Intercept Model
#' For each pair of species i interaction with species j
#' $$ Obs_{i,j} \sim Binom(\rho_{i,j})$$
#' $$ logit(\rho_{i,j}) = \alpha_{i,j} + \beta_{i,j}$$
#'
#' With a hierarchical relationship among species intercepts and slopes i (eg. pollinators)
#' $$\alpha_{i,j} \sim Normal(\alpha_\mu,\alpha_\sigma$$
#' $$\beta_{i,j} \sim Normal(\beta_\mu,\beta_\sigma$$
#' @example 
#' simdat<-sim.traitmatch(10,10,rpois(10,30),rpois(10,20))
#' 
#' @return A jags model obect (see package R2Jags)
#' @rdname fit.bayesreg
#' @export
fit.bayesreg <- function(dat, algorithm = "Binomial") {
    
    # format traitmatch as matrix
    dat$Traitmatch <- abs(dat$TraitI - dat$TraitJ)
    Traitmatch <- reshape2::acast(data = dat, I ~ J, value.var = "Traitmatch")
    
    if (algorithm == "Intercept") {
        
        runs <- 10000
        
        # for parallel run
        Yobs <- dat$Interactions
        Bird <- as.numeric(as.factor(dat$I))
        Plant <- as.numeric(as.factor(dat$J))
        Birds <- length(unique(dat$I))
        Traitmatch <- Traitmatch
        Plants <- length(unique(dat$J))
        Nobs <- length(Yobs)
        
        # MCMC options
        ni <- runs  # number of draws from the posterior
        nt <- 1  #thinning rate
        nb <- max(0, runs - 500)  # number to discard for burn-in
        nc <- 2  # number of chains
        
        modelDat <- list("Yobs", "Bird", "Plant", "Plants", "Birds", "Nobs")
        
        ParsStage <- c("alpha", "alpha_mu", "ynew", "fit", "fitnew")
        
        # file path
        modfile <- paste(system.file(package = "alienR"), "/Bayesian/Intercept.jags", 
            sep = "")
        
        m1 <- do.call(R2jags::jags.parallel, list(data = modelDat, model.file = modfile, 
            parameters.to.save = ParsStage, n.thin = nt, n.iter = ni, n.burnin = nb, 
            n.chains = nc, DIC = F))
    } else if (algorithm == "Binomial") {
        
        runs <- 10000
        
        # for parallel run
        Yobs <- dat$Interactions
        Bird <- as.numeric(as.factor(dat$I))
        Plant <- as.numeric(as.factor(dat$J))
        Birds <- length(unique(dat$I))
        Traitmatch <- Traitmatch
        Plants <- length(unique(dat$J))
        Nobs <- length(Yobs)
        
        # Parameters to track
        ParsStage <- c("alpha", "beta", "alpha_mu", "alpha_sigma", "beta_sigma", 
            "beta_mu", "ynew", "fit", "fitnew")
        
        # MCMC options
        ni <- runs  # number of draws from the posterior
        nt <- 1  #thinning rate
        nb <- max(0, runs - 500)  # number to discard for burn-in
        nc <- 2  # number of chains
        
        modelDat <- list("Yobs", "Bird", "Plant", "Plants", "Traitmatch", "Birds", 
            "Nobs")
        
        modfile <- paste(system.file(package = "alienR"), "/Bayesian/Binomial.jags", 
            sep = "")
        
        m1 <- do.call(R2jags::jags.parallel, list(data = modelDat, parameters.to.save = ParsStage, 
            model.file = modfile, n.thin = nt, n.iter = ni, n.burnin = nb, n.chains = nc, 
            DIC = F))
    }
    
    m1$Algorithm <- algorithm
    return(m1)
}

