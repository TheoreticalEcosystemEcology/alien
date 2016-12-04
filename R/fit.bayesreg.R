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
#' @param draws The number of MCMC draws to run before taking 100 samples from the posterior distribution
#' @details
#' Intercept Model
#' For each pair of species i interaction with species j
#' \deqn{ Obs_{i,j} \sim Binom(\rho_{i,j})}
#' \deqn{ logit(\rho_{i,j}) = \alpha_{i,j} }
#'
#' With a hierarchical relationship among species i (eg. pollinators)
#' \deqn{\alpha_{i,j} \sim Normal(\alpha_\mu,\alpha_\sigma)}
#'#' Intercept Model
#' For each pair of species i interaction with species j
#' \deqn{ Obs_{i,j} \sim Binom(\rho_{i,j})}
#' \deqn{ logit(\rho_{i,j}) = \alpha_{i,j} + \beta_{i,j}}
#'
#' With a hierarchical relationship among species intercepts and slopes i (eg. pollinators)
#' \deqn{\alpha_{i,j} \sim Normal(\alpha_\mu,\alpha_\sigma}
#' \deqn{\beta_{i,j} \sim Normal(\beta_\mu,\beta_\sigma}
#' @return A jags model obect (see package R2Jags)
#' @import coda
#' @export

fit.bayesreg <- function(dat, algorithm = "Binomial", draws = 10000) {
  
    #jags needs to be manually started
    library(coda)
    stopifnot(algorithm %in% c("Binomial", "Intercept", "Poisson","Multinomial","Occupancy"))
    
    # format traitmatch as matrix
    dat$Traitmatch <- abs(dat$TraitI - dat$TraitJ)
    Traitmatch <- reshape2::acast(data = dat, I ~ J, value.var = "Traitmatch", fun = mean)
    
    # legacy name change.
    runs <- draws
    
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
    nb <- max(0, runs - 100)  # number to discard for burn-in
    nc <- 2  # number of chains
    
    modelDat <- list("Yobs", "Bird", "Plant", "Plants", "Birds", "Nobs")
    
    if (algorithm == "Intercept") {
        # parameters to track
        ParsStage <- c("alpha", "alpha_mu", "ynew", "fit", "fitnew")
        # 
        modfile <- paste0(tempdir(), "/Intercept.jags")
        interceptToJags(modfile)
        # 
        m1 <- do.call(R2jags::jags.parallel, list(data = modelDat, model.file = modfile, 
            parameters.to.save = ParsStage, n.thin = nt, n.iter = ni, n.burnin = nb, 
            n.chains = nc, DIC = F))
    }
    if (algorithm == "Binomial") {
        # 
        modelDat <- list("Yobs", "Bird", "Plant", "Plants", "Birds", "Nobs", "Traitmatch")
        # Parameters to track
        ParsStage <- c("alpha", "beta", "alpha_mu", "alpha_sigma", "beta_sigma", 
            "beta_mu", "ynew", "fit", "fitnew")
        # 
        modfile <- paste0(tempdir(), "/Binomial.jags")
        binomialToJags(modfile)
        m1 <- do.call(R2jags::jags.parallel, list(data = modelDat, parameters.to.save = ParsStage, 
            model.file = modfile, n.thin = nt, n.iter = ni, n.burnin = nb, n.chains = nc, 
            DIC = F))
    }
    
    if (algorithm == "Poisson") {
        
        # Parameters to track
        ParsStage <- c("alpha", "beta", "alpha_mu", "alpha_sigma", "beta_sigma", 
            "beta_mu", "ynew", "fit", "fitnew")
        # jags file.
        modfile <- paste0(tempdir(), "/Poisson.jags")
        poissonToJags(modfile)
        # 
        m1 <- do.call(R2jags::jags.parallel(list(data = modelDat, parameters.to.save = ParsStage, 
            model.file = modfile, n.thin = nt, n.iter = ni, n.burnin = nb, n.chains = nc, 
            DIC = F)))
    }
    
    if (algorithm == "Multinomial") {
        
        #The total number of interactions
        N<-sum(dat$Interactions)
        
        #The total number of pairwise classes 
        k=Birds * Plants
        
        #The number of total visits per class
        classes<-dat %>% group_by(I,J) %>% summarise(totalvisits=sum(Interactions))
        tvisits=classes$totalvisits
        
        #Send data to jags
        modelDat <- list("N", "k", "tvisits")
        
        # Parameters to track
        ParsStage <- c("alpha", "p","fit","fitnew")
        
        # jags file.
        modfile <- paste0(tempdir(), "/Multinomial.jags")
        multinomialToJags(modfile)
        # 
        
        m1 <- do.call(R2jags::jags.parallel, list(data = modelDat, parameters.to.save = ParsStage, 
            model.file = modfile, n.thin = nt, n.iter = ni, n.burnin = nb, n.chains = nc, 
            DIC = F))
        
        #append classes
        m1$classes<-classes
    }
    if (algorithm == "Occupancy") {
      
      #encode replicate sampling.
      Time<-dat$Replicate
      Times <- max(dat$Replicate)
      
      modelDat <- list("Yobs","Ninit", "Bird", "Plant", "Plants", "Birds", "Nobs", "Traitmatch","Times","Time")
      
      #init unobserved variance
      Ninit<-array(dim=c(Birds,Plants,Times),data=1)
      InitStage <- function(){list(N=Ninit)}
      
      # Parameters to track
      ParsStage <- c("alpha", "beta", "alpha_mu", "alpha_sigma", "beta_sigma", 
                     "beta_mu", "ynew", "fit", "fitnew")
      
      # jags file.
      modfile <- paste0(tempdir(), "/Occupancy.jags")
      OccupancyToJags(modfile)
      # 
      
      m1 <- do.call(R2jags::jags.parallel, list(data = modelDat, parameters.to.save = ParsStage, inits=InitStage, 
                                                model.file = modfile, n.thin = nt, n.iter = ni, n.burnin = nb, n.chains = nc, DIC = F))
    }
    # Append the algorith and dataset, it will be helpful for later
    m1$Algorithm <- algorithm
    m1$data <- dat
    # 
    return(m1)
}
