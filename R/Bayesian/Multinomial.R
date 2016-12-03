
sink("Bayesian/Multinomial.jags")

cat("C
    B<-Birds
    P<-PlantsC
    
    model {

    for (x in 1:Nobs){
    
    # Observed State
    Yobs[x] ~ dmulti(size=TotalObs,prob=alpha[Bird[x],Plant[x]]) 
    
    #Assess Model Fit
    #Fit discrepancy statistics
    eval[x]<-lambda[Bird[x],Plant[x]]
    E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)
    
    #Prediction
    ynew[x]~dpois(lambda[Bird[x],Plant[x]])
    E.new[x]<-pow((ynew[x]-eval[x]),2)/(eval[x]+0.5)
    
    }
    
    ###Priors
    
    #Process Model
    
    #Species level priors
    for (i in 1:Birds){
    
    #Intercept
    alpha[i] ~ dnorm(alpha_mu,alpha_tau)
    
    #Traits slope 
    beta1[i] ~ dnorm(beta1_mu,beta1_tau)    
    }
    
    #Group process priors
    
    #Intercept 
    alpha_mu ~ dnorm(0,0.001)
    alpha_tau ~ dunif(0,10)
    alpha_sigma<-pow(1/alpha_tau,0.5) 
    
    #Trait
    beta1_mu~dnorm(0,0.001)
    beta1_tau ~ dunif(0,10)
    beta1_sigma<-pow(1/beta1_tau,0.5)
    
    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    
    
    }
    ",fill=TRUE)

sink()

