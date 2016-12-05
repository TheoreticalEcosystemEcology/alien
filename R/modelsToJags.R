#' Write models as JAGS file.
#'
#' Write one of the available model into a \code{.jags} file.
#'
#' @author
#' Ben Weistein
#'
#' @param filename A connection, or a character string naming the file to print to.
#'
#' @return
#' A \code{.jags} file including the desired model.
#' @describeIn modelsToJags Binomial model.
#' @export
binomialToJags <- function(filename) {
    cat("model {
      #Compute intensity for each pair of species
      for (i in 1:Birds){
        for (j in 1:Plants){
          #Binomial Model with logit regression
          logit(phi[i,j])<-alpha[i] + beta[i] * Traitmatch[i,j]
        }
      }
      #Prediction
      for (x in 1:Nobs){
        # Observed State
        Yobs[x] ~ dpois(phi[Bird[x],Plant[x]])
        #Assess Model Fit
        #Fit discrepancy statistics
        eval[x]<-phi[Bird[x],Plant[x]]
        E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)
        ynew[x]~dpois(phi[Bird[x],Plant[x]])
        E.new[x]<-pow((ynew[x]-eval[x]),2)/(eval[x]+0.5)
      }
      ###Priors
      #Process Model
      #Species level priors
      for (i in 1:Birds){
        #Intercept
        alpha[i] ~ dnorm(alpha_mu,alpha_tau)
        #Traits slope
        beta[i] ~ dnorm(beta_mu,beta_tau)
      }
      #Group process priors
      #Intercept
      alpha_mu ~ dnorm(0,0.001)
      alpha_tau ~ dunif(0,10)
      alpha_sigma<-pow(1/alpha_tau,0.5)
      #Trait
      beta_mu~dnorm(0,0.001)
      beta_tau ~ dunif(0,10)
      beta_sigma<-pow(1/beta_tau,0.5)
      #derived posterior check
      fit<-sum(E[]) #Discrepancy for the observed data
      fitnew<-sum(E.new[])
      }",
        file = filename, fill = TRUE)
}

#' @describeIn modelsToJags Intercept model.
#' @export
interceptToJags <- function(filename) {
    cat("model {
    #Compute intensity for each pair of birds and plants
    for (i in 1:Birds){
      for (j in 1:Plants){
        #Process Model with log normal overdispersion
        logit(lambda[i,j])<-alpha[i,j]
      }
    }
    #Prediction
    for (x in 1:Nobs){
      # Observed State
      Yobs[x] ~ dpois(lambda[Bird[x],Plant[x]])
      #Assess Model Fit
      #Fit discrepancy statistics
      eval[x]<-lambda[Bird[x],Plant[x]]
      E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)
      ynew[x]~dpois(lambda[Bird[x],Plant[x]])
      E.new[x]<-pow((ynew[x]-eval[x]),2)/(eval[x]+0.5)
    }
    ###Priors
    #Process Model
    #Species level priors
    for (i in 1:Birds){
      for (j in 1:Plants){
      #Intercept
      alpha[i,j] ~ dnorm(alpha_mu,alpha_tau)
      }
    }
    #Group process priors
    #Intercept
    alpha_mu ~ dnorm(0,0.001)
    alpha_tau ~ dunif(0,10)
    alpha_sigma<-pow(1/alpha_tau,0.5)
    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    }",
        file = filename, fill = TRUE)
}

#' @describeIn modelsToJags Multinomial model.
#' @export
multinomialToJags <- function(filename) {
    cat("model {
        
        #The total visits of a given class is a multinomial draw from a vector of probabilities 1:k classes
        tvisits[1:k] ~ dmulti(p[1:k],N)

        #Those probabilities come from a dirichlet, such that they sum to one
        p[1:k] ~ ddirch(alpha[1:k])

        #Assess Model Fit
        #Fit discrepancy statistics
        eval<- p[1:k] * N
        E<-pow((tvisits[1:k]-eval),2)/(eval+0.5)

        #Prediction
        ynew~dmulti(p[1:k],N)
        E.new<-pow((ynew-eval),2)/(eval+0.5)

        #Priors
        # The probability prior is equal probability among any two classes.

        for (i in 1:k){
        alpha[i]<-1/k
        }

        #derived posterior check
        fit<-sum(E[]) #Discrepancy for the observed data
        fitnew<-sum(E.new[]) #Discrepancy for the new data


        }
        ",
        file = filename, fill = TRUE)
}

#' @describeIn modelsToJags Poisson model.
#' @export
poissonToJags <- function(filename) {
    cat("model {

        #Compute intensity for each pair of birds and plants
        for (i in 1:Birds){
        for (j in 1:Plants){

        #Process Model with log normal overdispersion
        log(lambda[i,j])<-alpha[i] + beta1[i] * Traitmatch[i,j]
          }
        }

        #Prediction
        for (x in 1:Nobs){

        # Observed States
        Yobs[x] ~ dpois(lambda[Bird[i],Plant[i]])

        #Assess Model Fit
        #Fit discrepancy statistics
        eval[x]<-lambda[Bird[x],Plant[x]]
        E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)

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
        }",
        file = filename, fill = TRUE)
}
#' @describeIn modelsToJags Occupancy model.
#' @export
OccupancyToJags <- function(filename) {
  
  cat("model {
    #Compute intensity for each pair of birds and plants
    for (i in 1:Birds){
      for (j in 1:Plants){
        for (k in 1:Times){
          
          #Process Model with log normal overdispersion
          #mean intensity
          
          #log transformed variance
          logit(phi[i,j,k]) <- alpha[i] + beta1[i] * Traitmatch[i,j]
          
          #For each Time - there is a latent count, log transformed intensity
          N[i,j,k] ~ dbern(phi[i,j,k])
        }
      }
    }
    
    #Observed counts for each day of sampling at that Time
    for (x in 1:Nobs){
      
      #Observation Process
      #The effective probability of observation is the true state * detection prob
      p.eff[Bird[x],Plant[x],Time[x]]<-N[Bird[x],Plant[x],Time[x]] * detect[Bird[x]]
      Yobs[x] ~ dbern(p.eff[Bird[x],Plant[x],Time[x]]) 
      
      #Assess Model Fit
      
      #Fit discrepancy statistics
      eval[x]<-detect[Bird[x]]*N[Bird[x],Plant[x],Time[x]]
      E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)
      
      ynew[x]~dbin(detect[Bird[x]],N[Bird[x],Plant[x],Time[x]])
      E.new[x]<-pow((ynew[x]-eval[x]),2)/(eval[x]+0.5)
      
    }
    
    ###Priors###
    #Observation model
    #Detect priors, logit transformed - Following lunn 2012 p85
    
    for(x in 1:Birds){
      #For Cameras
      logit(detect[x])<-dcam[x]
      dcam[x]~dnorm(omega,omega_tau)
    }
    
    #Process Model
    #Species level priors
    for (i in 1:Birds){
      
      #Intercept
      alpha[i] ~ dnorm(alpha_mu,alpha_tau)
      
      #Traits slope 
      beta1[i] ~ dnorm(beta1_mu,beta1_tau)    
    }
    
    #Group Detection Prior
    omega ~ dnorm(0,0.386)
    omega_tau ~ dt(0,1,1)I(0,)
    
    #Group process priors
    
    #Intercept 
    alpha_mu ~ dnorm(0,0.01)
    alpha_tau ~ dt(0,1,1)I(0,)
    alpha_sigma<-pow(1/alpha_tau,0.5) 
    
    #Trait
    beta1_mu~dnorm(0,0.01)
    beta1_tau ~ dt(0,1,1)I(0,)
    beta1_sigma<-pow(1/beta1_tau,0.5)
    
    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    
    
  }",
  file = filename, fill = TRUE)
  }
