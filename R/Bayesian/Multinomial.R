
sink("Bayesian/Multinomial.jags")

cat("
  model {
  
    #The total visits of a given class is a multinomial draw from a vector of probabilities 1:k classes
    tvisits[1:k] ~ dmulti(p[1:k],N) 

    #Those probabilities come from a dirichlet, such that they sum to one
    p[1:k] ~ ddirch(alpha[1:k])
    
    #Assess Model Fit
    #Fit discrepancy statistics
    eval<- alpha[i] * N
    E<-pow((tvisits[1:k]-eval),2)/(eval+0.5)
    
    #Prediction
    ynew~dmulti(p[1:k],N)
    E.new<-pow((ynew-eval[x]),2)/(eval+0.5)

    #Priors
    # The probability prior is equal probability among any two classes.

    for (i in 1:k){
    alpha[1:k]<-1/k
    }

    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[]) #Discrepancy for the new data
    
    
    }
    ",fill=TRUE)

sink()

