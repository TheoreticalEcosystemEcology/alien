#' Title Hierarchical Bayesian Regression for estimating trait-matching in species interaction data
#'
#' @param dat A dataframe with rows, columns
#' @param algorithm
#'
#' @return
#' @export
#'
#' @examples
fit.bayesreg<-function(dat,algorithm="Intercept"){

  #format traitmatch as matrix
  dat$Traitmatch<-abs(dat$TraitI-dat$TraitJ)
  Traitmatch<-acast(data=dat,I~J,value.var="Traitmatch")

  if(algorithm == "Intercept"){


    runs<-10000

    #Source model
    source("Bayesian/Intercept.R")

    #print model
    print.noquote(readLines("Bayesian//Intercept.R"))

    #for parallel run
    Yobs=dat$Interactions
    Bird=as.numeric(as.factor(dat$I))
    Plant=as.numeric(as.factor(dat$J))
    Birds=length(unique(dat$I))
    Traitmatch=Traitmatch
    Plants=length(unique(dat$J))
    Nobs<-length(Yobs)

    #MCMC options
    ni <- runs  # number of draws from the posterior
    nt <- 1  #thinning rate
    nb <- max(0,runs-500) # number to discard for burn-in
    nc <- 2  # number of chains

    modelDat<-list("Yobs","Bird","Plant","Plants","Birds","Nobs")

    ParsStage <- c("alpha","alpha_mu","ynew","fit","fitnew")

    m1<-do.call(jags.parallel,list(data=modelDat,parameters.to.save=ParsStage,model.file="Bayesian/Intercept.jags",n.thin=nt, n.iter=ni,n.burnin=nb,n.chains=nc,DIC=F))
  } else

    if(algorithm=="Binomial") {

      runs<-10000

      #Source model
      source("Bayesian/Binomial.R")

      #print model
      print.noquote(readLines("Bayesian//Binomial.R"))

      #for parallel run
      Yobs=dat$Interactions
      Bird=as.numeric(as.factor(dat$I))
      Plant=as.numeric(as.factor(dat$J))
      Birds=length(unique(dat$I))
      Traitmatch=Traitmatch
      Plants=length(unique(dat$J))
      Nobs<-length(Yobs)

      #Parameters to track
      ParsStage <- c("alpha","beta","alpha_mu","beta_mu","ynew","fit","fitnew")

      #MCMC options
      ni <- runs  # number of draws from the posterior
      nt <- 1  #thinning rate
      nb <- max(0,runs-500) # number to discard for burn-in
      nc <- 2  # number of chains

      modelDat<-list("Yobs","Bird","Plant","Plants","Traitmatch","Birds","Nobs")

      m1<-do.call(jags.parallel,list(data=modelDat,parameters.to.save=ParsStage,model.file="Bayesian/Binomial.jags",n.thin=nt, n.iter=ni,n.burnin=nb,n.chains=nc,DIC=F))
    }

  m1$Algorithm<-algorithm
  return(m1)
}
