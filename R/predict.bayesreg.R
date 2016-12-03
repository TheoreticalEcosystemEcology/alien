#' @name predict.bayesreg
#' @aliases prediction bayesian regression
#'
#' @title Predict species interaction probabilities based on a fitted hierarchical bayesian regression 
#'
#' @description A helper functional for generating the probability of interaction among hypothetical species based on their traits.
#' The trait-matching function is currently |Trait_i - Trait_j|
#' @param x A JAGS model file return from fit.bayesreg
#' @param newdata A data frame with columns "I","J" and "Traitmatch"
#' @details The newdata object gives the trait-matching between species I (eg. pollinator) and species J (eg. plant)
#' If fit.bayesreg was fit to an intercept model, newdata is ignored, not possible to create a prediction of a new species. In this case, it is assumed that the user wants the probability matrix for all input species.
#' 
#' @return A vector of probability of interaction values.
#' I: Species in the higher level (eg. pollinators)
#' J: Species in the lower level (eg. plants)
#' mean: Mean probability of interaction
#' lower: 0.05 quantile of the probability of interaction
#' upper: 0.95 quantile of the probability of interaction
#' @author
#' Ben Weinstein
#'
#' @references
#' Weinstein et al. 2016. Oikos
#'
#' @rdname predict.bayesreg
#' @export

predict.bayesreg<-function(x,newdata=NULL){
  
  #can't call namespace on special %>% for dplyr.
  library(dplyr)
  
  parsm1<-extract.bayesreg(x)
  
  #matching function
  if(x$Algorithm=="Intercept"){
    
    #get intercepts
    alphas<-parsm1 %>% filter(par %in% c("alpha")) %>% group_by(Draw,Chain) 
    
    #label species
    alphas$I<-as.numeric(stringr::str_match(alphas$parameter,pattern="\\[(\\d+),(\\d+)]")[,2])
    alphas$J<-as.numeric(stringr::str_match(alphas$parameter,pattern="\\[(\\d+),(\\d+)]")[,3])
    
    #return matrix as probability
    df<-alphas %>% group_by(I,J) %>% mutate(value=boot::inv.logit(estimate)) %>% select(I,J,value)  
    
    #merge with input data
    df$I<-levels(factor(dat$I))[df$I]
    df$J<-levels(factor(dat$J))[df$J]
  }
  
  #matching function
  if(x$Algorithm=="Binomial"){
    
    #default is predicting the observed data frame
     if(is.null(newdata)){newdata<-x$data %>% select(I,J,Traitmatch) %>% distinct()}
    
    #Trait-matching functions
    predfun<-function(alpha,beta,newdata){
      data.frame(newdata,value=boot::inv.logit(alpha + beta * newdata[["Traitmatch"]])
      )}
    df<-parsm1 %>% filter(par %in% c("alpha_mu","beta_mu")) %>% select(Draw,Chain,par,estimate) %>% reshape2::dcast(.,Draw + Chain ~ par,value.var="estimate") %>% group_by(Draw,Chain)%>%  do((predfun(alpha=.$alpha_mu,beta=.$beta_mu,newdata))) %>% group_by(I,J) %>% inner_join(newdata) 
  }

  return(df)  
}

