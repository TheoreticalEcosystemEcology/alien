#' Generate network statistics from a hierarchical bayesian model of species interactions
#' @name network.bayesreg
#' @param x A predicted model object from predict.bayesreg
#' @param indices a vector of network statistics corresponding to network statistics (see details)
#' @details Network statistics are calculated using the fucntion networklevel from the package bipartite, see ?networklevel for a list of available options.
#'
#' @return
#' @export
#'
#' @examples
network.bayesreg<-function(x,indices){
  
  #helper function for web prediction
  makeN<-function(a,indices){
    
    #draw an observed state
    a$state<-sapply(a$value,function(w) rbinom(1,1,prob=w))
    
    #cast into I by J matrix
    predweb<-reshape2::acast(data=a,I~J,value.var="state")
    
    #calculate network statistic
    nstat<-bipartite::networklevel(predweb,indices)
    data.frame(Metric=names(nstat),nstat)
  }
  
  #calculate network stats for each model
  nstats<-x %>% group_by(Draw,Chain) %>% do(makeN(.,indices))
 
  return(nstats)
 }


