extract_par<-function(x){
  parsO<-melt(x$BUGSoutput$sims.array)
  colnames(parsO)<-c("Draw","Chain","parameter","estimate")
  
  #label species and plants
  l<-levels(parsO$parameter)
  
  #parameters to saveC
  totrack<- x$parameters.to.save
  
  #assign species index to ragged frame.
  sp_pl<-data.frame(parameter=l,Index=as.numeric(str_match(l,pattern="\\[(\\d+)]")[,2]),par=str_extract(l,"\\w+"))
  
  #merge levels
  pars<-merge(parsO,sp_pl)
  
  pars<-pars[!pars$par %in% "deviance",]
  return(pars)
}

#Trait-matching functions
pred.binomial<-function(alpha,beta,newdata){
  data.frame(newdata,value=inv.logit(alpha + beta * newdata[["Traitmatch"]])
      )}


