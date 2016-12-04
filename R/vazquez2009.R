#' Simulate a network from a given probability matrix following null model proposed in Vazquez 2009
#'
#' @param web A I * J matrix of probabilities (binary) or total counts (quantitative)
#' @param n Total number of observations
#' @param keep.species 
#' @param rep.cell 
#' @details This is simply a wrapper function for the mgen function in the package bipartite, included here as part of the quantitative tests. See ?bipartite:mgen for details.
#' @return A predicted matrix of interactions
#' @export
predict.vazquez2009<-function(web,n,keep.species=TRUE,rep.cell=T){
  df<-mgen(pweb, n, keep.species=keep.species, rep.cell=rep.cell) # Not allowing zero marginal sums
  return(df)
}