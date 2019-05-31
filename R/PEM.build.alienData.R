#' @title Construct Phylogenetic Eigenvector Maps with \code{\link{alienData}}
#' 
#' @description This function is essentially a wrapper around \code{\link[MPSEM]{PEM.build}}. It is designed to use the phylogenetic information in an \code{\link{alienData}} object to construct phylogenetic eigenvector maps and replace (or add) them as traits in a new \code{\link{alienData}} object.
#' 
#' @param x an object of class \code{\link{alienData}}. See details.
#' @param \dots Arguments passed to \code{\link[MPSEM]{PEM.build}}. See details.
#' 
#' @details 
#' 
#' For this function it is essential that the \code{\link{alienData}} object includes phylogenetic information, otherwise the function will send an error message. However, it is not necessary for the \code{\link{alienData}} object to include traits. 
#' 
#' The arguments passed to \code{\link[MPSEM]{PEM.build}} are typically that ones used for tunning when constructing the phylogenetic eigenvector maps.
#' 
#' @return 
#' 
#' An object of class \code{\link{alienData}}.
#' 
#' @references 
#' 
#' Guénard, G., Legendre, P., and Peres-Neto, P. 2013. Phylogenetic eigenvector maps (PEM): a framework to model and predict species traits. \emph{Methods in Ecology and Evolution} \strong{4} 1120-1131.
#' 
#' @method PEM.build alienData
#' @export
#' 
PEM.build.alienData <- function(x, ...){
  # General check
  if(is.null(x$phylo)){
    stop("There is no phylogenetic information in 'x'")
  }
  
  nphylo <- length(x$phylo)
  PEM <- vector("list", length = nphylo)
  PEMLong <- data.frame(idInd = NULL, trait = NULL,value = NULL)
  
  # Convert phylogenies to PEM
  for(i in 1:nphylo){
    graph <- MPSEM::Phylo2DirectedGraph(x$phylo[[1]])
    PEM[[i]] <- as.data.frame(MPSEM::PEM.build(graph, ...))
    colnames(PEM[[i]]) <- paste0("PEM_",names(x$phylo)[i],"_",1:ncol(PEM[[i]]))
    PEMLong <- rbind(PEMLong,melt(as.matrix(PEM[[i]])))
  }
  
  # Format data
  colnames(PEMLong) <- c("idInd", "trait", "value")
  PEMLong$value <- as.character(PEMLong$value)
  
  if(is.null(x$trait)){
    x$trait <- PEMLong
  }else{
    x$trait <- rbind(x$trait,PEMLong)
  }
  
  # return new alienData
  return(x)
}
  