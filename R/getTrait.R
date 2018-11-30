#' @title Compute the trait matrix (or matrices) for a given alienData object
#'
#' @description Computes the trait matrix (or matrices) using \code{node} and \code{trait} of an \link{alienData} object.
#'
#' @param object An object of class \code{alienData}.
#' @param bipartite Logical. Whether the network to consider is bipartite (TRUE) or not (FALSE). Default is FALSE.
#'
#' @details 
#' 
#' When the argument \code{bipartite} is set to TRUE, it is assumed that there are two sets of species with potentially different sets of traits. In this case, \code{edge} will be used in the \code{alienData} object to seperate both sets of traits
#' 
#' @return 
#' 
#' An object of class alienTrait
#' 
#' @author
#' F. Guillaume Blanchet
#' 
#' @importFrom reshape2 dcast
#' 
#' @export
getTrait <- function(object, bipartite = TRUE){

  # General check
  stopifnot(class(object) == "alienData")
  
  # Check for numeric values in character string
  grepNum <- "^((-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?)))$"
  
  # Check for complex values in character string
  grepComp <- "^((-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))i?)$"
  
  if(bipartite){
    ## "to" as rows
    uiTo <- unique(object$edge$to)
    traitTo <- object$trait[object$trait$idInd %in% uiTo,]
    
    ## "from" as columns
    uiFrom <- unique(object$edge$from)
    traitFrom <- object$trait[object$trait$idInd %in% uiFrom,]
    
    res <- vector("list", length = 2)
    names(res) <- c("from", "to")
    
    res[[1]] <- reshape2::dcast(traitFrom, idInd ~ trait, value.var = "value")
    res[[2]] <- reshape2::dcast(traitTo, idInd ~ trait, value.var = "value")
    
    # Make idInd as row names
    for(i in 1:2){
      rowName <- res[[i]]$idInd
      colName <- colnames(res[[i]])
      
      if(length(colName) == 2){
        res[[i]] <- data.frame(bogusName = res[[i]][,2])
        colnames(res[[i]]) <- colName[2]
      }
      
      if(length(colName) > 2){
        res[[i]] <- res[[i]][,-1]
      }
      rownames(res[[i]]) <- rowName
    }
    
    # Check if a variable can be converted to numeric (and convert if it is the case)
    for(i in 1:2){
      for(j in 1:ncol(res[[i]])){
        # Find NAs
        foundNA <- which(is.na(res[[i]][,j]))
        
        # Find numeric
        foundNum <- grep(grepNum, res[[i]][,j])
        numVec <- nrow(res[[i]]) == length(c(foundNA, foundNum))
        
        # Find complex
        foundComp <- grep(grepComp, res[[i]][,j])
        compVec <- nrow(res[[i]]) == length(c(foundNA, foundComp))
        
        # Convert to complex
        if(compVec){
          res[[i]][,j] <- as.complex(res[[i]][,j])
        }
        
        # Convert to numeric
        if(numVec){
          res[[i]][,j] <- as.numeric(res[[i]][,j])
        }
        
        # Convert to factor
        if(length(foundNum) == 0 & length(foundComp) == 0){
          res[[i]][,j] <- as.factor(res[[i]][,j])
        }
      }
    }
  }else{
    res <- reshape2::dcast(trait, idInd ~ trait, value.var = "value")

    # Make idInd as row names
    rowName <- res$idInd
    colName <- colnames(res)
    
    if(length(colName) == 2){
      res <- data.frame(bogusName = res[,2])
      colnames(res) <- colName[2]
    }
    
    if(length(colName) > 2){
      res <- res[,-1]
    }
    rownames(res) <- rowName

    # Check if a variable can be converted to numeric (and convert if it is the case)
    for(j in 1:ncol(res)){
      # Find NAs
      foundNA <- which(is.na(res[,j]))
      
      # Find numeric
      foundNum <- grep(grepNum, res[,j])
      numVec <- nrow(res) == length(c(foundNA, foundNum))
      
      # Find complex
      foundComp <- grep(grepComp, res[,j])
      compVec <- nrow(res) == length(c(foundNA, foundComp))
      
      # Convert to complex
      if(compVec){
        res[,j] <- as.complex(res[,j])
      }
    
      # Convert to numeric
      if(numVec){
        res[,j] <- as.numeric(res[,j])
      }
      
      # Convert to factor
      if(length(foundNum) == 0 & length(foundComp) == 0){
        res[,j] <- as.factor(res[,j])
      }
    }
  }

  class(res) <- "alienTrait"
  
  return(res)
}
