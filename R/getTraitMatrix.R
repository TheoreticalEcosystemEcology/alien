#' @title Compute the trait matrix (or matrices) for a given alienData object
#'
#' @description Computes the trait matrix (or matrices) using \code{dfNodes} and \code{nmTrait} of an \link{alienData} object.
#'
#' @param object An object of class \code{alienData}.
#' @param level Either 'individual' or 'species'. The ecological level on which the traits should be extracted. Default is 'species'.
#' @param bipartite Logical. Whether the network to consider is bipartite (TRUE) or not (FALSE). Default is FALSE.
#'
#' @details 
#' 
#' The argument \code{level} takes into account species if the data considered has multiple measures for the same species. In this case the mean (numeric variable) or the dominant level (factor) will be considered when gathering the data by species.
#'
#'
#' @author
#' F. Guillaume Blanchet
#' 
#' @export
getTraitMatrix <- function(object, level = "species", bipartite = TRUE){

  # General check
  stopifnot(class(object) == "alienData")
  
  # Extract the trait information
  selCol <- colnames(object$dfNodes) %in% object$nmTrait
  traitRaw <- object$dfNodes[,selCol]
  spRaw <- object$dfNodes$idSp
  nodeRaw <- object$dfNodes$idNodes
  
  # Separate the traits in two groups (From and To)
  if(bipartite){
    from <- unique(object$dfEdges$idFrom)
    fromRow <- which(object$dfNodes$idNodes %in% from)
    to <- unique(object$dfEdges$idTo)
    toRow <- which(object$dfNodes$idNodes %in% to)
    
    traitFrom <- traitRaw[fromRow,]
    spFrom <- as.factor(spRaw[fromRow])

    traitTo <- traitRaw[toRow,]
    spTo <- as.factor(spRaw[toRow])

    # Remove the columns of NAs in From
    traitFrom <- traitFrom[,-which(apply(traitFrom, 2, function(x) all(is.na(x))))]
    traitTo <- traitTo[,-which(apply(traitTo, 2, function(x) all(is.na(x))))]
    
    if(any(is.na(traitFrom)) || any(is.na(traitTo))){
      stop("Traits have NAs that should not be there")
    }

    # Species
    if(level == "species"){
      # From
      traitFromSp <- vector("list", length = ncol(traitFrom))
      names(traitFromSp) <- colnames(traitFrom)
      
      for(i in 1:ncol(traitFrom)){
        if(is.numeric(traitFrom[,i])){
          traitFromSp[[i]] <- tapply(traitFrom[,i],spFrom, mean)
        }
        
        if(is.character(traitFrom[,i])){
          traitFromSp[[i]] <- tapply(as.factor(traitFrom[,i]),spFrom, 
                                     function(x) names(summary(x))[which.max(summary(x))])
          traitFromSp[[i]] <- as.factor(traitFromSp[[i]])
        }
      }
      
      traitFromSp <- as.data.frame(traitFromSp)
  
      # To
      traitToSp <- vector("list", length = ncol(traitTo))
      names(traitToSp) <- colnames(traitTo)
      
      for(i in 1:ncol(traitTo)){
        if(is.numeric(traitTo[,i])){
          traitToSp[[i]] <- tapply(traitTo[,i],spTo, mean)
        }
        
        if(is.character(traitTo[,i])){
          traitToSp[[i]] <- tapply(as.factor(traitTo[,i]),spTo, 
                                     function(x) names(summary(x))[which.max(summary(x))])
          traitToSp[[i]] <- as.factor(traitToSp[[i]])
        }
      }
      
      traitToSp <- as.data.frame(traitToSp)
      
      res <- list(from = traitFromSp, to = traitToSp)
    }
    
    # Individual
    if(level == "individual"){
      res <- list(from = traitFrom, to = traitTo)
    }
  }else{
    if(any(is.na(traitRaw))){
      stop("Traits have NAs that should not be there")
    }
    
    # Species
    if(level == "species"){
      traitRawSp <- vector("list", length = ncol(traitRaw))
      names(traitRawSp) <- colnames(traitRaw)
      
      for(i in 1:ncol(traitRaw)){
        if(is.numeric(traitRaw[,i])){
          traitRawSp[[i]] <- tapply(traitRaw[,i],spFrom, mean)
        }
        
        if(is.character(traitRaw[,i])){
          traitRawSp[[i]] <- tapply(as.factor(traitRaw[,i]),spFrom, 
                                     function(x) names(summary(x))[which.max(summary(x))])
          traitRawSp[[i]] <- as.factor(traitRawSp[[i]])
        }
      }
      
      res <- as.data.frame(traitRawSp)
    }
    
    # Individual
    if(level == "individual"){
      res <- traitRaw
    }
  }

  return(res)
}