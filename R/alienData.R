#' @title Formatting data and return an \code{alienData} object
#'
#' @description \code{alienData} is used to check the data, if correct
#' it returns an object of class \code{alienData}.
#'
#' @param adjMat An adjancency matrix. The rows (From) species are influencing the column (To) species.
#' @param traitFrom A data.frame containing the traits of the row (From) species.
#' @param traitTo A data.frame containing the traits of the column (To) species.
#' @param traitDistFrom A dist object containing the distance between pairs of traits of the row (From) species.
#' @param traitDistTo A dist object containing the distance between pairs of traits of the column (To) species.
#' @param phyloDistFrom A dist object containing phylogenetic distance between pairs of row (From) species.
#' @param phyloDistTo A dist object containing phylogenetic distance between pairs of column (To) species.
#'
#' @details
#'
#' This function is essentially designed to make sure the names of all components match in the right order. The output of this function is at the basis of all the analyses implemented in the alien package.
#'
#' @return
#' An object of class \code{alienData} is returned. 
#' 
#' @author F. Guillaume Blanchet, Kevin Cazelles & Steve Vissault
#'
#' @keywords manip
#' @keywords classes
#' @export
alienData <- function(adjMat, traitFrom = NULL, traitTo = NULL,
                      traitDistFrom = NULL, traitDistTo = NULL,
                      phyloDistFrom = NULL, phyloDistTo = NULL) {

  ##################
  # Adjacency matrix
  ##################
  if(!is.matrix(adjMat)){
    stop("'adjMat' should be a matrix")
  }
  
  # row names adjMat
  adjMatFromNames <- rownames(adjMat)
  
  if(is.null(adjMatFromNames)){
    stop("'adjMat' needs to have row names")
  }
  
  # column names adjMat
  adjMatToNames <- colnames(adjMat)
  
  if(is.null(adjMatToNames)){
    stop("'adjMat' needs to have column names")
  }
  
  #-#-#-#-#-
  # Raw data
  #-#-#-#-#-
  #######
  # Trait
  #######
  #-----------
  # Trait from 
  #-----------
  if(!is.null(traitFrom)){
    # Check object class
    if(!is.data.frame(traitFrom)){
      stop("'traitFrom' should be a data.frame")
    }
    
    # Names
    traitFromNames <- rownames(traitFrom)
    
    # Check variable class
    traitFromVarClass <- sapply(traitFrom,
                                function(x) is.numeric(x) | is.factor(x))
    
    if(!all(traitFromVarClass)){
      stop("Variables in 'traitFrom' need to be a 'numeric' or a 'factor'")
    }
   
    # Check if species name match
    if(!all(adjMatFromNames %in% traitFromNames)){
      stop("traitFrom and the rows of adjMat do not have the same labels")
    }
    
    expOrd <- match(adjMatFromNames, traitFromNames)
    if(!all(expOrd == 1:length(adjMatFromNames))){
      stop("The row names of traitFrom and the row names of adjMat needs to have the same order")
    }
  }
  
  #---------
  # Trait to
  #---------
  if(!is.null(traitTo)){
    # Check object class
    if(!is.data.frame(traitTo)){
      stop("'traitTo' should be a data.frame")
    }
    
    # Names
    traitToNames <- rownames(traitTo)
    
    # Check variable class
    traitToVarClass <- sapply(traitTo,
                                function(x) is.numeric(x) | is.factor(x))
    
    if(!all(traitToVarClass)){
      stop("Variables in 'traitTo' need to be a 'numeric' or a 'factor'")
    }

    # Check if species name match
    if(!all(adjMatToNames %in% traitToNames)){
      stop("traitTo and the columns of adjMat do not have the same labels")
    }
    
    # Check if the order of the species names match between the adjacency matrix and the cophenetic matrix
    expOrd <- match(adjMatToNames, traitToNames)
    if(!all(expOrd == 1:length(adjMatToNames))){
      stop("The rownames of traitTo and the column names of adjMat needs to have the same order")
    }
  }

  #-#-#-#-#-#-#-#
  # Distance data
  #-#-#-#-#-#-#-#
  ########
  # Traits
  ########
  #--------------
  # traitDistFrom
  #--------------
  if(!is.null(traitDistFrom)){
    # Check object class
    if(class(traitDistFrom) != "dist"){
      stop("'traitDistFrom' needs to be of class dist")
    }
    
    # Extract labels
    traitDistFromNames <- attributes(traitDistFrom)$Labels
    
    # Check if species name match
    if(!all(adjMatFromNames %in% traitDistFromNames)){
      stop("Labels of traitDistFrom and the column of adjMat do not match")
    }
    
    # Check if the order of the species names match between the adjacency matrix and the cophenetic matrix
    expOrd <- match(adjMatFromNames,traitDistFromNames)
    if(!all(expOrd == 1:length(adjMatFromNames))){
      stop("The labels of traitDistFrom and the row names of adjMat needs to have the same order")
    }
  }
  
  #------------
  # traitDistTo
  #------------
  if(!is.null(traitDistTo)){
    # Check object class
    if(class(traitDistTo) != "dist"){
      stop("'traitDistTo' needs to be of class dist")
    }
    
    # Extract labels
    traitDistToNames <- attributes(traitDistTo)$Labels
    
    # Check if species name match
    if(!all(adjMatToNames %in% traitDistToNames)){
      stop("Labels of traitDistTo and the column of adjMat do not match")
    }
    
    # Check if the order of the species names match between the adjacency matrix and the trait distance matrix
    expOrd <- match(adjMatToNames,traitDistToNames)
    if(!all(expOrd == 1:length(adjMatToNames))){
      stop("The labels of traitDistTo and the column names of adjMat needs to have the same order")
    }
  }

  ###########
  # Phylogeny
  ###########
  #--------------
  # phyloDistFrom
  #--------------
  if(!is.null(phyloDistFrom)){
    # Check object class
    if(class(phyloDistFrom) != "dist"){
      stop("'phyloDistFrom' needs to be of class dist")
    }
    
    # Extract labels
    phyloDistFromNames <- attributes(phyloDistFrom)$Labels
    
    # Check if species name match
    if(!all(adjMatFromNames %in% phyloDistFromNames)){
      stop("Labels of phyloDistFrom and the column of adjMat do not match")
    }

    # Check if the order of the species names match between the adjacency matrix and the cophenetic matrix
    expOrd <- match(adjMatFromNames,phyloDistFromNames)
    if(!all(expOrd == 1:length(adjMatFromNames))){
      stop("The labels of phyloDistFrom and the row names of adjMat needs to have the same order")
    }
  }
  
  #------------
  # phyloDistTo
  #------------
  if(!is.null(phyloDistTo)){
    # Check object class
    if(class(phyloDistTo) != "dist"){
      stop("'phyloDistTo' needs to be of class dist")
    }
    
    # Extract labels
    phyloDistToNames <- attributes(phyloDistTo)$Labels
    
    # Check if species name match
    if(!all(adjMatToNames %in% phyloDistToNames)){
      stop("Labels of phyloDistTo and the column of adjMat do not match")
    }
    
    # Check if the order of the species names match between the adjacency matrix and the cophenetic matrix
    expOrd <- match(adjMatToNames,phyloDistToNames)
    if(!all(expOrd == 1:length(adjMatToNames))){
      stop("The labels of phyloDistTo and the column names of adjMat needs to have the same order")
    }
  }
  
  #########################################
  # Check to ensure number of species match
  #########################################
  # traitFrom
  if(!is.null(traitFrom)){
    if(nrow(adjMat) != nrow(traitFrom)){
      stop("The number of rows in adjMat should match the number of rows in traitFrom")
    }
  }
  
  # traitTo
  if(!is.null(traitTo)){
    if(ncol(adjMat) != nrow(traitTo)){
      stop("The number of columns in adjMat should match the number of rows in traitTo")
    }
  }
  
  # traitDistFrom
  if(!is.null(traitDistFrom)){
    if(nrow(adjMat) != attributes(traitDistFrom)$Size){
      stop("The number of rows in adjMat should match the size of traitDistFrom")
    }
  }
  
  # traitDistTo
  if(!is.null(traitDistTo)){
    if(ncol(adjMat) != attributes(traitDistTo)$Size){
      stop("The number of columns in adjMat should match the size of traitDistTo")
    }
  }
  
  # phyloDistFrom
  if(!is.null(phyloDistFrom)){
    if(nrow(adjMat) != attributes(phyloDistFrom)$Size){
      stop("The number of rows in adjMat should match the size of phyloDistFrom")
    }
  }
  
  # phyloDistTo
  if(!is.null(phyloDistTo)){
    if(ncol(adjMat) != attributes(phyloDistTo)$Size){
      stop("The number of columns in adjMat should match the size of phyloDistTo")
    }
  }
  
  # Results
  res <- list(adjMat = adjMat,
              traitFrom = traitFrom,
              traitTo = traitTo,
              traitDistFrom = traitDistFrom,
              traitDistTo = traitDistTo,
              phyloDistFrom = phyloDistFrom,
              phyloDistTo = phyloDistTo)
  
  class(res) <- "alienData"
  
  return(res)
}
