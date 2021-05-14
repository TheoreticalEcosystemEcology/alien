#' @title Formatting data and return an \code{alienData} object
#'
#' @description \code{alienData} is used to check the data, if correct
#' it returns an object of class \code{alienData}.
#'
#' @param adjMat An adjancency matrix. The rows (From) species are influencing the column (To) species.
#' @param traitFrom A data.frame containing the traits of the row (From) species.
#' @param traitTo A data.frame containing the traits of the column (To) species.
#' @param phyloFrom An object of class \code{\link[ape]{phylo}} for the row (From) species.
#' @param phyloTo An object of class \code{\link[ape]{phylo}} for the column (To) species.
#' @param traitDistFrom A dist object containing the distance between pairs of traits of the row (From) species.
#' @param traitDistTo A dist object containing the distance between pairs of traits of the column (To) species.
#' @param phyloDistFrom A dist object containing phylogenetic distance between pairs of row (From) species.
#' @param phyloDistTo A dist object containing phylogenetic distance between pairs of column (To) species.
#'
#' @details
#'
#' This function is essentially designed to make sure the names of all components match. The output of this function is at the basis of all the analyses implemented in this package.
#'
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
                      phyloFrom = NULL, phyloTo = NULL, 
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
  }

  ###########
  # Phylogeny
  ###########
  #-----------
  # Phylo from 
  #-----------
  if(!is.null(phyloFrom)){
    # Check object class
    if(class(phyloFrom) != "phylo"){
      stop("'phyloFrom' should be an object of class phylo")
    }
    
    # Species names
    phyloFromNames <- phyloFrom$tip.label
    
    # Check if species name match
    if(!all(adjMatFromNames %in% phyloFromNames)){
      stop("phyloFrom and the rows of adjMat do not have the same labels")
    }
  }

  #---------
  # Phylo to 
  #---------
  if(!is.null(phyloTo)){
    # Check object class
    if(class(phyloTo) != "phylo"){
      stop("'phyloTo' should be an object of class phylo")
    }
    
    # Species names
    phyloToNames <- phyloTo$tip.label
    
    # Check if species name match
    if(!all(adjMatToNames %in% phyloToNames)){
      stop("phyloTo and the column of adjMat do not have the same labels")
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
  
  # phyloFrom
  if(!is.null(phyloFrom)){
    if(nrow(adjMat) != ape::Ntip(phyloFrom)){
      stop("The number of rows in adjMat should match the number of tips in phyloFrom")
    }
  }
  
  # phyloTo
  if(!is.null(phyloTo)){
    if(ncol(adjMat) != ape::Ntip(phyloTo)){
      stop("The number of columns in adjMat should match the number of tips in phyloTo")
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
              phyloFrom = phyloFrom,
              phyloTo = phyloTo,
              traitDistFrom = traitDistFrom,
              traitDistTo = traitDistTo,
              phyloDistFrom = phyloDistFrom,
              phyloDistTo = phyloDistTo)
  
  class(res) <- "alienData"
  
  return(res)
}
