#' @title  Formatting data and return an \code{alienData} object
#'
#' @description \code{alienData} is used to check and format data, if correct
#' it returns an object of class \code{alienData}.
#'
#' @param node A data.frame with two columns. The first column includes unique individual identifications and needs to be defined as "idInd" while the second columns presents species identification and needs to be defined as "idSp". (See details).
#' @param edge A data frame with two columns: \code{from} and \code{to}
#' describing the set of edges (links between nodes). If \code{directed} is set
#' to \code{TRUE} then the interaction is directed \code{from} a resource \code{to} a consumer.
#' The presence of a \code{value} and \code{site}
#' which respectively provide the values associated with edges (if absent, they are
#' set to 1) and the identifier of the site where the interaction has been observed.
#' (See details).
#' @param trait A data.frame with three columns. The first column is the individual identifier and needs to be defined as "idInd", the second column is the trait names and needs to be defined as "trait" while the third column is the trait characteristic, which could be a numeric or a character string and needs to be called "value". (See details).
#' @param phylo An object (or a list of objects) of class \code{\link[ape]{phylo}} .
#' @param directed Logical. If \code{TRUE} (default value) the network is considered directed. (See details).
#' @param verbose Logical. Should extra information be reported on progress?
#'
#' @details
#'
#' In the \code{nodes} argument, the first columns (individual identification) should have unique (non-repetitive) identifiers for each lines while the species identifier (usually a species code or a the species name) can be repeted.
#'
#' If the data available is at the species level, the species as well as the individual identifiers will not independently repeat. In this case, the individual identifier \code{idInd} will be replaced by the species identifier \code{idSp} in \code{node}, thus allowing \code{edges} and \code{trait} to present relations using species identifier. An error will be sent if multiple individuals were measured for each species.
#'
#' It is from the \code{trait} argument that an individual (or a species) by trait matrix is constructed using the [getTrait()] function. Because, many of the models considered in this package do not handle NAs, it becomes important to make sure all combinations of individuals (or species) by traits are defined in the trait matrix resulting from the [getTrait()] function. 
#'
#' The check on the \code{phylo} argument assumes that the phylogeny is at the species level.
#'
#' @return
#' An object of class \code{alienData} is returned. 
#' 
#' Because an  \code{alienData} object is organized for data processing rather than for presentation. When printed on screen, only a summary of the data is presented.
#'
#' @author F. Guillaume Blanchet, Kevin Cazelles & Steve Vissault
#'
#' @keywords manip
#' @keywords classes
#' @export
alienData <- function(node, edge, trait = NULL, phylo = NULL,
                      directed = TRUE, verbose = TRUE) {

  ######
  # node
  ######
  if(ncol(node) != 2){
    stop("'node' must have two columns")
  }

  nIndID <- length(unique(node$idInd))
  nSpID <- length(unique(node$idSp))
  nSample <- nrow(node)

  if(nIndID != nSample){
    stop("The number of identifier in the first column should be unique")
  }

  nodeName <- colnames(node)

  if(nodeName[1] != "idInd"){
    stop("The first column name of 'node' should be 'idInd'")
  }

  if(nodeName[2] != "idSp"){
    stop("The second column name of 'node' should be 'idSp'")
  }

  if(nIndID == nSpID){
    node$idInd <- node$idSp
  }

  ######
  # edge
  ######
  stopifnot(all(edge$from %in% node$idInd))
  stopifnot(all(edge$to %in% node$idInd))

  idn <- which(!node$idInd %in% c(edge$from, edge$to))
  if (length(idn)){
    warning(paste0("Unlinked nodes: ", paste(node$idInd, collapse = ", ")))
  }

  if (!"value" %in% names(edge)) {
    if (verbose){
      message("==> No Edges' value detected, values are set to 1")
    }
    edge$value <- 1
  }

  #######
  # trait
  #######
  if(!is.null(trait)){
    stopifnot(all(trait$idInd %in% node$idInd))

    if(ncol(trait) != 3){
      stop("'trait' must have three columns")
    }

    traitName <- colnames(trait)

    if(traitName[1] != "idInd"){
      stop("The first column name of 'trait' should be 'idInd'")
    }

    if(traitName[2] != "trait"){
      stop("The second column name of 'trait' should be 'trait'")
    }

    if(traitName[3] != "value"){
      stop("The third column name of 'trait' should be 'value'")
    }

    # Make sure "value" is a character string
    trait$value <- as.character(trait$value)
  }


  #######
  # phylo
  #######
  if(!is.null(phylo)){
    
    if(!is.list(phylo)){
      stopifnot(all(phylo$tip.label %in% node$idSp))
      
      if(class(phylo) != "phylo"){
        stop("'phylo' needs to be an object of class phylo")
      }
    }else{
      for(i in 1:length(phylo)){
        stopifnot(all(phylo[[i]]$tip.label %in% node$idSp))
        
        # Find phylo for from species
        if(all(phylo[[1]]$tip.label  %in% edge$from)){
          names(phylo) <- c("from", "to")
        }else{
          names(phylo) <- c("to", "from")
        }
        
        if(class(phylo[[i]]) != "phylo"){
          stop("Each part of 'phylo' needs to be an object of class phylo")
        }
      }
    }
  }

  # Results
  res <- list(node = node, edge = edge, trait = trait,
              phylo = phylo, info = list(directed = directed))
  class(res) <- "alienData"
  res
}
