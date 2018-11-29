#' @title  Formatting data and return an \code{alienData} object
#'
#' @description \code{alienData} is used to check and format data, if correct
#' it returns an object of class \code{alienData}.
#'
#' @param node A data.frame with two columns. The first column includes unique individual identifications and the second columns presents species identification. (See details).
#' @param edges A data frame with at least two columns: \code{from} and \code{to}
#' descibring the set of edges (links between nodes). If \code{directed} is set
#' to \code{TRUE} then the interaction is directed from \code{from} to \code{to}.
#' Directed interactions for consumer/resource interactions correspond to a transfer
#' of energy so that \code{from} is the resource and \code{to} is the consumer.
#' The presence of two additonnal columns are checked: \code{value} and \code{site}
#' which respectively provide the values associated with edges (if absent, they are
#' set to 1) and the identifier of the site where the interaction has been observed.
#' (See details).
#' @param trait A data.frame with three columns. The first column is the individual identifier, the second colum is the trait names and the third column is the trait characteristic, which could be a numeric or a character string. (See details).
#' @param phylo An object of class \code{\link[ape]{phylo}}.
#' @param directed Logical. If \code{TRUE} (default value) the network is considered directed. (See details).
#' @param dfSites A data frame with at least two columns named \code{idSite}
#' providing information about the site where the interactions have been observed.
#' @param siteEnv A vector indicating colums number (or names) of \code{dfSites} containing environmental variables (see \code{Details}).
#' @param dfOcc A data frame with at least two columns \code{idNodes} and \code{idSite}
#' providing the occurrence of nodes.
#' @param verbose Logical. Should extra information be reported on progress?
#'
#' @details
#'
#' In the \code{nodes} argument, the first columns (individual identification) should have unique (non-repetitive) identifiers for each lines while the species identifier (usually a species code or a the species name) can be repeted. Note also that if the data available is at the species level, the species as well as the the individual identifiers will not repeat.
#' 
#' It is from the \code{traits} argument that an individual (or a species) by trait matrix is constructed using the \code{\link{getTraitMatrix}} function. Because, many of the models considered in this package do not handle NAs, it becomes important to make sure all combinations of individuals (or species) by traits are defined in the trait matrix resulting from the \code{\link{getTraitMatrix}} function. 
#' 
#' The user is required to provide specific column names to prevent the function
#' from returning errors. Two primary keys \code{idNodes} and \code{idSite} (if site
#' information are provided) are used to check the consistency of the data.
#' First, all values taken by \code{from} and \code{to} column in \code{edge}
#' must be found in \code{idNodes} column of \code{nodes} (otherwise an error
#' is returned). Second if \code{dfSites} and occurrence information is provided too,
#' \code{idSite} is used to ensure all the sites for which an occurrence event have
#' are reported in \code{idSite}.
#'
#' If \code{site} is found in \code{edges} and \code{dfSites} is \code{NULL} then, this
#' column will be used to identify sites. Also, if \code{dfOcc} is \code{NULL},
#' it will be used to build \code{dfOcc}. Note that providing \code{idSites} in
#' \code{edge} means that theuser has spatial information about interactions
#' which is more informative than providing occurrence and interaction  data separetly.
#'
#' @return
#' An object of the class \code{alienData} is returned.
#'
#' @author F. Guillaume Blanchet, Kevin Cazelles & Steve Vissault
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @keywords manip
#' @keywords classes
#' @export


load("/Users/guslevesque/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/4D1DB951-1F0F-48DC-A0D6-7054CE4DECEA/edges_gal_par.RDA")

load("/Users/guslevesque/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/E0320368-CB9E-42BF-998E-A7AD41182E82/edges_sal_gal.RDA")

load("/Users/guslevesque/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/CF898134-3311-4E51-89A9-B59788869401/galler_traits.RDA")

load("/Users/guslevesque/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/1D3778C7-4EAF-4B87-BF0D-02385C01DE1A/paras_traits.RDA")

load("/Users/guslevesque/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/E98FB05E-E006-4E3F-AA12-596732643172/salix_traits.RDA")

load("/Users/guslevesque/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/C700AF82-CA1E-4250-9C59-1AC49A7858A7/sites.RDA")

node <- list(salix = salix, galler = galler, paras = paras)

alienData <- function(node, edge, trait = NULL, phylo = NULL,
                      directed = TRUE, verbose = TRUE) {
  ######
  # node
  ######
  if(ncol(node) != 2){
    stop("'node' must have two columns")
  }
  
  nIndID <- length(unique(node[,1]))
  nSample <- nrow(node)
  
  if(nIndID != nSample){
    stop("The number of identifier in the first column should be unique")
  }
  
  ######
  # edge
  ######
  stopifnot(all(edge$from %in% node[,1]))
  stopifnot(all(edge$to %in% node[,1]))
  idn <- which(!node[,1] %in% c(edge$from, edge$to))
  if (length(idn))
    warning(paste0("Unlinked nodes: ", paste(node[idn,1], collapse = ", ")))
  
  if (!"value" %in% names(edge)) {
    if (verbose){
      message("==> No Edges' value detected, values are set to 1")
    }
    edge$value <- 1
  }else{
    if (verbose){
      message("==> Edges' values detected")
    }
  }
  
  #######
  # trait
  #######
  if(ncol(trait) != 3){
    stop("'trait' must have three columns")
  }
  
  nIndID <- length(unique(node[,1]))
  nSample <- nrow(node)
  
  if(nIndID != nSample){
    stop("The number of identifier in the first column should be unique")
  }
  
  
  # Results
  res <- list(node = node, edge = edge, trait = trait,
              dfOcc = dfOcc, info = list(nbNode = nrow(node),
                                         nbEdge = nrow(edge),
                                         directed = directed))
  class(res) <- "alienData"
  
  return(res)

}
