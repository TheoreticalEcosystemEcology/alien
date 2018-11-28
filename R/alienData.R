#' @title  Formatting data and return an \code{alienData} object
#'
#' @description \code{alienData} is used to check and format data, if correct
#' it returns an object of class \code{alienData}.
#'
#' @param nodes A data.frame with two columns. The first column includes unique individual identifications and the second columns presents species identification. (See details).
#' unique identifiers for each species considered. The remaining
#' columns are traits data that must
#' be specified in the \code{trait} argument, otherwise they will be ignored.
#' @param dfEdges A data frame with at least two columns: \code{idFrom} and \code{idTo}
#' descibring the set of edges (links between nodes). If \code{directed} is set
#' to \code{TRUE} then the interaction is directed from \code{idFrom} to \code{idTo}.
#' Directed interactions for consumer/resource interactions correspond to a transfer
#' of energy so that \code{idFrom} is the resource and \code{idTo} is the consumer.
#' The presence of two additonnal columns are checked: \code{value} and \code{idSite}
#' which respectively provide the values associated with edges (if absent, they are
#' set to 1) and the identifier of the site where the interaction has been obsereved
#' (see details).
#' @param trait A vector indicating columns number (or names) of \code{nodes} containing traits data (see \code{Details}).
#' @param directed Logical. If `TRUE` (default value) the network is considered as directed (see \code{Details}).
#' @param dfSites A data frame with at least two columns named \code{idSite}
#' providing information about the site where the interactions have been observed.
#' @param siteEnv A vector indicating colums number (or names) of \code{dfSites} containing environmental variables (see \code{Details}).
#' @param dfOcc A data frame with at least two columns \code{idNodes} and \code{idSite}
#' providing the occurrence of nodes.
#' @param verbose Logical. Should extra information be reported on progress?
#'
#' @details
#'
#' In the \code{nodes} argument, the first columns (individual identification) identifathe first columns should have noeach layer of the list needs to include information about a particular layer of species in the network. If all species can potentially interact among each other, only a vector or a data.frame can be included. 
#'
#' The user is required to provide specific column names to prevent the function
#' from returning errors. Two primary keys \code{idNodes} and \code{idSite} (if site
#' information are provided) are used to check the consistency of the data.
#' First, all values taken by \code{idFrom} and \code{idTo} column in \code{dfEdges}
#' must be found in \code{idNodes} column of \code{nodes} (otherwise an error
#' is returned). Second if \code{dfSites} and occurrence information is provided too,
#' \code{idSite} is used to ensure all the sites for which an occurrence event have
#' are reported in \code{idSite}.
#'
#' If \code{idSite} is found in \code{dfEdges} and \code{dfSites} is \code{NULL} then, this
#' column will be used to identify sites. Also, if \code{dfOcc} is \code{NULL},
#' it will be used to build \code{dfOcc}. Note that providing \code{idSites} in
#' \code{dfEdges} means that theuser has spatial information about interactions
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

nodes <- list(salix = salix, galler = galler, paras = paras)

alienData <- function(nodes, dfEdges, trait = NULL, phylo = NULL, taxo = NULL,
    dfSites = NULL, siteEnv = NULL, dfOcc = NULL, directed = FALSE, verbose = TRUE) {
  
}
