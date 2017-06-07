#' @title Format data for the \code{alien} class
#'
#' @description This function formats the data and returns an object of class alienData.
#'
#' @param dfNodes A data frame with at least one column named \code{idNodes} providing
#' unique identifiers for each species identified within the dataset. The remainig
#' columns could be either traits or phylogenetic or taxonomic data that must
#' be specified respectively by \code{trait}, \code{phylo} or \code{taxo}
#' parameter described below.
#' @param dfEdges A data frame with at least two columns: \code{idFrom} and \code{idTo}
#' descibring the set of edges. If \code{directed} is set to \code{TRUE} is also
#' describes the direction of the interaction. Two additonnal columns are checked
#' \code{value} \code{iDsite} which respectively stand for the value associated
#' with the edge and where the interaction has been observed.
#' @param trait A vector indicating columns number (or names) of \code{dfNodes} containing traits data (see \code{Details}).
#' @param phylo A vector indicating colums number (or names) of \code{dfNodes} containing phylo data (see \code{Details}).
#' @param taxo A vector indicating columns number (or names) of \code{dfNodes} containing taxo data (see \code{Details}).
#' @param binary Logical. If `TRUE`, interactions are turned into a boolean variable (see \code{Details}).
#' @param directed Logical. If `TRUE` (default value) the network is considered as directed (see \code{Details}).
#' @param dfSite A data frame with at least two columns named \code{idFrom}
#' @param dfOcc A data frame with at least two columns
#' @param verbose Logical. Should extra information be reported on progress?
#'
#' @details
#'
#' The user is required to provide specific column names to prevent from
#' generating errors. Two primary keys \code{idNodes} and \code{idSite} if
#' argument \code{dfSite} is not null.
#'
#' trait/phylo/taxo non exlusive a column be three or both. There are associated and help distinguishing methods.
#'
#' character of factors => as.charcater
#' The strength of the interactions defined in the third column of
#'
#' If no values in \code{dfEdges} then values are set to 1 is used.
#' \code{directed} => methods and => plots
#' \code{binary} => methods and option
#'
#' @return
#' An object of the class \code{alienData} is returned by \code{as.alienData}.
#' TODO: Declare more accurately the structure of object returned by the function. What has been generated from the function.
#'
#' @author Guillaume Blanchet, Kevin Cazelles & Steve Vissault
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @keywords manip
#' @keywords classes
#' @export


as.alienData <- function(dfNodes, dfEdges, trait = NULL, phylo = NULL, taxo = NULL, 
    siteEnv = NULL, traitSp = NULL, traitInd = NULL, phy = NULL, dfSite = NULL, dfOcc = NULL, 
    binary = FALSE, directed = FALSE, verbose = TRUE) {
    
    ############################## dfNodes
    dfNodes %<>% as.data.frame(stringsAsFactors = FALSE)
    dfEdges %<>% as.data.frame(stringsAsFactors = FALSE)
    ## 
    stopifnot("idNodes" %in% names(dfNodes))
    stopifnot("idFrom" %in% names(dfEdges))
    stopifnot("idTo" %in% names(dfEdges))
    ## 
    dfNodes$idNodes %<>% as.character
    dfEdges$idFrom %<>% as.character
    dfEdges$idTo %<>% as.character
    ## 
    availableMeths <- data.frame(methods = c("Co-occurence", "Direct Matching Centrality", 
        "iEat"), available = FALSE, stringsAsFactors = FALSE)
    
    ## 
    stopifnot(!any(table(dfNodes$idNodes) > 1))
    message("==> Nodes information detected")
    
    ## 
    sc <- 0
    if (is.null(trait)) {
        nmTrait <- NULL
        if (verbose) 
            message("==> No traits detected")
    } else {
        nmTrait <- names(dfNodes[, trait])
        sc <- 1
        if (verbose) 
            message(paste0("==> Traits detected: ", paste(nmTrait, collapse = ", ")))
    }
    ## 
    if (is.null(phylo)) {
        nmPhylo <- NULL
        if (verbose) 
            message("==> No phylo detected")
    } else {
        nmPhylo <- names(dfNodes[, phylo])
        sc <- 1
        if (verbose) 
            message(paste0("==> Phylo detected: ", paste(nmPhylo, collapse = ", ")))
    }
    ## 
    if (is.null(taxo)) {
        nmTaxo <- NULL
        if (verbose) 
            message("==> No taxon detected")
    } else {
        nmTaxo <- names(dfNodes[, taxo])
        sc <- 1
        if (verbose) 
            message(paste0("==> Taxo detected: ", paste(nmTaxo, collapse = ", ")))
    }
    ## 
    if (sc) {
        availableMeths$available[availableMeths$methods == "iEat"] <- TRUE
        availableMeths$available[availableMeths$methods == "Direct Matching Centrality"] <- TRUE
    }
    
    
    ############################## dfEdges
    stopifnot(all(dfEdges$idFrom %in% dfNodes$idNodes))
    stopifnot(all(dfEdges$idTo %in% dfNodes$idNodes))
    
    if (!"value" %in% names(dfEdges)) {
        if (verbose) 
            message("==> No values associated with edges, values are set to 1")
        dfEdges$value <- 1
    } else message("==> Edges values detected")
    
    if (binary) {
        dfEdges$value <- dfEdges$value > 0
        message("==> Interactions are considered as binary")
    }
    
    
    ############################## dfSite
    if (is.null(dfSite)) {
        if (verbose) 
            message("==> No site info detected")
        nbSites <- NULL
    } else {
        stopifnot("idSite" %in% names(dfSite))
        stopifnot(all(table(dfSite$idSite) == 1))
        dfSite$idSite %<>% as.character
        nmSite <- names(dfSite)
        if (verbose) 
            message(paste0("==> Site info detected: ", paste(nmSite, collapse = ", ")))
        if ("idSite" %in% names(dfEdges)) {
            stopifnot(all(dfSite$idSite %in% dfEdges$idSite))
        }
        nbSites <- nrow(dfSite)
    }
    
    
    ############################## dfOcc
    occ <- FALSE
    if ("idSite" %in% names(dfEdges) & is.null(dfOcc)) {
        stopifnot(all(dfEdges$idSite %in% dfSite$idSite))
        dfEdges$idSite %<>% as.character
        if (verbose) 
            message("==> Getting occurrence information from 'dfEdges'...")
        ## 
        dfOcc <- data.frame(id = c(dfEdges$idTo, dfEdges$idFrom), idSite = rep(dfEdges$idSite, 
            2), stringsAsFactors = FALSE) %>% unique
        ## 
        names(dfOcc)[1L] <- "idNodes"
    }
    
    
    if (!is.null(dfOcc)) {
        stopifnot("idSite" %in% names(dfOcc))
        dfOcc$idSite %<>% as.character
        stopifnot("idNodes" %in% names(dfOcc))
        ## 
        stopifnot(all(dfOcc$idSite %in% dfSite$idSite))
        stopifnot(all(dfOcc$idNodes %in% dfNodes$idNodes))
        occ <- TRUE
        if (!all(dfNodes$idNodes %in% dfOcc$idNodes)) 
            warning("Nodes without any occurrence record.")
        if (verbose) 
            message("==> Occurrence information detected")
        dfOcc$idNodes %<>% as.character
        ## 
        nbOcc <- nrow(dfOcc)
        availableMeths$available[availableMeths$methods == "Co-occurence"] <- TRUE
    } else {
        if (nbSites > 0) 
            warning("Site information provided without any occurrence")
        nbOcc <- NULL
    }
    
    
    ############################## Return results
    res <- list(nbNodes = nrow(dfNodes), nbInteractions = nrow(dfEdges), directed = directed, 
        nbSites = nbSites, nbOcc = nbOcc, dfNodes = dfNodes, dfEdges = dfEdges, dfSite = dfSite, 
        dfOcc = dfOcc, nmTrait = nmTrait, nmPhylo = nmPhylo, nmTaxo = nmTaxo, nmSite = nmSite, 
        availableMeths = availableMeths)
    
    class(res) <- "alienData"
    return(res)
}
