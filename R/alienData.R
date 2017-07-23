#' @title  Formatting data and return an \code{alienData} object
#'
#' @description \code{alienData} is used to check and format data, if correct
#' it returns an object of class \code{alienData}.
#'
#' @param dfNodes A vector or a data frame with at least one column named \code{idNodes} providing
#' unique identifiers for each species (or individuals) of the dataset. The remainig
#' columns could be either traits or phylogenetic or taxonomic data that must
#' be specified respectively by \code{trait}, \code{phylo} or \code{taxo}
#' parameter described below (otherwise they are ignored).
#' @param dfEdges A data frame with at least two columns: \code{idFrom} and \code{idTo}
#' descibring the set of edges (links between nodes). If \code{directed} is set
#' to \code{TRUE} then the interaction is directed from \code{idFrom} to \code{idTo}.
#' Directed interactions for consumer/resource interactions correspond to a transfer
#' of energy so that \code{idFrom} is the resource and \code{idTo} is the consumer.
#' The presence of two additonnal columns are checked: \code{value} and \code{idSite}
#' which respectively provide the values associated with edges (if absent, they are
#' set to 1) and the identifier of the site where the interaction has been obsereved
#' (see details).
#' @param trait A vector indicating columns number (or names) of \code{dfNodes} containing traits data (see \code{Details}).
#' @param phylo A vector indicating colums number (or names) of \code{dfNodes} containing phylo data (see \code{Details}).
#' @param taxo A vector indicating columns number (or names) of \code{dfNodes} containing taxo data (see \code{Details}).
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
#' The user is required to provide specific column names to prevent the function
#' from returning errors. Two primary keys \code{idNodes} and \code{idSite} (if site
#' information are provided) are used to check the consistency of the data.
#' First, all values taken by \code{idFrom} and \code{idTo} column in \code{dfEdges}
#' must be found in \code{idNodes} column of \code{dfNodes} (otherwise an error
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
#' An object of the class \code{alienData} is returned .
#'
#' @author Guillaume Blanchet, Kevin Cazelles & Steve Vissault
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @keywords manip
#' @keywords classes
#' @export


alienData <- function(dfNodes, dfEdges, trait = NULL, phylo = NULL, taxo = NULL, 
    dfSites = NULL, siteEnv = NULL, dfOcc = NULL, directed = FALSE, verbose = TRUE) {
    
    ############################## 
    osaf <- options()
    options(stringsAsFactors = FALSE)
    on.exit(options(osaf))
    
    ############################## dfNodes
    if (is.vector(dfNodes)) {
        dfNodes <- data.frame(ID = as.character(dfNodes))
    }
    ## 
    dfNodes %<>% as.data.frame
    dfEdges %<>% as.data.frame
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
        "iEat"), available = FALSE)
    
    ## 
    stopifnot(!any(table(dfNodes$idNodes) > 1))
    if (verbose) 
        message("==> Nodes information detected")
    
    ## 
    sc <- 0
    if (is.null(trait)) {
        nmTrait <- NULL
        if (verbose) 
            message("==> No traits detected")
    } else {
        nmTrait <- names(dfNodes[, trait, drop = FALSE])
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
        nmPhylo <- names(dfNodes[, phylo, drop = FALSE])
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
        nmTaxo <- names(dfNodes[, taxo, drop = FALSE])
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
    idn <- which(!dfNodes$idNodes %in% c(dfEdges$idFrom, dfEdges$idTo))
    print(idn)
    if (length(idn)) 
        warning(paste0("Unlinked nodes: ", paste(dfNodes$idNodes[idn], collapse = ", ")))
    
    if (!"value" %in% names(dfEdges)) {
        if (verbose) 
            message("==> No Edges' value detected, values are set to 1")
        dfEdges$value <- 1
    } else if (verbose) 
        message("==> Edges' values detected")
    
    
    ############################## dfSites
    if (is.null(dfSites)) {
        if ("idSite" %in% names(dfEdges)) {
            if (verbose) 
                message("==> Sites' ID are provided by dfEdges")
            dfSites <- data.frame(idSite = unique(dfEdges$idSite))
            nbSites <- nrow(dfSites)
        } else {
            if (verbose) 
                message("==> No site info detected")
            nbSites <- NULL
        }
        
    } else {
        stopifnot("idSite" %in% names(dfSites))
        stopifnot(all(table(dfSites$idSite) == 1))
        dfSites$idSite %<>% as.character
        if ("idSite" %in% names(dfEdges)) 
            stopifnot(all(dfSites$idSite %in% dfEdges$idSite))
        nbSites <- nrow(dfSites)
    }
    if (!is.null(nbSites)) {
        nmSite <- names(dfSites)
        if (verbose) 
            message(paste0("==> Site info detected: ", paste(nmSite, collapse = ", ")))
    } else {
        nmSite <- NULL
    }
    
    ############################## dfOcc
    occ <- FALSE
    if ("idSite" %in% names(dfEdges) & is.null(dfOcc)) {
        stopifnot(all(dfEdges$idSite %in% dfSites$idSite))
        dfEdges$idSite %<>% as.character
        if (verbose) 
            message("==> Getting occurrence information from 'dfEdges'...")
        ## 
        dfOcc <- data.frame(id = c(dfEdges$idTo, dfEdges$idFrom), idSite = rep(dfEdges$idSite, 
            2)) %>% unique
        ## 
        names(dfOcc)[1L] <- "idNodes"
    }
    
    
    if (!is.null(dfOcc)) {
        stopifnot("idSite" %in% names(dfOcc))
        dfOcc$idSite %<>% as.character
        stopifnot("idNodes" %in% names(dfOcc))
        ## 
        stopifnot(all(dfOcc$idSite %in% dfSites$idSite))
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
        if (!is.null(nbSites)) 
            warning("Site information provided without any occurrence")
        nbOcc <- NULL
    }
    
    
    ############################## Return results
    res <- list(nbNodes = nrow(dfNodes), nbEdges = nrow(dfEdges), directed = directed, 
        nbSites = nbSites, nbOcc = nbOcc, dfNodes = dfNodes, dfEdges = dfEdges, dfSites = dfSites, 
        dfOcc = dfOcc, nmTrait = nmTrait, nmPhylo = nmPhylo, nmTaxo = nmTaxo, nmSite = nmSite, 
        availableMeths = availableMeths)
    
    class(res) <- "alienData"
    return(res)
}
