#' @title  Formatting data and return an \code{alienData} object
#'
#' @description \code{alienData} is used to check and format data, if correct
#' it returns an object of class \code{alienData}.
#'
#' @param nodes A vector, data.frame or list of data.frame with at least one column named \code{idNodes} providing
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
#' In the \code{nodes} argument, each layer of the list needs to include information about a particular layer of species in the network. If all species can potentially interact among each other, only a vector or a data.frame can be included. 
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

    #### handle options
    osaf <- options()
    options(stringsAsFactors = FALSE)
    on.exit(options(osaf))

    #### nodes 
    # Vector
    if (is.atomic(nodes)) {
      nodes <- list(df1 = data.frame(ID = as.character(nodes)))
    }
    
    # data.frame
    if (is.data.frame(nodes)) {
      nodes <- list(df1 = nodes)
    }
   
    # List
    if(is.list(nodes)){
      checkDf <- sapply(nodes, is.data.frame)
      if(!all(checkdf)){
        nodesNoDf <- which(!checkdf)
        for(i in nodesNoDf){
          if(is.vector(nodes[[i]])){
            nodes[[i]] <- data.frame(idNodes = as.character(nodes[[i]]))
          }
        }
      }
    }
    
    ### ICI ###
    ### ICI ###
    ### ICI ###
    ##
    dfEdges %<>% as.data.frame
    
    ##
    stopifnot("idNodes" %in% names(nodes))
    stopifnot("idFrom" %in% names(dfEdges))
    stopifnot("idTo" %in% names(dfEdges))
    
    ##
    nodes$idNodes %<>% as.character
    dfEdges$idFrom %<>% as.character
    dfEdges$idTo %<>% as.character
    
    ##
    availableMeths <- data.frame(methods = c("Co-occurence", "Direct Matching Centrality",
        "iEat"), available = FALSE)

    ##
    stopifnot(!any(table(nodes$idNodes) > 1))
    if (verbose)
        message("==> Nodes information detected")

    ##
    sc <- 0
    if (is.null(trait)) {
        nmTrait <- NULL
        if (verbose)
            message("==> No traits detected")
    } else {
        nmTrait <- names(nodes[, trait, drop = FALSE])
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
        nmPhylo <- names(nodes[, phylo, drop = FALSE])
        sc <- 1
        if (verbose)
            message(paste0("==> Phylo detected: ", paste(nmPhylo, collapse = ", ")))
    }
    ##
    if (sc) {
        availableMeths$available[availableMeths$methods == "iEat"] <- TRUE
        availableMeths$available[availableMeths$methods == "Direct Matching Centrality"] <- TRUE
    }


    ############################## dfEdges
    stopifnot(all(dfEdges$idFrom %in% nodes$idNodes))
    stopifnot(all(dfEdges$idTo %in% nodes$idNodes))
    idn <- which(!nodes$idNodes %in% c(dfEdges$idFrom, dfEdges$idTo))
    if (length(idn))
        warning(paste0("Unlinked nodes: ", paste(nodes$idNodes[idn], collapse = ", ")))

    if (!"value" %in% names(dfEdges)) {
        if (verbose)
            message("==> No Edges' value detected, values are set to 1")
        dfEdges$value <- 1
    } else if (verbose)
        message("==> Edges' values detected")


    #### dfSites
    nmSite <- NULL
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
        ##
        if (!is.null(siteEnv)) {
            ##
            dfSites <- cbind(idSite = dfSites$idSite, dfSites[, siteEnv, drop = FALSE])
            dfSites <- dfSites[, unique(names(dfSites))]
            ##
        }
        if (ncol(dfSites) > 1) {
            nmSite <- names(dfSites)[-1L]
            if (verbose)
                message(paste0("==> Site info detected: ", paste(nmSite, collapse = ", ")))
        } else if (verbose)
            message("==> No site info detected")
        ##
        dfSites$idSite %<>% as.character
        if ("idSite" %in% names(dfEdges)) {
            if (!all(dfSites$idSite %in% dfEdges$idSite)) {
                warnings("Sites without interaction records")
            }
        }
        nbSites <- nrow(dfSites)
    }

    #### dfOcc
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
        stopifnot(all(dfOcc$idNodes %in% nodes$idNodes))
        occ <- TRUE
        if (!all(nodes$idNodes %in% dfOcc$idNodes))
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


    #### Return results
    res <- list(nodes = nodes, dfEdges = dfEdges, dfSites = dfSites,
                dfOcc = dfOcc, info = list(nbNodes = nrow(nodes),
                                           nbEdges = nrow(dfEdges),
                                           directed = directed, 
                                           nbSites = nbSites, 
                                           nbOcc = nbOcc, 
                                           nmTrait = nmTrait, 
                                           nmSite = nmSite, 
                                           availableMeths = availableMeths))
    class(res) <- "alienData"
    res
}
