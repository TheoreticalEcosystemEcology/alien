#' @title Format data for the \code{alien} class
#'
#' @description This function formats the data and returns an object of class alienData.
#'
#' @param dfSpecies A data frame with at least one column named \code{idSp} providing
#' unique identifiers for each species identified within the dataset. Optionnaly,
#' a \code{idSp} column could be specied and will be detected as information on
#' individuals. The remainig columns could be either traits or phylogenetic or taxonomic
#' data that must be specified respectively by \code{trait}, \code{phylo} or \code{taxo}
#' parameter described below.
#' @param dfInteract A data frame with at least two columns: \code{idFrom}  which contains interaction at the finest level
#' (individus or species). The first two columns are \code{idFrom} and \code{idTo} and
#' determine the sens of the interaction. idFrom and \code{idTo} are unique identifier
#' of species or individu documented in the \code{idObs} data frame. Finaly, the thrid
#' column is the strength of the interaction (Please see details).
#' @param trait A square symmetric matrix of 0s and 1s that define co-occurence patterns among pairs of species. If this matrix is not provided, the co-occurence matrix is derived from the coAbund matrix else the \code{idObs} dataframe (see return section).
#' @param phylo A square symmetric matrix that includes any types of values, defining co-abundance patterns among pairs of species. TODO: Not implemented yet.
#' @param taxo A matrix or a data frame where each column is a descriptor of the sites. TODO: siteEnv should cover the possibility that environmental variables could be taken at several times - link to idTime in idObs?.
#' @param binary Logical. Should the
#' @param directed Logical. (see \code{Details}).
#' @param dfSite A data frame with at least two columns named \code{idFrom}
#' @param dfOcc Logical
#' @param verbose Logical
#'
#' @details
#'
#' The user is required to provide specific column names to prevent from generating errors.
#'
#' trait/phylo/taxo non exlusive a column be three or both. There are associated and help distinguishing methods.
#'
#' character of factors => as.charcater
#' The strength of the interactions defined in the third column of
#' \code{interactPair} can be a 0 if no direct interaction has been observed
#' (defined as true absence of interaction) or any numerical value.
#' WARNING: interactSp is aggregating the information at the species level using the sum function by default.
#' Undocumented interactions among species or individus will be assumed as NA
#' by default.
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


as.alienData <- function(dfSpecies, dfInteract, trait = NULL, phylo = NULL, taxo = NULL, 
    siteEnv = NULL, traitSp = NULL, traitInd = NULL, phy = NULL, dfSite = NULL, dfOcc = NULL, 
    binary = FALSE, directed = FALSE, verbose = TRUE) {
    
    ############################## dfSpecies
    dfSpecies %<>% as.data.frame(stringsAsFactors = FALSE)
    dfInteract %<>% as.data.frame(stringsAsFactors = FALSE)
    ## 
    stopifnot("idSp" %in% names(dfSpecies))
    stopifnot("idFrom" %in% names(dfInteract))
    stopifnot("idTo" %in% names(dfInteract))
    ## 
    dfSpecies$idSp %<>% as.character
    dfInteract$idFrom %<>% as.character
    dfInteract$idTo %<>% as.character
    ## 
    dfMethAvail <- data.frame(methods = c("Direct Matching Centrality", "Co-occurence", 
        "iEat"), available = FALSE, stringsAsFactors = FALSE)
    ## 
    if ("idInd" %in% names(dfSpecies)) {
        indiv <- TRUE
        # dfInteract$idInd %<>% as.character()
        stopifnot(!any(table(dfSpecies$idInd) > 1))
        stopifnot(!any(dfSpecies$idInd %in% dfSpecies$idSp))
        nbIndividuals <- nrow(dfSpecies)
        if (verbose) 
            message("==> Information at the individual level detected")
    } else {
        if (verbose) {
            nbIndividuals <- NULL
            message("==> Information at the species level detected")
            stopifnot(!any(table(dfSpecies$idSp) > 1))
        }
    }
    nbSpecies <- dfSpecies$idSp %>% unique %>% length
    
    #### 
    tra <- phy <- tax <- TRUE
    ## 
    if (is.null(trait)) {
        tra <- FALSE
        if (verbose) 
            message("==> No trait detected")
    } else {
        nmTrait <- names(dfSpecies[, trait])
        if (verbose) 
            message(paste("==> Traits detected: ", nmTrait, sep = " "))
    }
    ## 
    if (is.null(phylo)) {
        phy <- FALSE
        if (verbose) 
            message("==> No phylogeny detected")
    } else {
        nmPhylo <- names(dfSpecies[, phylo])
        if (verbose) 
            message(paste("==> Phylo detected: ", nmPhylo, sep = " "))
    }
    ## 
    if (is.null(taxo)) {
        tax <- FALSE
        if (verbose) 
            message("==> No taxonomy detected")
    } else {
        nmTaxo <- names(dfSpecies[, taxo])
        if (verbose) 
            message(paste("==> Taxonomy detected: ", nmTaxo, sep = " "))
    }
    ## 
    if (any(c(tra, phy, tax))) {
        dfMethAvail$available[dfMethAvail$methods == "iEat"] <- TRUE
        dfMethAvail$available[dfMethAvail$methods == "Direct Matching Centrality"] <- TRUE
    }
    
    
    ############################## dfInteract
    indint <- FALSE  # Interactions described at the individual level?
    if (indiv) {
        if (all(dfInteract$idFrom %in% dfSpecies$idInd)) {
            stopifnot(all(dfInteract$idTo %in% dfSpecies$idInd))
            indint <- TRUE
            if (verbose) 
                message("==> Interactions described at the individual level")
            
        } else {
            stopifnot(all(dfInteract$idFrom %in% dfSpecies$idSp))
            stopifnot(all(dfInteract$idTo %in% dfSpecies$idSp))
            if (verbose) 
                message("==> Interactions described at the species level")
        }
    } else {
        stopifnot(all(dfInteract$idFrom %in% dfSpecies$idSp))
        stopifnot(all(dfInteract$idTo %in% dfSpecies$idSp))
        if (verbose) 
            message("==> Interactions described at the species level")
    }
    ## 
    if (!"value" %in% names(dfInteract)) 
        dfInteract$value <- 1
    if (binary) {
        dfInteract$value <- dfInteract$value > 0
    }
    ## 
    if (indint) {
        interLevel <- "individuals"
    } else interLevel <- "species"
    ### 
    nbInteractions <- nrow(dfInteract)
    
    
    
    ############################## dfSite
    if (is.null(dfSite)) {
        if (verbose) 
            message("==> No site information detected")
        nbSites <- NULL
    } else {
        stopifnot("idSite" %in% names(dfSite))
        stopifnot(all(table(dfSite$idSite) == 1))
        dfSite$idSite %<>% as.character
        nmSite <- names(dfSite)
        if (verbose) 
            message(paste("==> Site information detected:", nmSite, sep = " "))
        if ("idSite" %in% names(dfInteract)) {
            stopifnot(all(dfSite$idSite %in% dfInteract$idSite))
        }
        nbSites <- nrow(dfSite)
    }
    
    
    
    ############################## dfOcc
    occ <- FALSE
    if (is.null(dfOcc)) {
        if ("idSite" %in% names(dfInteract)) {
            stopifnot(all(dfInteract$idSite %in% dfSite$idSite))
            if (verbose) 
                message("==> Trying to get occurrence information from 'dfInteract'...")
            ## 
            dfOcc <- data.frame(id = c(dfInteract$idTo, dfInteract$idForom), idSite = rep(dfInteract$idSite, 
                2), stringsAsFactors = FALSE) %>% unique
            ## 
            if (indint) {
                names(dfOcc)[1L] <- "idInd"
                dfSite$idInd %<>% as.character
            } else {
                names(dfOcc)[1L] <- "idSp"
                dfSite$idSp %<>% as.character
            }
            occ <- TRUE
            if (verbose) 
                message("==> Occurrence information detected")
        } else {
            if (nrow(dfSite)) {
                warning("Site information provided without any occurrence")
            }
            if (verbose) 
                message("==> No occurrence information")
        }
        if (verbose) 
            message("==> No occurrence information")
    } else {
        stopifnot("idSite" %in% names(dfOcc))
        stopifnot(all(dfOcc$idSite %in% dfSite$idSite))
        if (indint) {
            stopifnot("idInd" %in% names(dfOcc))
            stopifnot(all(dfOcc$idInd %in% dfSpecies$idInd))
            if (!all(dfOcc$idInd %in% dfSpecies$idInd)) {
                warning("Individuals without any occurrence record.")
            }
            dfSite$idInd %<>% as.character
            occ <- TRUE
            if (verbose) 
                message("==> Occurrence information detected")
        } else {
            stopifnot("idSp" %in% names(dfOcc))
            stopifnot(all(dfOcc$idSp %in% dfSpecies$idSp))
            if (!all(dfOcc$idInd %in% dfSpecies$idInd)) {
                warning("Species without any occurrence record.")
            }
            dfSite$idSp %<>% as.character
        }
    }
    ## 
    if (occ) {
        nbOcc <- nrow(dfOcc)
        dfMethAvail$available[dfMethAvail$methods == "Co-occurence"] <- TRUE
    } else nbOcc <- NULL
    
    ############################## Return results
    res <- list(nbSpecies = nbSpecies, nbIndividuals = nbIndividuals, nbInteractions = nbInteractions, 
        interLevel = interLevel, directed = directed, nbSites = nbSites, nbOcc = nbOcc, 
        dfSpecies = dfSpecies, dfInteract = dfInteract, dfSite = dfSite, dfOcc = dfOcc, 
        dfMethAvail = dfMethAvail)
    
    class(res) <- "alienData"
    return(res)
}
