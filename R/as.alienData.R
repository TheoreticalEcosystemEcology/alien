#' @title Format data for the \code{alien} class
#'
#' @description This function formats the data and returns an object of class alienData.
#'
#' @param dfSpecies A data frame with a least two colomuns. A given row describes one
#' observation. The site indentifiers (\code{idSp}, must be unique) has to come first
#' and the species identifier (\code{idSp}) in second. Two
#' extra columns could be provided by the user: \code{idTime} adding a timestamp to the observations and
#' \code{idInd}, an identifier of individus. If these two extra columns
#' are not supplied, \code{NA} will be added. Columns must be correctly ordered.
#' @param dfInteract A data frame with three columns which contains interaction at the finest level
#' (individus or species). The first two columns are \code{idFrom} and \code{idTo} and
#' determine the sens of the interaction. idFrom and \code{idTo} are unique identifier
#' of species or individu documented in the \code{idObs} data frame. Finaly, the thrid
#' column is the strength of the interaction (Please see details).
#' @param trait A square symmetric matrix of 0s and 1s that define co-occurence patterns among pairs of species. If this matrix is not provided, the co-occurence matrix is derived from the coAbund matrix else the \code{idObs} dataframe (see return section).
#' @param phylo A square symmetric matrix that includes any types of values, defining co-abundance patterns among pairs of species. TODO: Not implemented yet.
#' @param taxo A matrix or a data frame where each column is a descriptor of the sites. TODO: siteEnv should cover the possibility that environmental variables could be taken at several times - link to idTime in idObs?.
#' @param binary Logical
#' @param directed Logical
#' @param dfSite Logical
#' @param dfOcc Logical
#' @param verbose Logical
#'
#' @details
#' \code{idObs} is used to check consistency and prevent errors among unique
#' identifiers of each alienData arguments.
#'
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
#' @import ape
#'
#' @keywords manip
#' @keywords classes
#' @export


as.alienData <- function(dfSpecies, dfInteract, trait = NULL, phylo = NULL, taxo = NULL, 
    siteEnv = NULL, traitSp = NULL, traitInd = NULL, phy = NULL, dfSite = NULL, dfOcc = NULL, 
    verbose = TRUE) {
    
    #### checks
    stopifnot("idSp" %in% names(dfSpecies))
    ## 
    dfMethAvail <- data.frame(methods = c("Direct Matching Centrality", "Co-occurence", 
        "iEat"), available = FALSE, stringsAsFactors = FALSE)
    ## 
    if ("idInd" %in% names(dfSpecies)) {
        indiv <- TRUE
        stopifnot(!any(table(dfSpecies$idInd) > 1))
        stopifnot(!any(dfSpecies$idInd %in% dfSpecies$idSp))
        if (verbose) 
            message("==> Information at the individual level detected")
    } else {
        if (verbose) {
            message("==> Information at the species level detected")
            stopifnot(!any(table(dfSpecies$idSp) > 1))
        }
    }
    
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
    if (any(c(tra, phy, tax))) 
        dfMethAvail$available[which(dfMethAvail$methods == "iEat")] <- TRUE
    
    #### dfInteract
    
    #### Dfsite
    
    
    # } } # ================== Return results ================== ## Create res list
    # with NULL res <- list(idObs = idObs, interactPair = interactPair, interactSp =
    # interactSp, interactInd = interactInd, coOcc = coOcc, coAbund = coAbund,
    # siteEnv = siteEnv, traitSp = traitSp, traitInd = traitInd, phy = phy)
    
    # attr(res, 'coOccSource') <- coOccFrom attr(res, 'scaleSiteEnv') <- scaleSiteEnv
    # attr(res, 'scaleTrait') <- scaleTrait attr(res, 'interceptSiteEnv') <-
    # interceptSiteEnv attr(res, 'interceptTrait') <- interceptTrait
    
    res <- list(nbSpecies = nrow(dfSpecies), dfMethAvail = dfMethAvail)
    
    class(res) <- "alienData"
    return(res)
}
