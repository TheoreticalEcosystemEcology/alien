#' @title Format data for the \code{alien} class
#'
#' @description This function formats the data and returns an object of class alienData.
#'
#' @param idObs A data.frame which is mandatory and will help to check consistency and prevent errors among unique identifiers of each alienData arguments. The first column (idSite) contains unique identifier of where the observation was made. The second column (idTime) is not mandatory and contains temporal information: an unique identifier at the time the sample has been taken (needed for timeseries analysis). The third column (idSpcies) is an unique identifier of the species sampled at time (idTime) and location (idSite). The fourth column is an unique identifier of individu of species (idSp) observed at time (idTime) and location (idSite).
#' @param interactPair A data.frame which contains interaction at the finest level (individuals or species). The first two columns are idFrom and idTo and determine the sens of the interaction. idFrom and idTo are unique identifier of species or individu documented in the idObs data.frame. Finaly, the thrid column is the strength of the interaction (Please see details).
#' @param coOcc A square symmetric matrix of 0s and 1s that define co-occurence patterns among pairs of species. If this matrix is not provided, the co-occurence matrix is derived from the coAbund matrix else the idObs dataframe (see return section).
#' @param coAbund A square symmetric matrix that includes any types of values, defining co-abundance patterns among pairs of species. TODO: Not implemented yet.
#' @param siteEnv A matrix or a data.frame where each column is a descriptor of the sites. TODO: siteEnv should cover the possibility that environmental variables could be taken at several times - link to idTime in idObs?.
#' @param traitSp A matrix or a data.frame where each column is a trait characterizing all species. The first column is a unique identifier of the species documented in idObs data.frame.
#' @param traitInd A matrix or a data.frame where each column is a trait characterizing an individual. The first column is a unique identifier of the individu documented in idObs data.frame.
#' @param phy An object of class 'phylo' describing the phylogenetic relationships across species (see details). TODO: Not implemented yet.
#' @param scaleSiteEnv Logical. Whether the columns of X should be centred and divided by the standard deviation. Default is TRUE.
#' @param scaleTrait Logical. Whether the rows of Tr should be centred and divided by the standard deviation. Default is TRUE.
#' @param interceptSiteEnv Logical. Whether a column of 1s should be added to X. Default is TRUE.
#' @param interceptTrait Logical. Whether a row of 1s should be added to Tr. Default is TRUE.
#' @param verbose Logical. warning and help messages are printed. Default is FALSE.
#'
#' @details
#'
#' The strength of the interactions defined in the third column of \code{interactPair} can be a 0 if no direct interaction has been observed (defined as true absence of interaction) or any numerical value. Undocumented interactions among species or individus will be assumed as NA by default.
#'
#' @return
#'
#' An object of the class \code{alienData} is returned by \code{as.alienData}.
#' TODO: Declare more accurately the structure of object returned by the function. What has been generated from the function.
#'
#' @author F. Guillaume Blanchet & Steve Vissault
#'
#' @importFrom stats sd
#' @examples
#'
#' @keywords manip
#' @keywords classes
#' @export


as.alienData <- function(idObs = NULL, interactPair = NULL, coOcc = NULL, coAbund = NULL, 
    siteEnv = NULL, traitSp = NULL, traitInd = NULL, phylo = NULL, scaleSiteEnv = FALSE, 
    scaleTrait = FALSE, interceptSiteEnv = FALSE, interceptTrait = FALSE, verbose = TRUE) {
    
    # OBJECT: idObs ================================================
    
    if (is.null(idObs)) {
        stop("idObs argument cannot be NULL")
    }
    
    # Test idObs structure
    if (!is.data.frame(idObs) & !is.matrix(idObs) & ncol(idObs) != 4) {
        stop("idObs has to be a matrix/dataframe with 4 columns")
    }
    
    if (is.matrix(idObs)) {
        idObs <- as.data.frame(idObs)
        if (verbose) 
            message("'idObs' converted as data.frame")
    }
    
    # Rename columns
    if (ncol(idObs) == 4) {
        colnames(idObs) <- c("idSite", "idTime", "idSp", "idInd")
        if (verbose) 
            message("'idObs' columns have been rename to 'idSite','idTime','idSp','idInd'")
    }
    
    # Check for duplicates rows
    if (nrow(idObs[duplicated(idObs), ]) != 0) {
        stop("some idObs entries are duplicated")
    }
    
    # Cast all columns has factors
    if (!all(sapply(idObs, class)[1:4] == "factor")) {
        idObs <- as.data.frame(lapply(idObs, as.factor))
    }
    
    # OBJECT: interactPair ================================================
    
    if (is.null(interactPair)) {
        stop("interactPair argument cannot be NULL")
    }
    
    ### Check for number of columns
    if (!is.data.frame(interactPair) & !is.matrix(interactPair) & ncol(interactPair) != 
        3) {
        stop("'interactPair' has to be a matrix/dataframe with 3 columns")
    }
    
    ### Check class
    
    ### Make sure interactPair is a data.frame
    if (is.matrix(interactPair)) {
        interactPair <- as.data.frame(interactPair)
        if (verbose) 
            message("'interactPair' converted as data.frame")
    }
    
    # Rename columns
    if (ncol(interactPair) == 3) {
        colnames(interactPair) <- c("idTo", "idFrom", "strength")
        if (verbose) 
            message("'interactPair' columns have been rename to 'idTo','idFrom','strength' ")
    }
    
    ### Make sure the first and second columns are factor
    if (!all(sapply(interactPair, class)[1:2] == "factor")) {
        interactPair$idFrom <- as.factor(interactPair$idFrom)
        interactPair$idTo <- as.factor(interactPair$idTo)
    }
    
    ### Make sure the third column is numeric
    if (!is.numeric(interactPair$strength)) {
        interactPair$strength <- as.numeric(interactPair$strength)
    }
    
    ## Check data consistency
    
    ## Check if idFrom and idTo are in levels(idSp) or levels(idInd) and not both
    ## interactPair are observations at species level OR at individual level but not
    ## both
    
    
    if (any(levels(interactPair$idFrom) %in% levels(idObs$idSp)) & any(levels(interactPair$idFrom) %in% 
        levels(idObs$idInd))) {
        stop("'idFrom' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")
    }
    
    if (any(levels(interactPair$idTo) %in% levels(idObs$idSp)) & any(levels(interactPair$idTo) %in% 
        levels(idObs$idInd))) {
        stop("'idTo' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")
    }
    
    ## WHERE interactPair are Species Check if all ids exists in idObs
    if (any(c(levels(interactPair$idFrom), levels(interactPair$idTo)) %in% levels(idObs$idSp))) {
        
        if (!all(levels(interactPair$idFrom) %in% levels(idObs$idSp))) {
            stop("Some species ids in 'idFrom' are not in 'idObs'")
        }
        
        if (!all(levels(interactPair$idTo) %in% levels(idObs$idSp))) {
            stop("Some species ids in 'idTo' are not in 'idObs'")
        }
        
    }
    
    ## WHERE interactPair are individu Check if all ids exists in idObs
    if (any(c(levels(interactPair$idFrom), levels(interactPair$idTo)) %in% levels(idObs$idInd))) {
        
        if (!all(levels(interactPair$idFrom) %in% levels(idObs$idInd))) {
            stop("Some individus ids in 'idFrom' are not in 'idObs'")
        }
        
        if (!all(levels(interactPair$idTo) %in% levels(idObs$idInd))) {
            stop("Some individus ids in 'idTo' are not in 'idObs'")
        }
        
    }
    
    # Check if rows are not duplicated
    if (nrow(interactPair[duplicated(interactPair[, c("idFrom", "idTo")]), ]) != 
        0) {
        stop("Some 'idFrom' and 'idTo' are duplicated")
    }
    
    
    
    # OBJECT: interactSp and interactInd
    # ================================================ Turn interactPair into
    # interactSp and interactInd MATRICES
    
    # if interactPair at individus level
    if (all(c(levels(interactPair$idTo), levels(interactPair$idFrom)) %in% levels(idObs$idInd))) {
        
        ## built interactInd
        nsp <- nlevels(interactPair$idTo) + nlevels(interactPair$idFrom)
        interactInd <- matrix(NA, nrow = nsp, ncol = nsp)
        colnames(interactInd) <- c(levels(interactPair$idTo), levels(interactPair$idFrom))
        rownames(interactInd) <- c(levels(interactPair$idTo), levels(interactPair$idFrom))
        
        
        for (i in 1:nrow(interactPair)) {
            interactInd[interactPair[i, "idFrom"], interactPair[i, "idTo"]] <- interactPair[i, 
                "strength"]
        }
        
        ## Retrieve Sp ids from idObs
        idFromSp <- merge(interactPair, idObs, by.x = "idFrom", by.y = "idInd")[, 
            "idSp"]
        idToSp <- merge(interactPair, idObs, by.x = "idTo", by.y = "idInd")[, "idSp"]
        
        # Create interactPair with idSp
        interactPairSp <- interactPair
        interactPairSp$idFrom <- idFromSp
        interactPairSp$idTo <- idToSp
        
        ## Aggregate TODO: WARNING - If the strength is not a count. The sum might not be
        ## appropriate.
        interactPairSp <- aggregate(strength ~ idFrom + idTo, interactPairSp, FUN = sum)
        
        ## built interactSp
        nsp <- nlevels(interactPairSp$idTo) + nlevels(interactPairSp$idFrom)
        interactSp <- matrix(NA, nrow = nsp, ncol = nsp)
        colnames(interactSp) <- c(levels(interactPairSp$idTo), levels(interactPairSp$idFrom))
        rownames(interactSp) <- c(levels(interactPairSp$idTo), levels(interactPairSp$idFrom))
        
        
        for (i in 1:nrow(interactPairSp)) {
            interactSp[interactPairSp[i, "idFrom"], interactPairSp[i, "idTo"]] <- interactPairSp[i, 
                "strength"]
        }
        
    } else if (all(c(levels(interactPair$idTo), levels(interactPair$idFrom)) %in% levels(idObs$idSp))) {
        
        # if interactPair at species level
        nsp <- nlevels(interactPair$idTo) + nlevels(interactPair$idFrom)
        interactSp <- matrix(NA, nrow = nsp, ncol = nsp)
        colnames(interactSp) <- c(levels(interactPair$idTo), levels(interactPair$idFrom))
        rownames(interactSp) <- c(levels(interactPair$idTo), levels(interactPair$idFrom))
        
        
        for (i in 1:nrow(interactPair)) {
            interactSp[interactPair[i, "idFrom"], interactPair[i, "idTo"]] <- interactPair[i, 
                "strength"]
        }
        
        # let interactInd null
        interactInd <- NULL
        
    }
    
    
    # OBJECT: traitInd ================================================
    
    if (!is.null(traitInd)) {
        
        ### Check class
        
        ### Check for number of columns
        if (!is.data.frame(traitInd) & !is.matrix(traitInd) & ncol(traitInd) != 3) {
            stop("'traitInd' has to be a matrix/dataframe with 3 columns: idInd, traitName, value")
        }
        
        
        ### Make sure interactPair is a data.frame
        if (is.matrix(traitInd)) {
            traitInd <- as.data.frame(traitInd)
            if (verbose) 
                message("'traitInd' converted as data.frame")
        }
        
        # Rename columns
        if (ncol(traitInd) == 3) {
            colnames(traitInd)[1] <- "idInd"
            colnames(traitInd)[2] <- "traitName"
            colnames(traitInd)[3] <- "value"
            if (verbose) 
                message("'traitInd' columns have been rename to: idInd, traitName, value ")
        }
        
        ### Make sure the first column idInd is a factor (all other columns are free form)
        if (!is.factor(traitInd$idInd)) {
            traitInd$idInd <- as.factor(traitInd$idInd)
        }
        
        ### Make sure the second column traitName is a factor
        if (!is.factor(traitInd$traitName)) {
            traitInd$traitName <- as.factor(traitInd$traitName)
        }
        
        ## The third column is free form
        
        ### Make sure 'idInd' levels are referenced into idObs
        if (!all(levels(traitInd$idInd) %in% levels(idObs$idInd))) {
            stop(cat("Some individu ids are not referenced in 'idObs': \n", levels(traitInd$idInd)[which(!levels(traitInd$idInd) %in% 
                levels(idObs$idInd))]))
        }
        
    }
    
    # OBJECT: traitSp ================================================
    
    if (!is.null(traitSp)) {
        
        ### Check class Check for number of columns
        if (!is.data.frame(traitSp) & !is.matrix(traitSp) & ncol(traitSp) != 3) {
            stop("'traitSp' has to be a matrix/dataframe with 3 columns: idSp, traitName, value")
        }
        
        
        ### Make sure interactPair is a data.frame
        if (is.matrix(traitSp)) {
            traitSp <- as.data.frame(traitSp)
            if (verbose) 
                message("'traitSp' converted as data.frame")
        }
        
        # Rename columns
        if (ncol(interactPair) == 3) {
            colnames(traitSp)[1] <- "idSp"
            colnames(traitSp)[2] <- "traitName"
            colnames(traitSp)[3] <- "value"
            if (verbose) 
                message("'traitSp' columns have been rename to: idSp, traitName, value ")
        }
        
        ### Make sure the first column idSp is a factor (all other columns are free form)
        if (!is.factor(traitSp$idSp)) {
            traitSp$idSp <- as.factor(traitSp$idSp)
        }
        
        ### Make sure the second column traitName is a factor
        if (!is.factor(traitSp$traitName)) {
            traitSp$traitName <- as.factor(traitSp$traitName)
        }
        
        ### Make sure 'idSp' levels are referenced into idObs
        if (!all(levels(traitSp$idSp) %in% levels(idObs$idSp))) {
            stop(cat("Some species ids are not referenced in 'idObs': \n", levels(traitSp$idSp)[which(!levels(traitSp$idSp) %in% 
                levels(idObs$idSp))]))
        }
        
    }
    
    # OBJECT: coOcc ================================================
    
    if (!is.null(coOcc)) {
        
        # co-occurence matrix has been provided by the user
        coOccFrom <- "user"
        
        if (nrow(coOcc) != ncol(coOcc)) {
            stop("'coOcc' should be a square table")
        }
        
        ### Check if symmetry
        if (!isSymmetric(coOcc)) {
            stop("'coOcc' need to be a symmetric matrix")
        }
        
        ### Check positive definiteness
        if (!all(unique(coOcc) == c(0, 1))) {
            stop("'coOcc' should include only 0s and 1s")
        }
        
        ### Check if colnames and rownames are not null
        if (is.null(rownames(coOcc)) | is.null(colnames(coOcc))) {
            stop("'coOcc' cannot have rownames or/and colnames NULL, both should referred to idSp in the 'idObs' data.frame")
        }
        
        ### Check if colnames and rownames are referenced in idObs
        if (!all(c(rownames(coOcc), colnames(coOcc)) %in% levels(idObs$idSp))) {
            stop("Some unique identifiers of species provided in rows and columns names of 'coOcc' are not documented in 'idObs' data.frame")
        }
        
        
    } else if (!is.null(coAbund)) {
        # Generate coOcc from coAbund if is available, else build coOcc based on
        # interactSp.
        coOccFrom <- "coAbund"
        coOcc <- ifelse(coAbund > 0, 1, 0)
        
    } else {
        
        coOccFrom <- "idObs"
        
        occSite <- idObs
        occSite$idSiteTime <- as.factor(paste(occSite$idSite, occSite$idTime, sep = "-"))
        occSite <- reshape2::dcast(idSiteTime ~ idSp, data = occSite, value.var = "idSiteTime", 
            fun.aggregate = length)
        occSite <- as.matrix(occSite[, -1])
        
        coOcc <- crossprod(occSite)
        coOcc <- ifelse(coOcc > 0, 1, 0)
        
    }
    
    
    # OBJECT: siteEnv ================================================
    
    
    if (!is.null(siteEnv)) {
        ### Check for number of columns
        if (!is.data.frame(siteEnv) & !is.matrix(siteEnv) & ncol(siteEnv) != 3) {
            stop("'siteEnv' has to be a matrix/dataframe with at least 3 columns: idSite, envName, value")
        }
        
        ### Make sure traitSp is a data.frame
        if (is.matrix(siteEnv)) {
            siteEnv <- as.data.frame(siteEnv)
            if (verbose) 
                message("'siteEnv' converted as data.frame")
        }
        
        # Rename columns
        if (ncol(siteEnv) == 3) {
            colnames(siteEnv)[1] <- "idSite"
            colnames(siteEnv)[2] <- "envName"
            colnames(siteEnv)[3] <- "value"
            if (verbose) 
                message("'siteEnv' columns have been rename to: 'idSite', 'envName', 'value'")
        }
        
        ### Make sure the first column is a factor (all other columns are free form)
        if (!is.factor(siteEnv$idSite)) {
            siteEnv$idSite <- as.factor(siteEnv$idSite)
        }
        
        ### TODO: The rest 2:ncol has to be in numeric to be scale
        
        ### Make sure 'idSite' levels are referenced into idObs
        if (!all(levels(siteEnv$idSite) %in% levels(idObs$idSite))) {
            stop(cat("Some site ids are not referenced in 'idObs': \n", levels(siteEnv$idSite)[which(!levels(siteEnv$idSite) %in% 
                levels(idObs$idSite))]))
        }
        
    }
    
    # TRANSFORM: SCALE AND INTERCEPT OPTIONS
    # ================================================
    
    ## For the moment, Force all scales and intercepts to be FALSE
    scaleSiteEnv <- FALSE
    scaleTrait <- FALSE
    interceptSiteEnv <- FALSE
    interceptTrait <- FALSE
    
    ## TODO: for siteEnv, traitSp, traitInd 1. Turn wide format into long format 2.
    ## Test each columns is.numeric 3. if yes, scale() 4. go back to the original
    ## format: wide.
    
    ### Add an intercept to siteEnv and scale
    if (!is.null(siteEnv)) {
        if (interceptSiteEnv) {
            if (scaleSiteEnv) {
                ### Check if any columns have a null variance
                zeroVar <- which(apply(siteEnv, 2, sd) == 0)
                if (length(zeroVar) != 0) {
                  siteEnv[, -zeroVar] <- scale(siteEnv[, -zeroVar])
                  warning(paste(colnames(siteEnv)[zeroVar], "are explanatory variable(s) with a variance of 0, for this reason no intercept were added, check to make sure this is OK"))
                } else {
                  siteEnv <- cbind(1, scale(siteEnv))
                  colnames(siteEnv)[1] <- "Intercept"
                }
            } else {
                siteEnv <- cbind(1, siteEnv)
                colnames(siteEnv)[1] <- "Intercept"
            }
        } else {
            if (scaleSiteEnv) {
                ### Check if any columns have a null variance
                zeroVar <- which(apply(siteEnv, 2, sd) == 0)
                if (length(zeroVar) != 0) {
                  siteEnv[, -zeroVar] <- scale(siteEnv[, -zeroVar])
                  warning(paste(colnames(siteEnv)[zeroVar], "are explanatory variable(s) with a variance of 0, make sure this is OK"))
                } else {
                  siteEnv <- scale(siteEnv)
                }
            }
        }
    }
    
    ### Add an intercept to traitSp
    if (!is.null(traitSp)) {
        if (interceptTrait) {
            if (scaleTrait) {
                ### Check if any columns have a null variance
                zeroVar <- which(apply(traitSp, 2, sd) == 0)
                if (length(zeroVar) != 0) {
                  traitSp[, -zeroVar] <- scale(traitSp[, -zeroVar])
                  warning(paste(colnames(traitSp)[zeroVar], "are traitSp(s) with a variance of 0, for this reason no intercept were added, check to make sure this is OK"))
                } else {
                  traitSp <- cbind(1, scale(traitSp))
                  colnames(traitSp)[1] <- "Intercept"
                }
            } else {
                traitSp <- cbind(1, traitSp)
                colnames(traitSp)[1] <- "Intercept"
            }
        } else {
            if (scaleTrait) {
                ### Check if any columns have a null variance
                zeroVar <- which(apply(traitSp, 1, sd) == 0)
                if (length(zeroVar) != 0) {
                  traitSp[-zeroVar, ] <- scale(traitSp[-zeroVar, ])
                  warning(paste(rownames(traitSp)[zeroVar], "are traits with a variance of 0, make sure this is OK"))
                }
            }
        }
    }
    
    ### TODO: traitInd has also to be scaled
    
    #### Check classes for coOcc, coAbund, interactSp, interactInd
    
    if (!is.null(interactSp)) {
        if (!is.matrix(interactSp)) {
            interactSp <- as.matrix(interactSp)
            if (verbose) 
                message("'interactSp' was converted to a matrix")
        }
    }
    
    if (!is.null(interactInd)) {
        if (!is.matrix(interactInd)) {
            interactInd <- as.matrix(interactInd)
            if (verbose) 
                message("'interactInd' was converted to a matrix")
        }
    }
    
    if (!is.null(coOcc)) {
        if (!is.matrix(coOcc)) {
            coOcc <- as.matrix(coOcc)
            if (verbose) 
                message("'coOcc' was converted to a matrix")
        }
    }
    
    if (!is.null(coAbund)) {
        if (!is.matrix(coAbund)) {
            coAbund <- as.matrix(coAbund)
            if (verbose) 
                message("'coAbund' was converted to a matrix")
        }
    }
    
    # ================== Return results ==================
    
    
    ## Create res list with NULL
    res <- list(idObs = idObs, interactPair = interactPair, interactSp = interactSp, 
        interactInd = interactInd, coOcc = coOcc, coAbund = coAbund, siteEnv = siteEnv, 
        traitSp = traitSp, traitInd = traitInd, phylo = phylo)
    
    attr(res, "coOccSource") <- coOccFrom
    attr(res, "scaleSiteEnv") <- scaleSiteEnv
    attr(res, "scaleTrait") <- scaleTrait
    attr(res, "interceptSiteEnv") <- interceptSiteEnv
    attr(res, "interceptTrait") <- interceptTrait
    
    class(res) <- "alienData"
    return(res)
}
