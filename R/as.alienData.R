#' @title Format data for the \code{alien} class
#'
#' @description This functions is used to format the data
#'
#' @param idObs A data.frame which is mandatory and will help to check consistency and prevent errors among unique identifiers of each alienData arguments. The first column (idSite) contains unique identifier of where the observation was made. The second column (idTime) is not mandatory and contains temporal information: an unique identifier at the time the sample has been taken (needed for timeseries analysis). The third column (idSpcies) is an unique identifier of the species sampled at time (idTime) and location (idSite). The fourth column is an unique identifier of individu of species (idSp) observed at time (idTime) and location (idSite).
#' @param interactPair A data.frame which contains interaction at the finest level (individus or species). The first two columns are idFrom and idTo and determine the sens of the interaction. idFrom and idTo are unique identifier of species or individu documented in the idObs data.frame. Finaly, the thrid column is the strength of the interaction (Please see details).
#' @param coOcc A square symmetric matrix of 0s and 1s that define co-occurence patterns among pairs of species. If this matrix is not provided, the co-occurence matrix is derived from the coAbund matrix else the interactSp matrix (see return section).
#' @param coAbund A square symmetric matrix that includes any types of values, defining co-abundance patterns among pairs of species. TODO: Not implemented yet.
#' @param siteEnv A matrix or a data.frame where each column is a descriptor of the sites. TODO: siteEnv should cover the possibility that environmental variables could be taken at several times.
#' @param traitSp A matrix or a data.frame where each column is a trait characterizing all species. The first column is a unique identifier of the species documented in idObs data.frame.
#' @param traitInd A matrix or a data.frame where each column is a trait characterizing an individual. The first column is a unique identifier of the individu documented in idObs data.frame.
#' @param phylo A square symmetric matrix describing the phylogenetic relationships between pairs of all species (see details). TODO: Not implemented yet.
#' @param scaleSiteEnv Logical. Whether the columns of X should be centred and divided by the standard deviation. Default is TRUE.
#' @param scaleTrait Logical. Whether the rows of Tr should be centred and divided by the standard deviation. Default is TRUE.
#' @param interceptSiteEnv Logical. Whether a column of 1s should be added to X. Default is TRUE.
#' @param interceptTrait Logical. Whether a row of 1s should be added to Tr. Default is TRUE.
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
    siteEnv = NULL, traitSp = NULL, traitInd = NULL, phylo = NULL, scaleSiteEnv = TRUE, 
    scaleTrait = TRUE, interceptSiteEnv = TRUE, interceptTrait = TRUE) {
    
    # ===== Test which args exists
    
    args <- c("idObs", "coOcc", "coAbund", "siteEnv", "traitSp", "traitInd", "phylo", 
        "location")
    
    exist_args <- sapply(args, exists)
    exist_args <- names(exist_args[exist_args == TRUE])
    
    # OBJECT: idObs ================================================
    
    if (!("idObs" %in% exist_args)) {
        stop("idObs argument cannot be NULL")
    }
    
    # Test idObs structure
    if (!is.data.frame(idObs) && !is.matrix(idObs) && ncol(idObs) != 4) {
        stop("idObs has to be a matrix/dataframe with 4 columns")
    }
    
    if (is.matrix(idObs)) {
        idObs <- as.data.frame(idObs)
        message("'idObs' converted as data.frame")
    }
    
    # Rename columns
    if (ncol(idObs) == 4) {
        colnames(idObs) <- c("idSite", "idTime", "idSp", "idInd")
        message("'idObs' columns have been rename to 'idSite','idTime','idSp','idInd'")
    }
    
    # Check for duplicates rows
    if (nrow(idObs[duplicated(idObs), ]) != 0) {
        stop(cat("some idObs entries are duplicated: \n", idObs[duplicated(idObs), 
            ]))
    }
    
    # Cast all columns has factors
    if (!all(sapply(idObs, class)[1:4] == "factor")) {
        idObs <- as.data.frame(lapply(idObs, as.factor))
    }
    
    # OBJECT: interactPair ================================================
    
    if (!is.null(interactPair)) {
        ### Check for number of columns
        if (!is.data.frame(interactPair) && !is.matrix(interactPair) && ncol(interactPair) != 
            3) {
            stop("'interactPair' has to be a matrix/dataframe with 3 columns")
        }
        
        ### Check class
        
        ### Make sure interactPair is a data.frame
        if (is.matrix(interactPair)) {
            interactPair <- as.data.frame(interactPair)
            message("'interactPair' converted as data.frame")
        }
        
        # Rename columns
        if (ncol(interactPair) == 3) {
            colnames(interactPair) <- c("idTo", "idFrom", "strength")
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
        if (any(levels(interactPair$idFrom) %in% levels(idObs$idSp)) && any(levels(interactPair$idFrom) %in% 
            levels(idObs$idInd))) {
            stop("'idFrom' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")
        }
        
        if (any(levels(interactPair$idTo) %in% levels(idObs$idSp)) && any(levels(interactPair$idTo) %in% 
            levels(idObs$idInd))) {
            stop("'idTo' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")
        }
        
        ## WHERE interactPair are Species Check if all ids exists in idObs
        if (any(c(levels(interactPair$idFrom), levels(interactPair$idTo)) %in% levels(idObs$idSp))) {
            
            if (!all(levels(interactPair$idFrom) %in% levels(idObs$idSp))) {
                stop(cat("Some species ids in 'idFrom' are not in 'idObs': \n", levels(interactPair$idFrom)[!which(levels(interactPair$idFrom) %in% 
                  levels(idObs$idSp))]))
            }
            
            if (!all(levels(interactPair$idTo) %in% levels(idObs$idSp))) {
                stop(cat("Some species ids in 'idTo' are not in 'idObs': \n", levels(interactPair$idFrom)[!which(levels(interactPair$idFrom) %in% 
                  levels(idObs$idSp))]))
            }
            
        }
        
        ## WHERE interactPair are individu Check if all ids exists in idObs
        if (any(c(levels(interactPair$idFrom), levels(interactPair$idTo)) %in% levels(idObs$idInd))) {
            
            if (!all(levels(interactPair$idFrom) %in% levels(idObs$idInd))) {
                stop(cat("Some individus ids in 'idFrom' are not in 'idObs': \n", 
                  levels(interactPair$idFrom)[!which(levels(interactPair$idFrom) %in% 
                    levels(idObs$idInd))]))
            }
            
            if (!all(levels(interactPair$idTo) %in% levels(idObs$idInd))) {
                stop(cat("Some individus ids in 'idTo' are not in 'idObs': \n", levels(interactPair$idFrom)[!which(levels(interactPair$idFrom) %in% 
                  levels(idObs$idInd))]))
            }
            
        }
        
        # Check if rows are not duplicated
        if (nrow(interactPair[duplicated(interactPair[, c("idFrom", "idTo")]), ]) != 
            0) {
            stop(cat("Some 'idFrom' and 'idTo' are duplicated:\n", interactPair[duplicated(interactPair[, 
                c("idFrom", "idTo")]), ]))
        }
        
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
        if (!is.data.frame(traitInd) && !is.matrix(traitInd) && ncol(traitInd) <= 
            2) {
            stop("'traitInd' has to be a matrix/dataframe with 3 columns")
        }
        
        
        ### Make sure interactPair is a data.frame
        if (is.matrix(traitInd)) {
            traitInd <- as.data.frame(traitInd)
            message("'traitInd' converted as data.frame")
        }
        
        # Rename columns
        if (ncol(interactPair) <= 2) {
            colnames(traitInd)[1] <- "idInd"
            colnames(traitInd)[2:ncol(traitInd)] <- paste("trait", 1:(ncol(traitInd) - 
                1), sep = "")
            message("columns in 'traitInd' have been rename to: 'idInd', 'trait1', ...,'traitn' ")
        }
        
        ### Make sure the first column is a factor (all other columns are free form)
        if (!is.factor(traitInd$idInd)) {
            traitInd$idInd <- as.factor(traitInd$idInd)
        }
        
        ### Make sure 'idInd' levels are referenced into idObs
        if (!all(levels(traitInd$idInd) %in% levels(idObs$idInd))) {
            stop(cat("Some individu ids are not referenced in 'idObs': \n", levels(traitInd$idInd)[!which(levels(traitInd$idInd) %in% 
                levels(idObs$idInd))]))
        }
        
    }
    
    # OBJECT: traitSp ================================================
    
    if (!is.null(traitSp)) {
        
        ### Check class
        
        ### Check for number of columns
        if (!is.data.frame(traitSp) && !is.matrix(traitSp) && ncol(traitSp) <= 2) {
            stop("'traitSp' has to be a matrix/dataframe with 3 columns")
        }
        
        
        ### Make sure traitSp is a data.frame
        if (is.matrix(traitSp)) {
            traitSp <- as.data.frame(traitSp)
            message("'traitSp' converted as data.frame")
        }
        
        # Rename columns
        if (ncol(interactPair) <= 2) {
            colnames(traitSp)[1] <- "idSp"
            colnames(traitSp)[2:ncol(traitSp)] <- paste("trait", 1:(ncol(traitSp) - 
                1), sep = "")
            message("columns in 'traitSp' have been rename to: 'idSp', 'trait1', ...,'traitn' ")
        }
        
        ### Make sure the first column is a factor (all other columns are free form)
        if (!is.factor(traitSp$idSp)) {
            traitSp$idSp <- as.factor(traitSp$idSp)
        }
        
        ### Make sure 'idSp' levels are referenced into idObs
        if (!all(levels(traitSp$idSp) %in% levels(idObs$idSp))) {
            stop(cat("Some species ids are not referenced in 'idObs': \n", levels(traitSp$idSp)[!which(levels(traitSp$idSp) %in% 
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
        
        
    } else {
        # Generate coOcc If coAbund is available, coOcc has to be built from it, else
        # buid coOcc from interactSp.
        if (!is.null(coAbund)) {
            coOccFrom <- "coAbund"
            coOcc <- ifelse(coAbund > 0, 1, 0)
        } else {
            coOccFrom <- "interactSp"
            coOcc <- ifelse(interactSp > 0, 1, 0)
        }
        
    }
    
    
    # OBJECT: siteEnv ================================================
    
    
    if (!is.null(siteEnv)) {
        ### Check for number of columns
        if (!is.data.frame(siteEnv) && !is.matrix(siteEnv) && ncol(siteEnv) <= 2) {
            stop("'siteEnv' has to be a matrix/dataframe with at least 2 columns")
        }
        
        ### Make sure traitSp is a data.frame
        if (is.matrix(siteEnv)) {
            siteEnv <- as.data.frame(siteEnv)
            message("'traitSp' converted as data.frame")
        }
        
        # Rename columns
        if (ncol(siteEnv) <= 2) {
            colnames(siteEnv)[1] <- "idSite"
            message("First column in 'siteEnv' have been rename to: 'idSite'")
        }
        
        ### Make sure the first column is a factor (all other columns are free form)
        if (!is.factor(siteEnv$idSite)) {
            siteEnv$idSite <- as.factor(siteEnv$idSite)
        }
        
        ### Make sure 'idSite' levels are referenced into idObs
        if (!all(levels(siteEnv$idSite) %in% levels(idObs$idSite))) {
            stop(cat("Some site ids are not referenced in 'idObs': \n", levels(siteEnv$idSite)[!which(levels(siteEnv$idSite) %in% 
                levels(idObs$idSite))]))
        }
        
    }
    
    # TRANSFORM: SCALE AND INTERCEPT OPTIONS
    # ================================================
    
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
    
    #### Check classes for coOcc, coAbund, interactSp, interactInd
    
    if (!is.null(interactSp)) {
        if (!is.matrix(interactSp)) {
            interactSp <- as.matrix(interactSp)
            message("'coOcc' was converted to a matrix")
        }
    }
    
    if (!is.null(interactInd)) {
        if (!is.matrix(interactInd)) {
            interactInd <- as.matrix(interactInd)
            message("'coOcc' was converted to a matrix")
        }
    }
    
    if (!is.null(coOcc)) {
        if (!is.matrix(coOcc)) {
            coOcc <- as.matrix(coOcc)
            message("'coOcc' was converted to a matrix")
        }
    }
    
    if (!is.null(coAbund)) {
        if (!is.matrix(coAbund)) {
            coAbund <- as.matrix(coAbund)
            message("'coAbund' was converted to a matrix")
        }
    }
    
    # ================== Return results ==================
    
    
    ## Create res list with NULL
    res <- list(idObs = idObs, interactSp = NULL, interactInd = NULL, coOcc = NULL, 
        coAbund = NULL, siteEnv = NULL, traitSp = NULL, traitInd = NULL, phylo = NULL)
    
    
    attr(res, "coOccFrom") <- coOccFrom
    attr(res, "scaleSiteEnv") <- scaleSiteEnv
    attr(res, "scaleTrait") <- scaleTrait
    attr(res, "interceptSiteEnv") <- interceptSiteEnv
    attr(res, "interceptTrait") <- interceptTrait
    
    ## Fill the list with existing object
    for (obj in exist_args) res[obj] <- get(obj)
    
    class(res) <- "alienData"
    return(res)
}
