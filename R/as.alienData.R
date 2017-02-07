#' @title Format data for the \code{alien} class
#'
#' @description This functions is used to format the data
#'
#' @param idObs A data.frame which is mandatory and will help to check consistency and prevent errors among unique identifiers of each alienData arguments. The first column (idSite) contains unique identifier of where the observation was made. The second column (idTime) is not mandatory and contains temporal information: an unique identifier at the time the sample has been taken (needed for timeseries analysis). The third column (idSpecies) is an unique identifier of the species sampled at time (idTime) and location (idSite). The fourth column is an unique identifier of individu of species (idSp) observed at time (idTime) and location (idSite).
#' @param interactPair A data.frame with unique identifier of species name (consistent with idSp in idObs data.frame) in the two first columns called idSpeFrom and idSpTo, the strength of the interaction in the third column (see details) and location (in space and/or time) where the species was found in the following columns where species were found (see details).
#' @param coOcc A square symmetric matrix of 0s and 1s that define co-occurence patterns among pairs of species. If this matrix is not provided some methods could build it base on interactPair data.frame.
#' @param coAbund A square symmetric matrix that includes any types of values, defining co-abundance patterns among pairs of species.
#' @param siteEnv A matrix or a data.frame where each column is a descriptor of the sites.
#' @param traitSp A matrix or a data.frame where each column is a trait characterizing all species.
#' @param traitInd A matrix or a data.frame where each column is a trait characterizing an individual.
#' @param phylo A square symmetric matrix describing the phylogenetic relationships between pairs of all species (see details).
#' @param scaleSiteEnv Logical. Whether the columns of X should be centred and divided by the standard deviation. Default is TRUE.
#' @param scaleTrait Logical. Whether the rows of Tr should be centred and divided by the standard deviation. Default is TRUE.
#' @param interceptSiteEnv Logical. Whether a column of 1s should be added to X. Default is TRUE.
#' @param interceptTrait Logical. Whether a row of 1s should be added to Tr. Default is TRUE.
#'
#' @details
#'
#' The strength of the interactions defined in the third column of \code{interactPair} can be an NA or any numerical value.
#' In addition, species location (sites), will be converted to a factor. Note that there can be multiple sets of locations that can be considered, each one defined by a different factor.
#'
#' @return
#'
#' An object of the class \code{alienData} is returned by \code{as.alienData}.
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
    siteEnv = NULL, traitSp = NULL, traitInd = NULL, phylo = NULL, scaleSiteEnv = TRUE, scaleTrait = TRUE, interceptSiteEnv = TRUE,
    interceptTrait = TRUE) {

    #===== Test which args exists

    args <- c("idObs","coOcc", "coAbund", "siteEnv", "traitSp", "traitInd",
        "phylo", "location")

    exist_args <- sapply(args, exists)
    exist_args <- names(exist_args[exist_args == TRUE])

    # ====== Test Mandatory field

    if(!("idObs" %in% exist_args)){
        stop("idObs argument cannot be NULL")
    }

    # Test idObs structure
    if(!is.data.frame(idObs) && !is.matrix(idObs) && ncol(idObs) != 4){
      stop("idObs has to be a matrix/dataframe with 3 columns: idSite, idVisit, idSp, idInd")
    }

    if(is.matrix(idObs)){
      idObs <- as.data.frame(idObs)
      print("'idObs' converted as data.frame")
    }

    # Check for column names
    if (is.null(colnames(idObs))) {
        colnames(idObs)[1] <- "idSite"
        colnames(idObs)[2] <- "idTime"
        colnames(idObs)[3] <- "idSp"
        colnames(idObs)[4] <- "idInd"
        print("column names were added to 'idObs'")
    }

    # Check for duplicates rows
    if(nrow(idObs[duplicated(idObs),]) != 0){
      stop(cat("some idObs entries are duplicated: \n",idObs[duplicated(idObs),]))
    }

    # Cast all columns has factors
    if (!all(sapply(idObs, class)[1:3] == "factor")) {
        idObs <- apply(idObs,2,as.factor)
    }

    # ================== Start Checking ==================

    if (!is.null(interactPair)) {
        ### Check for number of columns
        if (ncol(interactPair) != 3) {
            stop("'interactPair' needs to have three columns: idInd1, idInd2, strength or idSp1, idSp2, strength")
        }

        ## Check consistency among idSp1, idSp2, idSite with idObs table
        if()

        ### Check for row names
        if (is.null(rownames(interactPair))) {
            rownames(interactPair) <- paste("pair", 1:ncol(interactPair), sep = "")
            print("row names names were added to 'interactPair'")
        }

        ### Check for column names
        if (is.null(colnames(interactPair))) {
            colnames(interactPair)[1] <- "idSite"
            colnames(interactPair)[2] <- "idSp1"
            colnames(interactPair)[3] <- "idSp2"
            colnames(interactPair)[4] <- "strength"
            print("column names were added to 'interactPair'")
        }

        ### Check class

        ### Make sure indSp is a data.frame
        if (!is.data.frame(interactPair)) {
            interactPair <- as.data.frame(interactPair)
        }

        ### Make sure the first, second, and fourth column are factor
        if (!all(sapply(interactPair, class)[c(1, 2, 4:ncol(interactPair))] == "factor")) {
            interactPair <- as.data.frame(lapply(interactPair, as.factor))
        }

        ### Make sure the third column is numeric
        if (!is.numeric(interactPair[, 3])) {
            interactPair[, 3] <- as.numeric(interactPair[, 3])
        }
    }

    # ============================== Individual traits long data

    if (!is.null(traitInd)) {
        ### Check for number of columns
        if (ncol(traitInd) <= 2) {
            stop("'traitInd' needs to have at least two columns")
        }
        ### Check for row names
        if (is.null(rownames(traitInd))) {
            rownames(traitInd) <- paste("Ind", 1:ncol(traitInd), sep = "")
            print("row names names were added to 'traitInd'")
        }

        ### Check for column names
        if (is.null(column(traitInd))) {
            colnames(traitInd)[1] <- "sp"
            colnames(traitInd)[2:ncol(traitInd)] <- paste("trait", 1:(ncol(traitInd) -
                1), sep = "")
            print("column names were added to 'traitInd'")
        }

        ### Check class

        ### Make sure traitInd is a data.frame
        if (!is.data.frame(traitInd)) {
            traitInd <- as.data.frame(traitInd)
        }

        ### Make sure the first column is a factor (all other columns are free form)
        if (!is.factor(traitInd[, 1])) {
            traitInd[, 1] <- as.factor(traitInd[, 1])
        }
    }

    # ========== Convert ==========
    nsp <- nlevels(interactPair[, 1]) + nlevels(interactPair[, 2])
    coAbund <- matrix(NA, nrow = nsp, ncol = nsp)
    colnames(coAbund) <- c(levels(interactPair[, 1]), levels(interactPair[, 2]))
    rownames(coAbund) <- c(levels(interactPair[, 1]), levels(interactPair[, 2]))

    for (i in 1:nrow(interactPair)) {
        coAbund[interactPair[i, 1], interactPair[i, 2]] <- interactPair[i, 3]
        coAbund[interactPair[i, 2], interactPair[i, 1]] <- interactPair[i, 3]
    }

    # =============================== Check all other matrix types
    # =============================== resCon is a special case that needs to be
    # checked independently

    if (!is.null(resCon)) {
        ### Check format
        if (length(dim(resCon)) != 2) {
            stop("'resCon' should be a table")
        }

        ### Row names
        if (is.null(rownames(resCon))) {
            rownames(resCon) <- paste("Resource", 1:ncol(resCon), sep = "")
            print("rownames names were added to 'resCon'")
        }

        ### Column names
        if (is.null(colnames(resCon))) {
            colnames(resCon) <- paste("Consumer", 1:ncol(resCon), sep = "")
            print("column names were added to 'resCon'")
        }

        ### Build interact matrix
        matSize <- nrow(resCon) + ncol(resCon)
        interact <- matrix(NA, nrow = matSize, ncol = matSize)
        interact[1:nrow(resCon), 1:ncol(resCon)] <- resCon
        interact[(nrow(resCon) + 1):(ncol(resCon) + nrow(resCon)), (ncol(resCon) +
            1):(nrow(resCon) + ncol(resCon))] <- t(resCon)
    }

    #### Check format
    if (!is.null(coOcc)) {
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
    }

    if (!is.null(coAbund)) {
        if (nrow(coAbund) != ncol(coAbund)) {
            stop("'coAbund' should be a square table")
        }

        ### Check if symmetry
        if (!isSymmetric(coAbund)) {
            stop("'coAbund' need to be a symmetric matrix")
        }

        ### Check positive definiteness
        if (!all(coAbund) >= 0) {
            stop("All values in 'coAbund' should be larger or equal to 0")
        }
    }

    if (!is.null(siteEnv)) {
        if (length(dim(siteEnv)) != 2) {
            stop("'siteEnv' should be a table")
        }
    }

    if (!is.null(traitSp)) {
        if (length(dim(traitSp)) != 2) {
            stop("'traitSp' should be a table")
        }
    }

    if (!is.null(phylo)) {
        if (nrow(phylo) != ncol(phylo)) {
            stop("'phylo' should be a square table")
        }

        ### Check if symmetry
        if (!isSymmetric(phylo)) {
            stop("'phylo' need to be a symmetric matrix")
        }

        ### Check positive definiteness
        if (any(eigen(phylo)$value < 0)) {
            stop("'phylo' need to be a positive definite matrix")
        }

        ### Check if phylo is positive definite
        if (!all(eigen(phylo)$value > 0)) {
            stop("'phylo' need to positive definite (i.e. all eigenvalues need to be positive)")
        }
    }

    #### Check column names

    if (!is.null(coOcc)) {
        if (is.null(colnames(coOcc))) {
            colnames(coOcc) <- paste("sp", 1:ncol(coOcc), sep = "")
            print("column names were added to 'coOcc'")
        }
    }

    if (!is.null(coAbund)) {
        if (is.null(colnames(coAbund))) {
            colnames(coAbund) <- paste("sp", 1:ncol(coAbund), sep = "")
            print("column names were added to 'coAbund'")
        }
    }

    if (!is.null(siteEnv)) {
        if (is.null(colnames(siteEnv))) {
            colnames(siteEnv) <- paste("env", 1:ncol(siteEnv), sep = "")
            print("column names were added to 'siteEnv'")
        }
    }

    if (!is.null(traitSp)) {
        if (is.null(colnames(traitSp))) {
            colnames(traitSp) <- paste("traitSp", 1:ncol(traitSp), sep = "")
            print("column names were added to 'traitSp'")
        }
    }

    if (!is.null(phylo)) {
        if (is.null(colnames(phylo))) {
            colnames(phylo) <- paste("sp", 1:ncol(phylo), sep = "")
            print("column names were added to 'phylo'")
        }
    }

    if (!is.null(colnames(location))) {
        colnames(location) <- paste("location", 1:ncol(location), sep = "")
        print("column names were added to 'location'")
    }

    #### Check row names
    if (!is.null(coOcc)) {
        if (is.null(rownames(coOcc))) {
            rownames(coOcc) <- paste("sp", 1:ncol(coOcc), sep = "")
            print("row names were added to 'coOcc'")
        }
    }

    if (!is.null(coAbund)) {
        if (is.null(rownames(coAbund))) {
            rownames(coAbund) <- paste("sp", 1:ncol(coAbund), sep = "")
            print("row names were added to 'coAbund'")
        }
    }

    if (!is.null(siteEnv)) {
        if (is.null(rownames(siteEnv))) {
            rownames(siteEnv) <- paste("site", 1:nrow(siteEnv), sep = "")
            print("row names were added to 'siteEnv'")
        }
    }

    if (!is.null(traitSp)) {
        if (is.null(rownames(traitSp))) {
            rownames(traitSp) <- paste("sp", 1:nrow(traitSp), sep = "")
            print("row names were added to 'traitSp'")
        }
    }

    if (!is.null(phylo)) {
        if (is.null(rownames(phylo))) {
            rownames(phylo) <- paste("sp", 1:ncol(phylo), sep = "")
            print("row names were added to 'phylo'")
        }
    }

    if (!is.null(location)) {
        rownames(location) <- paste("site", 1:nrow(location), sep = "")
    }

    ### If coAbund is available, coOcc can be constructed
    if (!is.null(coAbund)) {
        coOcc <- ifelse(coAbund > 0, 1, 0)
    }

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

    #### Check classes

    if (!is.null(coOcc)) {
        if (!is.matrix(coOcc)) {
            coOcc <- as.matrix(coOcc)
            print("'coOcc' was converted to a matrix")
        }
    }

    if (!is.null(coAbund)) {
        if (!is.matrix(coAbund)) {
            coAbund <- as.matrix(coAbund)
            print("'coAbund' was converted to a matrix")
        }
    }

    if (!is.null(siteEnv)) {
        if (!is.matrix(siteEnv)) {
            siteEnv <- as.matrix(siteEnv)
            print("'siteEnv' was converted to a matrix")
        }
    }

    if (!is.null(traitSp)) {
        if (!is.matrix(traitSp)) {
            traitSp <- as.matrix(traitSp)
            print("'traitSp' was converted to a matrix")
        }
    }

    if (!is.null(phylo)) {
        if (!is.matrix(phylo)) {
            phylo <- as.matrix(phylo)
            print("'phylo' was converted to a matrix")
        }
    }

    # ================== Return results ==================


    ## Create res list with NULL
    res <- list(idObs = idObs, coOcc = NULL, coAbund = NULL, siteEnv = NULL,
        traitSp = NULL, traitInd = NULL, phylo = NULL)

    ## Fill the list with existing object
    for (obj in exist_args) res[obj] <- get(obj)

    class(res) <- "alienData"
    return(res)
}
