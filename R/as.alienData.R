#' @title Format data for the \code{alien} class
#'
#' @description This functions is used to format the data
#'
#' @param idObs A data.frame which is mandatory and will help to check consistency and prevent errors among unique identifiers of each alienData arguments. The first column (idSite) contains unique identifier of where the observation was made. The second column (idTime) is not mandatory and contains temporal information: an unique identifier at the time the sample has been taken (needed for timeseries analysis). The third column (idSpecies) is an unique identifier of the species sampled at time (idTime) and location (idSite). The fourth column is an unique identifier of individu of species (idSp) observed at time (idTime) and location (idSite).
#' @param interactPair A data.frame with the first two columns are idFrom and idTo, be aware that these columns determined the sens of the interaction TODO: Explains that interactPair contains interaction at the finest level (individus or species). Finaly, the thrid column is the strength of the interaction (Please see details).
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
#' The strength of the interactions defined in the third column of \code{interactPair} can be a 0 if no direct interaction has been observed (defined as true absence of interaction) or any numerical value. Undocumented interactions among species or individus will be assumed as NA by default.
#'
#' @return
#'
#' An object of the class \code{alienData} is returned by \code{as.alienData}.
#' TODO: Declare more accurately the structure of object returned by the function.
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

    # OBJECT: idObs ================================================

    if(!("idObs" %in% exist_args)){
        stop("idObs argument cannot be NULL")
    }

    # Test idObs structure
    if(!is.data.frame(idObs) && !is.matrix(idObs) && ncol(idObs) != 4){
      stop("idObs has to be a matrix/dataframe with 4 columns")
    }

    if(is.matrix(idObs)){
      idObs <- as.data.frame(idObs)
      print("'idObs' converted as data.frame")
    }

    # Rename columns
    if(ncol(idObs)==4) {
      colnames(idObs) <- c("idSite","idTime","idSp","idInd")
      print("'idObs' columns have been rename to 'idSite','idTime','idSp','idInd'")
    }

    # Check for duplicates rows
    if(nrow(idObs[duplicated(idObs),]) != 0){
      stop(cat("some idObs entries are duplicated: \n",idObs[duplicated(idObs),]))
    }

    # Cast all columns has factors
    if (!all(sapply(idObs, class)[1:4] == "factor")) {
        idObs <- as.data.frame(lapply(idObs, as.factor))
    }

    # OBJECT: interactPair ================================================

    if (!is.null(interactPair)) {
        ### Check for number of columns
        if (!is.data.frame(interactPair) && !is.matrix(interactPair) && ncol(interactPair) != 3) {
            stop("'interactPair' has to be a matrix/dataframe with 3 columns")
        }

        ### Check class

        ### Make sure indSp is a data.frame
        if (!is.data.frame(interactPair)) {
            interactPair <- as.data.frame(interactPair)
        }

        # Rename columns
        if(ncol(interactPair)==3) {
          colnames(interactPair) <- c("idTo","idFrom","strength")
          print("'interactPair' columns have been rename to 'idTo','idFrom','strength' ")
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

        ## Check if idFrom and idTo are in levels(idSpe) or levels(idInd) and not both
        ## interactPair are observations at species level OR at individual level but not both
        if(any(levels(interactPair$idFrom) %in% levels(idObs$idSp))
        && any(levels(interactPair$idFrom) %in% levels(idObs$idInd))){
          stop("'idFrom' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")
        }

        if(any(levels(interactPair$idTo) %in% levels(idObs$idSp))
        && any(levels(interactPair$idTo) %in% levels(idObs$idInd))){
          stop("'idTo' values belongs to 'idSp' and 'idInd' in 'idObs'. Interaction can't be at the species AND individual levels")
        }

        ## WHERE interactPair are Species
        # Check if all ids exists in idObs
        if(any(c(levels(interactPair$idFrom),levels(interactPair$idTo)) %in% levels(idObs$idSp))){

          if(!all(levels(interactPair$idFrom) %in% levels(idObs$idSp))){
            stop(cat("Some species ids in 'idFrom' are not in 'idObs': \n", levels(interactPair$idFrom)[!which(levels(interactPair$idFrom) %in% levels(idObs$idSp))]))
          }

          if(!all(levels(interactPair$idTo) %in% levels(idObs$idSp))){
            stop(cat("Some species ids in 'idTo' are not in 'idObs': \n", levels(interactPair$idFrom)[!which(levels(interactPair$idFrom) %in% levels(idObs$idSp))]))
          }

        }

        ## WHERE interactPair are individu
        # Check if all ids exists in idObs
        if(any(c(levels(interactPair$idFrom),levels(interactPair$idTo)) %in% levels(idObs$idInd))){

          if(!all(levels(interactPair$idFrom) %in% levels(idObs$idInd))){
            stop(cat("Some individus ids in 'idFrom' are not in 'idObs': \n", levels(interactPair$idFrom)[!which(levels(interactPair$idFrom) %in% levels(idObs$idInd))]))
          }

          if(!all(levels(interactPair$idTo) %in% levels(idObs$idInd))){
            stop(cat("Some individus ids in 'idTo' are not in 'idObs': \n", levels(interactPair$idFrom)[!which(levels(interactPair$idFrom) %in% levels(idObs$idInd))]))
          }

        }

        # Check if rows are not duplicated
        if(nrow(interactPair[duplicated(interactPair[,c('idFrom','idTo')]),]) != 0){
          stop(cat("Some 'idFrom' and 'idTo' are duplicated:\n",
          interactPair[duplicated(interactPair[,c('idFrom','idTo')]),]))
        }

    }

    # OBJECT: interactSp and interacInd ================================================
    # Turn interactPair into interacSp and interacInd MATRICES

    # if interactPair at individus level
    if(all(c(levels(interactPair$idTo),levels(interactPair$idFrom)) %in% levels(idObs$idInd))){

      ## built interacInd
      nsp <- nlevels(interactPair$idTo) + nlevels(interactPair$idFrom)
      interacInd <- matrix(NA, nrow = nsp, ncol = nsp)
      colnames(interacInd) <- c(levels(interactPair$idTo), levels(interactPair$idFrom))
      rownames(interacInd) <- c(levels(interactPair$idTo), levels(interactPair$idFrom))


      for (i in 1:nrow(interactPair)) {
          interacInd[interactPair[i, 'idFrom'], interactPair[i, 'idTo']] <- interactPair[i, 'strength']
      }

      ## Retrieve Sp ids from idObs
      idFromSp <- merge(interactPair,idObs,by.x="idFrom",by.y="idInd")[,'idSp']
      idToSp <- merge(interactPair,idObs,by.x="idTo",by.y="idInd")[,'idSp']

      # Create interactPair with idSp
      interactPairSp <- interactPair
      interactPairSp$idFrom <- idFromSp
      interactPairSp$idTo <- idToSp

      ## Aggregate
      ### TODO: WARNING - If the strength is not a count. The sum might not be appropriate.
      interactPairSp <- aggregate(strength ~ idFrom + idTo,interactPairSp, FUN=sum)

      ## built interacSp
      nsp <- nlevels(interactPairSp$idTo) + nlevels(interactPairSp$idFrom)
      interacSp <- matrix(NA, nrow = nsp, ncol = nsp)
      colnames(interacSp) <- c(levels(interactPairSp$idTo), levels(interactPairSp$idFrom))
      rownames(interacSp) <- c(levels(interactPairSp$idTo), levels(interactPairSp$idFrom))


      for (i in 1:nrow(interactPairSp)) {
          interacSp[interactPairSp[i, 'idFrom'], interactPairSp[i, 'idTo']] <- interactPairSp[i, 'strength']
      }


    } else if(all(c(levels(interactPair$idTo),levels(interactPair$idFrom)) %in% levels(idObs$idSp))){

      # if interactPair at species level
      nsp <- nlevels(interactPair$idTo) + nlevels(interactPair$idFrom)
      interacSp <- matrix(NA, nrow = nsp, ncol = nsp)
      colnames(interacSp) <- c(levels(interactPair$idTo), levels(interactPair$idFrom))
      rownames(interacSp) <- c(levels(interactPair$idTo), levels(interactPair$idFrom))


      for (i in 1:nrow(interactPair)) {
          interacSp[interactPair[i, 'idFrom'], interactPair[i, 'idTo']] <- interactPair[i, 'strength']
      }

      # let interacInd null
      interacInd <- NULL

    }


    # OBJECT: traitInd ================================================

    if (!is.null(traitInd)) {
        ### Check for number of columns
        if (ncol(traitInd) <= 2) {
            stop("'traitInd' needs to have at least two columns")
        }


        ### Check for column names
        if (is.null(column(traitInd))) {
            colnames(traitInd)[1] <- "idInd"
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

    # OBJECT: traitSp ================================================

    if (!is.null(traitSp)) {
        ### Check for number of columns
        if (ncol(traitSp) <= 2) {
            stop("'traitSp' needs to have at least two columns")
        }


        ### Check for column names
        if (is.null(column(traitSp))) {
            colnames(traitSp)[1] <- "idInd"
            colnames(traitSp)[2:ncol(traitSp)] <- paste("trait", 1:(ncol(traitSp) -
                1), sep = "")
            print("column names were added to 'traitSp'")
        }

        ### Check class

        ### Make sure traitSp is a data.frame
        if (!is.data.frame(traitSp)) {
            traitSp <- as.data.frame(traitSp)
        }

        ### Make sure the first column is a factor (all other columns are free form)
        if (!is.factor(traitSp[, 1])) {
            traitSp[, 1] <- as.factor(traitSp[, 1])
        }
    }


    # =============================== Check all other matrix types

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
