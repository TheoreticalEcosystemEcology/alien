#' @title Aggregate alien object.
#'
#' @description Aggregate the interactions of an \code{alienData} object
#' according to a a given aggregateCol of \code{dfNodes}.
#'
#' @param object An object of class \code{alienData}.
#' @param aggregateCol A character string or integer identifying the column of \code{dfNodes} to be used to aggregate interactions.
#' @param keepCols Vector of strings or integer identifying the columns to be
#' kept along with \code{aggregateCol}.
#' @param idSuffix Suffix of the new identifiers.
#'
#'
#' @return
#' An object of the class \code{alienPredict} with the desired new level of interactions.
#'
#' @author Kevin Cazelles
#'
#' @keywords manip
#'
#' @export
#'
#' @examples
#' data(plantsPol)
#' plantsPolSp <- alienAggregate(plantsPol, 'idSp', 'type')
#' # plantsPol2 <- plantsPol
#' # plantsPol2$dfNodes$var1 <- runif(nrow(plantsPol2$dfNodes))
#' # plantsPolSp <- alienAggregate(plantsPol2, 'idSp', 'var1')


alienAggregate <- function(object, aggregateCol, keepCols = NULL, idSuffix = "id_") {
    
    stopifnot(class(object) == "alienData")
    stopifnot(length(aggregateCol) == 1)
    
    slc <- as.factor(object$dfNodes[, aggregateCol])
    nms <- names(object$dfNodes[, aggregateCol, drop = F])
    if (!is.null(keepCols)) {
        nms2 <- names(object$dfNodes[, keepCols, drop = F])
    } else nms2 <- NULL
    
    ## check whether the aggregation would change the number of interactions
    stopifnot(nlevels(slc) < nrow(object$dfNodes))
    ## 
    tmp <- object
    idnew <- paste0(idSuffix, as.numeric(slc))
    ## 
    tmp$dfEdges$idFrom <- idnew[unlist(lapply(object$dfEdges$idFrom, function(x) which(object$dfNodes$idNodes == 
        x)))]
    tmp$dfEdges$idTo <- idnew[unlist(lapply(object$dfEdges$idTo, function(x) which(object$dfNodes$idNodes == 
        x)))]
    ## 
    tmp$dfEdges <- unique(tmp$dfEdges[, names(tmp$dfEdges) %in% c("idFrom", "idTo", 
        "idSite")])
    ##--
    if (!is.null(tmp$dfOcc)) {
        tmp$dfOcc$idNodes <- idnew[unlist(lapply(object$dfOcc$idNodes, function(x) which(object$dfNodes$idNodes == 
            x)))]
        tmp$dfOcc <- unique(tmp$dfOcc[, names(tmp$dfOcc) %in% c("idNodes", "idSite")])
    }
    #-- need to think about what to do with the rest of the aggregateCols...
    tmp$dfNodes$idNodes <- idnew
    tmp$dfNodes <- unique(tmp$dfNodes[, unique(c("idNodes", nms, nms2))])
    if (nrow(tmp$dfNodes) != length(unique(idnew))) 
        stop("when building the new alienData object, duplicate has been found in dfNodes$idNodes, consider checking keepCols")
    #--
    out <- alienData(dfNodes = tmp$dfNodes, dfEdges = tmp$dfEdges, dfSites = tmp$dfSites, 
        dfOcc = tmp$dfOcc, verbose = F)
    out
    # tmp
}
