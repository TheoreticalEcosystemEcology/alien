#' @title Convert various formats to an alienData object.
#'
#' @description \code{as.alienData} is a generic function woth method to convert
#' \code{matrix}, \code{data.frame} and \code{igraph} object into a
#' an \code{alienData} object.
#'
#' @param x Input to as.alienData
#' @param ... other arguments to be passed to \code{\link[alien]{alienData}}.
#'
#' @details
#' For objects of class \code{matrix}, see \code{\link[alien]{getEdgesList}}
#' to learn how the edges list is retrieved from the adjacency matrix.
#' The same principles are applied to objects of class \code{igraph} except
#' they are handled using funtions from the \code{igraph} package.
#'
#' @return
#' An object of the class \code{alienPredict}.
#'
#' @author Kevin Cazelles
#'
#' @keywords method
#' @keywords classes
#' @export

as.alienData <- function(x, ...) UseMethod("as.alienData")

#' @rdname as.alienData
#' @export
as.alienData.alienData <- function(x, ...) {
    x
}

#' @rdname as.alienData
#' @export
as.alienData.matrix <- function(x, ...) {
    dfEdges <- getEdgesList(x)
    out <- as.alienData.data.frame(dfEdges, ...)
    # ##-- dfNodes <- args$dfNodes if(is.null(dfNodes)) dfNodes <- data.frame(idNodes
    # = unique(c(dfEdges$idFrom, dfEdges$idTo))) ##-- id <- which(names(args) %in%
    # c('dfEdges', 'dfNodes')) if (length(id)) args <- args[-id] ##-- out <-
    # do.call('alienData', c(list(dfNodes = dfNodes, dfEdges = dfEdges), args))
    out
}

#' @rdname as.alienData
#' @export
as.alienData.data.frame <- function(x, ...) {
    stopifnot(all(c("idFrom", "idTo") %in% names(x)))
    ##--
    args <- list(...)
    ##--
    id <- which(names(x) %in% c("idFrom", "idTo", "idSite", "value"))
    dfEdges <- x[, id]
    ##--
    dfNodes <- args$dfNodes
    if (is.null(dfNodes)) 
        dfNodes <- data.frame(idNodes = unique(c(dfEdges$idFrom, dfEdges$idTo)))
    ##--
    id <- which(names(args) %in% c("dfEdges", "dfNodes"))
    if (length(id)) 
        args <- args[-id]
    ##--
    out <- do.call("alienData", c(list(dfNodes = dfNodes, dfEdges = dfEdges), args))
    out
}
# { args <- list(...)  # dfEdges <- dfNodes out <- as.alienData(...)  ## rm } #'
# @rdname as.alienData #' @export as.alienData.igraph <- function(x, ...) { args
# <- list(...)  # dfEdges <- dfNodes <- out <- as.alienData(...)  out }
