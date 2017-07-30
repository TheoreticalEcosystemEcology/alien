#' @title Conversion from various fornat to alienData
#'
#' @description Format alien output and return an object of class \code{alienPredict}.
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

    args <- list(...)
    dfEdges <- getEdgesList(x)
    ##
    if ("dfNodes" %in% names(args)) {
        dfNodes <- args$dfNodes
    } else {
        dfNodes <- data.frame(idNodes = unique(c(dfEdges$idFrom, dfEdges$idTo)))
    }
    ##
    id <- which(names(args) %in% c("dfEdges", "dfNodes"))
    if (length(id))
        args <- args[-id]
    ##
    out <- do.call("alienData", c(list(dfNodes = dfNodes, dfEdges = dfEdges), args))
    out
}

# #' @rdname as.alienData #' @export as.alienData.data.frame <- function(x, ...)
# { args <- list(...)  # dfEdges <- dfNodes out <- as.alienData(...)  ## rm } #'
# @rdname as.alienData #' @export as.alienData.igraph <- function(x, ...) { args
# <- list(...)  # dfEdges <- dfNodes <- out <- as.alienData(...)  out }
