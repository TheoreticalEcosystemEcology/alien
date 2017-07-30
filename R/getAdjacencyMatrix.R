#' @title Compute the adjacency matrix for a given alienData object
#'
#' @description \code{getAdjacencyMatrix} computes the adjacency matrix based on
#' the \code{dfEdges} element of an alienData object.
#'
#' @param object An object of class \code{alienData}.
#' @param bipartite A logical. Is the adjacency matrix associated with a
#' bipartite network? Default is set to \code{FALSE}.
#' @param binary A logical. Should the interactions be binary (see details)?
#' Otherwise the \code{value} column of \code{dfEdges} is used. Default is set
#' to \code{FALSE}.
#'
#' @details
#' By default \code{getAdjacencyMatrix} creates a square matrix including all
#' nodes found in the \code{dfNodes} data frame of \code{object}. Then, it reads
#' \code{dfEdges} to fill the matrix out. If bipartite is \code{TRUE} then only the
#' nodes found in the \code{idFrom} column of \code{dfEdges} are used to name
#' the rows of the adjacency matrix. Similarly, \code{idTo}'s nodes become the
#' columns' names.
#'
#' Currenlty if there are several values for the same interaction
#' \code{getAdjacencyMatrix} sums them unless \code{binary} is \code{TRUE}.
#'
#' @return
#' An adjacency matrix of class \code{matrix}.
#'
#' @author Kevin Cazelles
#'
#' @importFrom magrittr %>%
#'
#' @keywords adjacency matrix
#' @export

getAdjacencyMatrix <- function(object, bipartite = FALSE, binary = FALSE) {
    stopifnot(class(object) == "alienData")
    tmp <- object$dfEdges[, c("idFrom", "idTo", "value")]
    if (!bipartite) {
        out <- matrix(0, object$nbNodes, object$nbNodes, dimnames = list(object$dfNodes$idNodes, 
            object$dfNodes$idNodes))
    } else {
        ## idTo as rows
        uit <- unique(tmp$idTo) %>% sort
        ## idFrom as columns
        uif <- unique(tmp$idFrom) %>% sort
        out <- matrix(0, length(uit), length(uif), dimnames = list(uit, uif))
    }
    if (binary) {
        tmp$value <- 1
        tmp <- unique(tmp)
    }
    for (i in 1:nrow(tmp)) {
        out[tmp[i, 2L], tmp[i, 1L]] <- out[tmp[i, 2L], tmp[i, 1L]] + tmp[i, 3L]
    }
    out
}
