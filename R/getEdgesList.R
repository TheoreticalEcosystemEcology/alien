#' @title Extract the edges list from an adjacency matrix.
#'
#' @description \code{getEdgesList} extracts the edges list from an adjacency matrix.
#'
#' @param x an adjacency matrix.
#' @param bipartite A logical. Is the adjacency matrix associated with a
#' bipartite network? Default is set to \code{TRUE}.
#' @param ignore a vector whose values will be regarded as an absence of interaction.
#'
#' @details
#' Row and column names of the matrix are used to identify the nodes of the edges
#' list. If at least one of these attributes is \code{NULL} then names are
#' assigned as follows: if \code{bipartite} is \code{TRUE} two different sets
#' of names are used, otherwise the same set of names is used. In the last
#' scenario, last names (in alphabetical order) are droped from the smaller set.
#' Note that names of matrix columns are treated as \code{idFrom} nodes and
#' names of matrix rows are treated as \code{idTo} nodes.
#'
#' @return
#' A data frame of three columns \code{idFrom}, \code{idTo} and \code{value}.
#'
#' @author Kevin Cazelles
#'
#' @importFrom magrittr %<>%
#'
#' @keywords adjacency matrix
#' @keywords edges list
#' @export

getEdgesList <- function(x, bipartite = TRUE, ignore = 0) {
    x %<>% as.matrix
    nmt <- rownames(x)
    nmf <- colnames(x)
    nr <- nrow(x)
    nc <- ncol(x)
    if (is.null(nmt) | is.null(nmf)) {
        ##-- Assign names
        if (bipartite) {
            nmf <- paste0("node_", 1:nc)
            nmt <- paste0("node_", (nc + 1):(nr + nc))
        } else {
            nmf <- paste0("node_", 1:nc)
            nmt <- paste0("node_", 1:nr)
        }
    }
    
    ##-- Store edges in a list
    out <- list()
    k <- 0
    for (i in 1:nr) {
        for (j in 1:nc) {
            if (!x[i, j] %in% ignore) {
                k <- k + 1
                out[[k]] <- data.frame(idFrom = nmf[j], idTo = nmt[i], value = as.numeric(x[i, 
                  j]), stringsAsFactors = FALSE)
            }
        }
    }
    
    out %<>% do.call(what = "rbind")
    out
}
