#' @title Compute the adjacency matrix for a given alienData object
#'
#' @description Computes the adjacency matrix based on
#' the \code{edge} par of an alienData object.
#'
#' @param object An object of class \code{alienData}.
#' @param bipartite Logical. Whether the adjacency matrix should be constructed for a associated with a
#' bipartite network? Default is \code{FALSE}.
#' @param binary Logical. Should the interactions be binary (see details)?
#' Otherwise the \code{value} column of \code{edge} is used. Default is \code{FALSE}.
#'
#' @details
#' By default \code{getAdjacencyMatrix} creates a square matrix including all
#' nodes found in the \code{node} data frame of \code{object}. Then, it reads
#' \code{edge} to fill the matrix out. If bipartite is \code{TRUE} then only the
#' nodes found in the \code{from} column of \code{edge} are used to name
#' the rows of the adjacency matrix. Similarly, the \code{to} nodes become the
#' columns names.
#' 
#' Currently if there are several values for the same interaction
#' \code{getAdjacencyMatrix} sums them unless \code{binary} is \code{TRUE}.
#'
#' @return
#' An adjacency matrix of class \code{matrix}.
#'
#' @author Kevin Cazelles, F. Guillaume Blanchet
#'
#' @importFrom magrittr %>%
#'
#' @keywords adjacency matrix
#' @export

getAdjacencyMatrix <- function(object, bipartite = FALSE,
                               binary = FALSE) {
  
  stopifnot(class(object) == "alienData")
    
  # Basic objects
  nNode <- nrow(object$node)
  
  if (!bipartite) {
      out <- matrix(0, nrow = nNode, ncol = nNode,
                    dimnames = list(object$node$idInd,
                                    object$node$idInd))
  } else {
      ## "to" as rows
      uit <- unique(object$edge$to) %>% sort
      ## "from" as columns
      uif <- unique(object$edge$from) %>% sort
      
      out <- matrix(0, length(uit), length(uif),
                    dimnames = list(uit, uif))
  }
  
  for (i in 1:nrow(object$edge)) {
      out[object$edge[i, 2L],
          object$edge[i, 1L]] <- out[object$edge[i, 2L],
                                     object$edge[i, 1L]] +
                                 object$edge[i, 3L]
  }
  
  if (binary) {
    out <- ifelse(out < 1, 0, 1)
  }
  
  return(out)
}
