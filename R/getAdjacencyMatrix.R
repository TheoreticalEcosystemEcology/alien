#' @title Compute the adjacency matrix for a given alienData object
#'
#' @description Computes the adjacency matrix based on
#' the `edge` par of an `alienData` object.
#'
#' @param object An object of class `alienData`.
#' @param bipartite Logical. Whether the adjacency matrix should be constructed for a associated with a
#' bipartite network? Default is `FALSE`.
#' @param binary Logical. Should the interactions be binary (see details)?
#' Otherwise the `value` column of `edge` is used. Default is `FALSE`.
#'
#' @details
#' By default `getAdjacencyMatrix` creates a square matrix including all
#' nodes found in the `node` data frame of `object`. Then, it reads
#' `edge` to fill the matrix out. If bipartite is `TRUE` then only the
#' nodes found in the `from` column of `edge` are used to name
#' the rows of the adjacency matrix. Similarly, the `to` nodes become the
#' columns names.
#' 
#' Currently if there are several values for the same interaction
#' `getAdjacencyMatrix` sums them unless `binary` is `TRUE`.
#'
#' @return
#' An adjacency matrix of class `matrix`.
#'
#' @author Kevin Cazelles, F. Guillaume Blanchet
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
      uit <- sort(unique(object$edge$to)) 
      ## "from" as columns
      uif <- sort(unique(object$edge$from))
      ##
      out <- matrix(0, length(uit), length(uif),
                    dimnames = list(uit, uif))
  }
  
  for (i in seq_len(nrow(object$edge))) {
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
