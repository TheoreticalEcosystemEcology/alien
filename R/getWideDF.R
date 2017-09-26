#' @name getWideDF
#'
#' @title Create a dataframe of the wide format.
#'
#' @description Estimation of interaction probabilities using the matching centrality approach as described in Rohr (2014) and Rohr (2016).
#'
#' @param data an object of the class alienData or an adjacency matrix.
#' @param d dimensionnality.
#' @param mxt Numeric. Maximum running time in seconds.
#' @param verbose Logical. Should extra information be reported on progress?
#'
#' @author
#' Kevin Cazelles
#'
#' @details
#' \code{fitMC} implements the matching-centrality method as described in
#' Rohr 2014 and Rohr 2016. Briefly, the method uses latent traits to fit
#' interactions. The first step requires the definition of two sets of interacting
#' species: set1 and set2 (respectively of size \code{nset1} and \code{nset2}).
#' The first category of latent traits are the matching traits. (to be developped)
#' The second category include the centrality terms. (to be developped).
#' Latents traits are built under certain topological constraints:
#' 1- all vectors belong to the orthogonal basis of the unit vector.
#' 2- all matching vectors of a particluar set of species are orthogonal to each other.
#'
#'
#' @return
#' An object of class \code{alienPredict}.
#'
#' @examples
#' load_all()
#' set.seed(1987)
