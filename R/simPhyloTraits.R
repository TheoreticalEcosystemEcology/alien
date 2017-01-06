#' Traits generator based on evolutionary models.
#'
#' This function simulates traits values according different evolutionary models.
#'
#' @param nsps integer giving the number of species to be returned.
#' @param ntraits integer giving the number of traits to be simulated.
#' @param model a characer string giving the evolutionary model used to simulated traits.
#' @param param parameter values of the model to be used (see \link[geiger]{rescale}).
#'
#' @return A species-by-traits matrix.
#'
#' @author
#' Ignacio Morales Castilla
#' Kevin Cazelles
#'
#' @export
#'
#' @examples
#' mattr <- simPhyloTraits(100, 3, 'lambda', 0)
simPhyloTraits <- function(nsps, ntraits = 1, model = c("lambda", "delta", "OU"), 
    param) {
    tree <- geiger::sim.bdtree(0.5, n = nsps)
    mat <- matrix(0, nsps, ntraits)
    for (i in 1:ntraits) {
        mat[, i] <- scale(ape::rTraitCont(geiger::rescale(tree, model, param)))[, 
            1L]
    }
    return(mat)
}
