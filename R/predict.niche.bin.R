#' @name predict.niche.bin
#'
#' @title This function use a pair of traits.
#'
#' @description By convention, i is the target layer and j is the opposed layer
#' Data: a data frame with Ti as traits of the layer i and Tj traits of the layer j
#' each row correspond to a pair of species with known interaction
#'
#' @param models The output of fit.niche.bin
#' @param newdata a dataframe with with Ti as traits of the layer i and Tj traits of the layer j
#' each row correspond to a pair of trait for which we want to know if there is an
#' interaction happenning
#'
#' @author
#' Dominique Gravel
#'
#' @references
#'
#' @rdname predict.niche.bin
#' @export
predict.niche.bin = function(models, newdata) {
  # Check if the newdata has the right format

  with(newdata, {
    npairs = length(Ti)

    # Lower boundary
    lo = models$model_lo[1] + models$model_lo[2]*Ti

    # Upper boundary
    up = models$model_up[1] + models$model_up[2]*Ti

    # Compute interactions among pairs of species
    L = numeric(npairs)
    L[Tj > lo & Tj < up] = 1

  })
  return(list(L = L))
}
