#' @name fitNicheBin
#'
#' @title This function use a pair of traits.
#'
#' @description By convention, i is the target layer and j is the opposed layer
#' Data: a data frame with Ti as traits of the layer i and Tj traits of the layer j
#' each row correspond to a pair of species with known interaction
#'
#' @param data a dataset with two columns named Tj and Ti
#' @param thresh quantiles to be estimated (tau in qr() function)
#'
#' @author
#' Dominique Gravel
#'
#' @references
#'
#' @export
fitNicheBin <- function(data, thresh) {
    # input data should be checked Fit model
    model_o <- stats::lm(Tj ~ Ti, data)
    model_lo <- quantreg::rq(Tj ~ Ti, tau = thresh, data)
    model_up <- quantreg::rq(Tj ~ Ti, tau = 1 - thresh, data)
    
    # Return results
    return(list(model_o = model_o, model_lo = model_lo, model_up = model_up))
}
