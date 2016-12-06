#' @name weightFcts
#' @aliases weightedMean
#' @aliases weightedSd
#'
#' @title weighted mean and sd
#'
#' @description just weighted mean and sd.
#'
#' @param x vector to compute the mean over.
#' @param w a vector of same length with the weigths of each element.
#'
#' @return a value with the weighted mean or sd.
#'
#' @examples
#' weightedMean(c(2,3,4), c(1,1,2))
#' weightedSd(c(2,3,4), c(2,1,2))
#'
#' @export
weightedMean <- function(x, w) {
    sum(x * w)/sum(w)
}
#' @export
weightedSd <- function(x, w) {
    sum_w <- sum(w)
    sum_w2 <- sum(w^2)
    mean_w <- sum(x * w)/sum(w)
    ((sum_w/(sum_w^2 - sum_w2)) * sum(w * (x - mean_w)^2))^0.5
}
