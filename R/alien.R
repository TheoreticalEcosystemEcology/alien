#' alien
#'
#' @name alien
#' @docType package
#' @description This should predict interactions!
#' @useDynLib alien
#' @importFrom utils tail stack 
#' @importFrom stats runif as.formula model.matrix predict
#' @importFrom GenSA GenSA
NULL


## helpers 
cat_sep <- function(n = 24) cat(paste(rep("-", n), collapse = ""), "\n")

cat_info <- function(...) cat("*", ..., "\n")