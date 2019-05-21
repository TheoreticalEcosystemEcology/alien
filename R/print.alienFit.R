#' @export
print.alienFit <- function(x, ...){
  baseAttr <- attributes(x)
  attributes(x) <- NULL
  
  attributes(x) <- list(dim = baseAttr$dim,
                          dimnames = baseAttr$dimnames)
  
  print(x)
}
