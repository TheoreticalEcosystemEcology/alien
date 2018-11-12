#' @title Compute the trait matrix (or matrices) for a given alienData object
#'
#' @description Computes the trait matrix (or matrices) using \code{dfNodes} and \code{nmTrait} of an \link{alienData} object.
#'
#' @param object An object of class \code{alienData}.
#' @param level Either 'individual' or 'species'. The ecological level on which the traits should be extracted. Default is 'species'.
#'
#' @author
#' F. Guillaume Blanchet
#' 
#' @export
getTraitMatrix <- function(object, level = "species"){
  # Extract the trait information
  selCol <- colnames(object$dfNodes) %in% object$nmTrait
  traitRaw <- cbind(object$dfNodes$idSp,
                    object$dfNodes[,selCol])
  
  if(level == "species"){
    dfTrait <- reshape2::dcast(data = df_trait, as.formula(paste(id, "~ traitName")),
                                value.var = "value")
    
  }
  
  if(level == "individual"){
    
  }
  
}