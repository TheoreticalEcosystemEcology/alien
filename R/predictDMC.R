#' @name predictDMC
#'
#' @title prediction based on direct matching centrality method
#'
#' @description predict species interaction based on the direct matching centrality model
#'
#' @param model a model applied on a alienData object.
#' @param newdata the data used for predictions.
#' @param type kinf od response desired.
#' @author
#' Dominique Gravel & Steve Vissault
#'
#'
#' @export
predictDMC <- function(model, data = NULL, type = "response") {
    
    if (model$level == "species") {
        
        data <- bartomeus
        
        interactSp <- reshape2::melt(data$interactSp, na.rm = TRUE)
        names(interactSp) <- c("idFrom", "idTo", "n_interact")
        
        traitSp <- reshape2::dcast(data$traitSp, idSp ~ traitName)
        traitSp <- data.frame(idSp = traitSp[, 1], as.data.frame(sapply(traitSp[, 
            -c(1)], type.convert)))
        
        
        # Cast trait TO
        interactSp <- merge(interactSp, traitSp, by.x = "idFrom", by.y = "idSp", 
            all.x = TRUE)
        
        # Cast trait FROM
        interactSp <- merge(interactSp, traitSp, by.x = "idTo", by.y = "idSp", all.x = TRUE)
        
        
    }
    
    
}
