#' @name fitDirectCentrality
#'
#' @title Fit direct matching centrality among species or individus
#'
#' @description Fit direct matching centrality models among species or individus
#'
#' @param data an object of the class alienData, see as.alienData function.
#' @param class method to be applied on the data which can be 'rf' for randomForest or 'glm'.
#' @param family one family of the `stats` package (e.g. binomial(), Gamma(), gaussian() etc.)
#' @param formula set the formula passed to the algorithm (see `family`).
#' @param level specify the ecological level on which the method is applied. Choices are 'individus' or 'species'. Default is species.
#' @param traits vector of traits to include in the algorithm
#' @param step logical, specify if variables are selected by stepAIC. Only availabe for glm class. FALSE by default
#' @param ... other params passed to the glm, step or randomForest functions.
#' @examples fitDMC(bartomeus,class='glm',family=gaussian(),level='species',step=TRUE)
#' @author
#' Dominique Gravel & Steve Vissault
#'
#' @importFrom stats aggregate as.formula
#' @importFrom utils type.convert
#' @import randomForest
#'
#' @export
fitDMC <- function(data, class = NULL, family = NULL, formula = "I ~ . * .", level = "species", 
    traits = NULL, step = FALSE, ...) {
    
    if (class(data) != "alienData") {
        stop("`data` arg has to be alienData class")
    }
    
    if (level == "species") {
        if (all(is.null(data$traitSp), is.null(data$interactSp))) {
            stop("traitSp and interactSp are absent from data. Both are to be provided to fit the algorithm at the individu level")
        }
        
        df_trait <- data$traitSp
        df_interact <- data$interactSp
        id <- "idSp"
    }
    
    if (level == "individus") {
        # check if traitInd and interactInd are provided by the function as.alienData()
        if (all(is.null(data$traitInd), is.null(data$interactInd))) {
            stop("traitInd and interactInd are absent from data. Both are to be provided to fit the algorithm at the individu level")
        }
        
        df_trait <- data$traitInd
        df_interact <- data$interactInd
        id <- "idInd"
    }
    
    # set I
    df_interact <- data.frame(expand.grid(rownames(df_interact), colnames(df_interact)), 
        I = as.vector(df_interact))
    names(df_interact)[1:2] <- c("idFrom", "idTo")
    
    # drop NA
    df_interact <- df_interact[which(!is.na(df_interact$I)), ]
    
    # cast df_trait
    df_trait <- reshape2::dcast(data = df_trait, as.formula(paste(id, "~ traitName")), 
        value.var = "value")
    
    # select df_trait
    if (!is.null(traits)) {
        df_trait <- subset(df_trait, traitName %in% traits)
    }
    
    # get covariates for idFrom
    df_interact <- merge(df_interact, df_trait, all.x = TRUE, by.x = "idFrom", by.y = id)
    colnames(df_interact)[4:ncol(df_interact)] <- paste("idFrom_", colnames(df_interact)[4:ncol(df_interact)], 
        sep = "")
    
    # get covariates for idTo
    nc <- ncol(df_interact)
    df_interact <- merge(df_interact, df_trait, all.x = TRUE, by.x = "idTo", by.y = id)
    colnames(df_interact)[(nc + 1):ncol(df_interact)] <- paste("idTo_", colnames(df_interact)[(nc + 
        1):ncol(df_interact)], sep = "")
    
    # remove columns containing all NA (no species match to the traits)
    df_interact <- df_interact[, colSums(is.na(df_interact)) < nrow(df_interact)]  # TODO: Check
    
    
    # subset df to get only I and covariates (traits)
    df_interact <- df_interact[, -c(1:2)]
    
    # guess and cast each columns in the right data types
    df_interact <- data.frame(I = df_interact[, 1], as.data.frame(sapply(df_interact[, 
        -c(1)], type.convert)))
    
    if (all(!is.null(class) && class == "rf")) {
        model <- randomForest::randomForest(as.formula(formula), data = df_interact)
        
    } else if (all(!is.null(class) && class == "glm")) {
        
        if (is.null(family)) {
            stop("You have to provide a family for the glm, see argument family")
        }
        
        model <- stats::glm(as.formula(formula), data = df_interact, family = family)
        
        if (step == TRUE) {
            model <- step(model)
        }
        
    } else {
        
        stop("No methods have been provided, see argument class")
        
    }
    
    
    attr(model, "level") <- level
    class(model) <- "fitDMC"
    
    return(model)
}
