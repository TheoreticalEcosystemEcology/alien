#' @name fitDirectCentrality
#'
#' @title Fit Direct matching centrality among species or individus
#'
#' @description Fit direct matching centrality models among species or individus
#'
#' @param data a dataframe with a first column named I (the interaction) and extra columns with covariates (can be named at will)
#' @param specify the algorithm: one of the `stats` family or the classification algorithm `randomForest`. Default is binomial.
#' @param specify the ecological level on which the method is applied. Choices are 'individus' or 'species'. Default is species.
#' @param set the formula passed to the algorithm (see `family`).
#' @param specificy the traits to include in the algorithm
#' @param ... other params passed to the glm or randomForest algorithm
#' @examples fitDirectCentrality(bartomeus,family='poisson',level='species')
#' @author
#' Dominique Gravel & Steve Vissault
#'
#' @references
#' Rohr
#'
#' @export
fitDirectCentrality <- function(data, family = "binomial", level = "species", formula = "I ~ . * .", traits = NULL, ...) {

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
    df_interact <- data.frame(expand.grid(rownames(df_interact), colnames(df_interact)), I = as.vector(df_interact))
    names(df_interact)[1:2] <- c("idFrom", "idTo")

    # drop NA
    df_interact <- df_interact[which(!is.na(df_interact$I)), ]

    # cast df_trait
    df_trait <- reshape2::dcast(data = df_trait, id ~ traitName, value.var = "value")

    # select df_trait
    if(!is.null(traits)){
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
    df_interact <- data.frame(I = df_interact[, 1], as.data.frame(sapply(df_interact[, -c(1)],
        type.convert)))

    if (family == "randomForest") {
        model <- randomForest::randomForest(formula, df_interact, ...)
    } else {
        model <- stats::glm(formula, df_interact, family = family, ...)
        if (step == TRUE){
          model <- step(model)
        }
    }


    ## set class set attributes for family


    return(model)
}
