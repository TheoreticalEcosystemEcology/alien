#' @name fitDirectCentrality
#'
#' @title Fit Direct matching centrality among species or individus
#'
#' @description Fit direct matching centrality models among species or individus
#'
#' @param data a dataframe with a first column named I (the interaction) and extra columns with covariates (can be named at will)
#' @param specify the algorithm: one of the `stats` family or the classification algorithm `randomForest`. Default is binomial.
#' @param specify the ecological level on which the method is applied. Choices are 'individus' or 'species'. Default is species.
#' @param ... other params passed to the glm or randomForest algorithm
#' @exemple fitDirectCentrality(bartomeus,family="poisson",level="species")
#' @author
#' Dominique Gravel
#'
#' @references
#' Poisot T., Cirtwill A. R., Cazelles A. R., Cazelles K., Gravel D., Fortin M.-J. and Stouffer D.B. (2015). The structure of probabilistic networks. \emph{bioRxiv 016485}; doi: https://doi.org/10.1101/016485.
#'
#' @export
fitDirectCentrality <- function(data, family = "binomial", level = "species", ...) {

    if (class(data) != "alienData") {

        stop("`data` arg has to be alienData class")

    }

    if (level == "species") {
        if (all(is.null(data$traitSp), is.null(data$interactSp))) {
            stop("traitSp and interactSp are absent from data. Both are to be provided to fit the algorithm at the individu level")
        }

        # set I
        interactSp <- data.frame(expand.grid(rownames(data$interactSp), colnames(data$interactSp)),
            I = as.vector(data$interactSp))
        names(interactSp)[1:2] <- c("spFrom", "spTo")

        # drop NA
        interactSp <- interactSp[which(!is.na(interactSp$I)), ]

        # cast trait
        traitSp <- reshape2::dcast(data = data$traitSp, idSp ~ traitName, value.var = "value")

        # get covariates for idFrom
        interactSp <- merge(interactSp, traitSp, all.x = TRUE, by.x = "spFrom", by.y = "idSp")
        colnames(interactSp)[4:ncol(interactSp)] <- paste("spFrom_", colnames(interactSp)[4:ncol(interactSp)],
            sep = "")

        # get covariates for idTo
        nc <- ncol(interactSp)
        interactSp <- merge(interactSp, traitSp, all.x = TRUE, by.x = "spTo", by.y = "idSp")
        colnames(interactSp)[(nc + 1):ncol(interactSp)] <- paste("spTo_", colnames(interactSp)[(nc +
            1):ncol(interactSp)], sep = "")

        # remove columns containing all NA (no species match to the traits)
        interactSp <- interactSp[, colSums(is.na(interactSp)) < nrow(interactSp)]

        # subset df to get only I and covariates (traits)
        interactSp <- interactSp[, -c(1:2)]

        # guess and cast each columns in the right data types
        interactSp <- data.frame(I = interactSp[, 1], as.data.frame(sapply(interactSp[,
            -c(1)], type.convert)))

        # set data to pass through model
        intertrait <- interactSp

    }

    if (level == "individus") {
        # check if traitInd and interactInd are provided by the function as.alienData()
        if (all(is.null(data$traitInd), is.null(data$interactInd))) {
            stop("traitInd and interactInd are absent from data. Both are to be provided to fit the algorithm at the individu level")
        }

        # set I
        interactInd <- data.frame(expand.grid(rownames(data$interactInd), colnames(data$interactInd)),
            I = as.vector(data$interactInd))
        names(interactInd)[1:2] <- c("indFrom", "indTo")

        # drop NA
        interactInd <- interactInd[which(!is.na(interactInd$I)), ]

        # cast trait
        traitInd <- reshape2::dcast(data = data$traitInd, idInd ~ traitName, value.var = "value")

        # get covariates for idFrom
        interactInd <- merge(interactInd, traitInd, all.x = TRUE, by.x = "indFrom", by.y = "idInd")
        colnames(interactInd)[4:ncol(interactInd)] <- paste("indFrom_", colnames(interactInd)[4:ncol(interactInd)],
            sep = "")

        # get covariates for idTo
        nc <- ncol(interactInd)
        interactInd <- merge(interactInd, traitInd, all.x = TRUE, by.x = "indTo", by.y = "idInd")
        colnames(interactInd)[(nc + 1):ncol(interactInd)] <- paste("indTo_", colnames(interactInd)[(nc +
            1):ncol(interactInd)], sep = "")

        # remove columns containing all NA (no species match to the traits)
        interactInd <- interactInd[, colSums(is.na(interactInd)) < nrow(interactInd)]

        # subset df to get only I and covariates (traits)
        interactInd <- interactInd[, -c(1:2)]

        # guess and cast each columns in the right data types
        interactInd <- data.frame(I = interactInd[, 1], as.data.frame(sapply(interactInd[,
            -c(1)], type.convert)))

        # set data to pass through model
        intertrait <- interactInd

    }


    if (family == "randomForest") {
        model <- randomForest::randomForest(I ~ . * ., intertrait, ...)
    } else {
        model <- stats::glm(I ~ . * ., intertrait, family = family, ...)
    }


    ## set class set attributes for family


    return(model)
}
