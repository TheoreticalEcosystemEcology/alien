#' @name fitDMC
#'
#' @title Fit direct matching centrality
#'
#' @description Fit direct matching centrality model
#'
#' @param formula formula to define the model.
#' @param data an object of the class \code{\link{alienData}}
#' @param class Method to use to estimate the model. Either "randomForest" (\link[randomForest]{randomForest}) or "glm" (\code{\link[stats]{glm}}). 
#' @param family The family of the response variable. See \link[stats]{family}, or the choices available.
#' @param \dots Other parameters passed to either \link[randomForest]{randomForest} or \link[stats]{glm}.
#' 
#' @author
#' 
#' Dominique Gravel, Steve Vissault, F. Guillaume Blanchet
#'
#' @importFrom stats glm
#' @importFrom randomForest randomForest
#' @importFrom stats aggregate as.formula
#' @importFrom utils type.convert
#'
#' @export
fitDMC <- function(formula, data, class = NULL, family = NULL, 
                   traits = NULL, step = FALSE, ...) {

  stopifnot(class(data) == "alienData")

  # Construct adjencency matrix
  adjMat <- getAdjacencyMatrix(data, bipartite = TRUE)
  
  # Construct trait matrix
  traits <- getTrait(data, bipartite = TRUE)
  
  # Check for NAs in traits
  if(any(sapply(traits, function(x) any(is.na(x))))){
    stop("There is at least one NA in the traits. Use getTrait() to investigate.")
  }
  
  # Unfold adj
  
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
        df_trait <- subset(df_trait, df_trait$traitName %in% traits)
    }

    # get covariates for idFrom
    df_interact <- merge(df_interact, df_trait, all.x = TRUE, by.x = "idFrom", by.y = id)

    # get covariates for idTo
    df_interact <- merge(df_interact, df_trait, all.x = TRUE, by.x = "idTo", by.y = id)


    # remove columns containing all NA (no species match to the traits)
    df_interact <- df_interact[, colSums(is.na(df_interact)) < nrow(df_interact)]  # TODO: Check
    # remove rows with all trait NA
    df_interact <- df_interact[rowSums(is.na(df_interact[, -1])) == 0, ]

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

    model
}
