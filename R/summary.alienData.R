#' @title Format data for the \code{alien} class
#'
#' @description These functions are all \code{methods} for class \code{alienData}
#' or \code{summary.alienData}.
#'
#' @param object An object of class \code{alienData}.
#' @param x An object of class \code{summaryAlienData}.
#'
#' @author Kevin Cazelles
#'
#' @importFrom magrittr %<>%
#'
#' @export

summary.alienData <- function(object) {
    out <- object
    ###
    if (!is.null(out$nmTrait)) {
        out$Traits <- summary(out$dfNodes[, out$nmTrait, drop = F])
    } else out$Traits <- NULL
    ##
    if (!is.null(out$nmPhylo)) {
        out$Phylos <- summary(out$dfNodes[, out$nmPhylo, drop = F])
    } else out$Phylos <- NULL
    ##
    if (!is.null(out$nmTaxo)) {
        out$Taxos <- summary(out$dfNodes[, out$nmTaxo, drop = F])
    } else out$Taxos <- NULL
    ##
    out$dfNodes %<>% summary

    class(out) <- "summary.alienData"
    return(out)
}

# #' @describeIn summary.alienData Return the values of y-axis for a given
# percentage.
#' @export

print.summary.alienData <- function(x) {
    msep <- function() cat("---------------\n\n")
    txt <- "\nA alienData object including :\n"
    txt %<>% paste0(" ---> ", x$nbNodes, " nodes", "\n")
    txt %<>% paste0(" ---> ", x$nbInteractions, " interactions ", ifelse(x$directed,
        "directed", "undirected"), "\n")
    if (!is.null(x$nbSite))
        txt %<>% paste0(" ---> ", x$nbSite, " sites", "\n")
    if (!is.null(x$nbOcc))
        txt %<>% paste0(" ---> ", x$nbOcc, " occurrences", "\n")
    cat(txt)
    msep()
    ##
    cat("Nodes summary:\n")
    print(x$dfNodes)
    msep()
    ##
    if (!is.null(x$Traits)) {
        cat("Traits summary:\n")
        print(x$Traits)
        msep()
    }
    ##
    if (!is.null(x$Phylos)) {
        cat("Phylos summary:\n")
        print(x$Phylos)
        msep()
    }
    ##
    if (!is.null(x$Taxos)) {
        cat("Taxos summary:\n")
        print(x$Taxos)
        msep()
    }
    ##
    cat("Available Methods:\n")
    print(x$availableMeths)
    ##
    invisible(x)
}
