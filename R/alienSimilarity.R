#' @name alienSimilarity
#'
#' @title
#'
#' @param data A \code{alienData} class object
#' @param similarityParam A vector indicating which ecological parameters should
#' be used to compute similarity between taxe. Choices are: "resource", "consumer",
#' "taxonomy", "phylo", "trait", "cooccurrence", "abundance". If no values
#' are provided, the function will default the value of \code{similarityParam}
#' to "consumer".
#' @param similarityMethod A vector indicating which method should be used to
#' evaluate similarity with each ecological parameter, with
#' \code{length(similarityMethod) == length(similarityParam)}. Choices are:
#' "tanimoto", "vegdist", "dist". If no values are provided, the function will
#' default the value of \code{similarityMethod} to "tanimoto" for characters and
#' "dist" for integers.
#' @param similarityWeight A vector indicating the weight to use to combine all
#' ecological parameters into an integrated similarity evaluation, with
#' \code{length(similarityWeight) == length(similarityParam)}. Must sum to 1.
#' If not provided, all ecological parameters will be given equal weights.
#' @param taxaSubset A vector indicating the subset of taxa for which similarity
#' should be evaluated with values found in unique ids provided in \code{alienData}
#' object in \code{dfNodes$idNodes}. iEat only requires an evaluation of similarity
#' between taxa for which interactions are being predicted and the full catalog of
#' interactions. A complete S x S taxa similarity evaluation can therefore
#' become computationnally heavy. This parameter allows the user to significantly
#' decrease processing time when the catalog provided is very big.
#'
#' @return
#' An object of class \code{alienSimilarity} is returned.
#'
#' @author
#' David Beauchesne
#'
#' @importFrom magrittr %>%
#'
#' @example
#' # Simulated data for testing
#'
#' @rdname
#'
#' @export


alienSimilarity <- function(data, similarityParam = NULL, similarityMethod = NULL, similarityWeight = NULL, taxaSubset = NULL, ) {

    availableParam <- c('taxonomy','resource','consumer','phylo','trait','cooccurrence','abundance')
    availableMeths <- c('tanimoto','vegdist','dist')

    ############# Tests
    # Data class
        if (class(data) != "alienData") {
            stop("`data` arg has to be alienData class")
        }

    # Similarity parameters
        if(is.null(similarityParam)) {
            similarityParam <- 'consumer'
        }
        stopifnot(similarityParam %in% availableParam)
        nbParam <- length(similarityParam)

        # Similarity parameters available from data provided
        if (is.null(data$nmTrait) & 'trait' %in% similarityParam) stop('Traits are not available in the data provided')
        if (is.null(data$nmPhylo) & 'phylogenetic' %in% similarityParam) stop('Phylogeny is not available in the data provided')
        if (is.null(data$nmTaxo) & 'taxonomy' %in% similarityParam) stop('Taxonomy is not available in the data provided')
        # /TODO: add cooccurrence
        # /TODO: add abundance

    # Similarity method
        if(is.null(similarityMethod)) {
            similarityMethod <- character(nbParam)
            similarityMethod[similarityParam %in% c("taxonomy","resource","consumer","trait")] <- 'tanimoto'
            similarityMethod[similarityParam %in% c("abundance","cooccurrence")] <- 'dist'
            #/TODO: how to incorporate phylogenies?
        }
        stopifnot(similarityMethod %in% availableMeths)

    # Weight param
        if (is.null(similarityWeight)) {
            similarityWeight <- rep((1 / nbParam),nbParam)
        }
        stopifnot(sum(similarityWeight) == 1)

    # Taxa subset
        if (is.null(taxaSubset)) {
            taxa <- dfNodes$idNodes
        }
        stopifnot(taxa %in% dfNodes$idNodes)
        nbTaxa <- length(taxa)

    ############# Embedded functions
    tanimoto <- function(taxon1, taxon2) {
        # Tanimoto similarity measure, which compares two vectors x and y with
        #   n = |x| = |y| elements, and is defined as the size of the
        #   intersection (∩) of two sets divided by their union (∪):
        # The order of vectors taxon1 or taxon2 has no importance, as long as
        #   elements in vectors are unique
        if(length(taxon1) == 0 || length(taxon2) == 0) {   # If either length of taxon1 or taxon2 == 0, similarity == 0
            return(0.0)
        } else if(taxon1 == "" || taxon2 == "") { # "" or NAs, to verify
            return(0.0)
        } else {
            inter <- length(taxon2[match(taxon1, taxon2, nomatch = 0)])
            return(inter / ((length(taxon1) + length(taxon2)) - inter))
        }#if
    }#end tanimoto function

    similarityEval <- function(method, taxon1, taxon2) {
        if (method == 'tanimoto') return(tanimoto(taxon1, taxon2))
        if (method == 'vegdist') return(vegdist(taxon1, taxon2))
        if (method == 'dist') return(dist(taxon1, taxon2))
    }

    leftJoinNA <- function(x, y, ...) {
      dplyr::left_join(x = x, y = y, by = ...) %>%
        dplyr::mutate_each(dplyr::funs(replace(., which(is.na(.)), 0)))
    }


    ############# Similarity evaluation
    dfSim <- expand.grid(taxon1 = as.character(dfNodes$idNodes), taxon2 = as.character(dfNodes$idNodes), stringsAsFactors = F)

    # Resource shared
        if ('resource' %in% similarityParam) {
            # Extract resources
            resource <- data$dfEdges %>%
                        tidyr::spread(idFrom, idFrom, fill = 0) %>%
                        as.data.frame(row.names = .$idTo) %>%
                        dplyr::select(-value, -idTo) %>%
                        apply(1, function(x) paste(x[x > 0]))

            # Evaluate similarity
            Sim <- expand.grid(taxon1 = names(resource), taxon2 = names(resource), stringsAsFactors = F) %>%
                     mutate(similarity = 0)
            # Similarity for all combinations of taxa with data to allow similarity evaluation

            #/TODO: Change to apply function

            for(i in 1:nrow(Sim)) Sim$similarity[i] <- similarityEval(similarityMethod[which(similarityParam == 'resource')], resource[[Sim$taxon1[i]]], resource[[Sim$taxon2[i]]])

            # Add to similarity data frame
            dfSim <- leftJoinNA(dfSim, Sim, c('taxon1','taxon2')) %>%
                     dplyr::rename(resource = similarity)
        }

    # Consumer shared
        if ('consumer' %in% similarityParam) {
            # Extract consumers
            consumer <- data$dfEdges %>%
                        tidyr::spread(idTo, idTo, fill = 0) %>%
                        as.data.frame(row.names = .$idFrom) %>%
                        dplyr::select(-value, -idFrom) %>%
                        apply(1, function(x) paste(x[x > 0]))

            # Evaluate similarity
            Sim <- expand.grid(taxon1 = names(consumer), taxon2 = names(consumer), stringsAsFactors = F) %>%
                     mutate(similarity = 0)
            # Similarity for all combinations of taxa with data to allow similarity evaluation

            #/TODO: Change to apply function

            for(i in 1:nrow(Sim)) Sim$similarity[i] <- similarityEval(similarityMethod[which(similarityParam == 'consumer')], consumer[[Sim$taxon1[i]]], consumer[[Sim$taxon2[i]]])

            # Add to similarity data frame
            dfSim <- leftJoinNA(dfSim, Sim, c('taxon1','taxon2')) %>%
                     dplyr::rename(consumer = similarity)
        }

    # Taxonomy
        if ('taxonomy' %in% similarityParam) {
            # Extract taxonomy
            taxonomy <- data$dfNodes[, data$nmTaxo] %>%
                        split(seq(nrow(.))) %>%
                        lapply(function(x) x <- x[!is.na(x)])

            # Evaluate similarity
            Sim <- expand.grid(taxon1 = names(taxonomy), taxon2 = names(taxonomy), stringsAsFactors = F) %>%
                     mutate(similarity = 0)
            # Similarity for all combinations of taxa with data to allow similarity evaluation

            #/TODO: Change to apply function

            for(i in 1:nrow(Sim)) Sim$similarity[i] <- similarityEval(similarityMethod[which(similarityParam == 'taxonomy')], taxonomy[[Sim$taxon1[i]]], taxonomy[[Sim$taxon2[i]]])

            # Add to similarity data frame
            dfSim <- leftJoinNA(dfSim, Sim, c('taxon1','taxon2')) %>%
                     dplyr::rename(taxonomy = similarity)
        }

    # Phylogeny
        #/TODO: integrate phylogeny
        # if (phylo %in% similarityParam)

    # Trait
        if ('trait' %in% similarityParam) {
            # Extract traits
            trait <- data$dfNodes[, data$nmTrait] %>%
                        split(seq(nrow(.))) %>%
                        lapply(function(x) x <- x[!is.na(x)])

            # Evaluate similarity
            Sim <- expand.grid(taxon1 = names(trait), taxon2 = names(trait), stringsAsFactors = F) %>%
                     mutate(similarity = 0)
            # Similarity for all combinations of taxa with data to allow similarity evaluation
            #/TODO: Change to apply function
            for(i in 1:nrow(Sim)) Sim$similarity[i] <- similarityEval(similarityMethod[which(similarityParam == 'trait')], trait[[Sim$taxon1[i]]], trait[[Sim$taxon2[i]]])

            # Add to similarity data frame
            dfSim <- leftJoinNA(dfSim, Sim, c('taxon1','taxon2')) %>%
                     dplyr::rename(trait = similarity)

        }

    # Cooccurrence
        #/TODO: integrate phylogeny
        # if (cooccurrence %in% similarityParam)

    # Abundance
        #/TODO: integrate phylogeny
        # if (abundance %in% similarityParam)


    ############# Integrated similarity evaluation
        # Reorder similarity weights
            wt <- similarityWeight[match(similarityParam, colnames(dfSim[, 3:ncol(dfSim)]))]
        # Matrix of similarities
            mat <- as.matrix(dfSim[, 3:ncol(dfSim)])

        # Apply weights to similarity evaluations
            similarity <- data.frame(taxon1 = dfSim$taxon1,
                                     taxon2 = dfSim$taxon2,
                                     similarity = rowSums(mat %*% diag(wt)),
                                     stringsAsFactors = F)

    ############################## Return results
    res <- list(similarity = similarity, # integrated similarity matrix
                dfSim = dfSim, # data frame of similarities used to compute final similarity matrix
                similarityChar = data.frame(similarityParam = similarityParam, # Names of similarity parameters used
                                            similarityMethod = similarityMethod, # Method used for each parameters
                                            similarityWeight = similarityWeight, # Weight used for each parameters
                                            stringsAsFactors = F,
                                            row.names = NULL), 
                nbParameters = nbParam) # number of ecological parameters used to compute similarity

    class(res) <- "alienSimilarity"
    return(res)


}
