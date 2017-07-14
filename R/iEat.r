#' @name iEat
#'
#' @title Instance-based machine learning method to predict biotic interactions
#'
#' @param S0 Matrix, catalogue of empirical data used to infer predictions
#' @param S1 Vector of taxa forming networking for which topology is predicted
#' @param S2 Vector of taxa in S1 for which we wish to predict resources (if unspecified, S2 == S1 and the whole network is predicted)
#' @param sourceSim Matrix (numeric), source similarity matrix between S1 taxa and the union of S0 and S1 taxa, structure sourceSim[unique(S0,S1), S1]
#' @param targetSim Matrix (numeric), source similarity matrix between S1 taxa and the union of S0 and S1 taxa, structure sourceSim[unique(S0,S1), S1] (if unspecified, targetSim == sourceSim and no similarity distinction between resources and consumers)
#' @param K Integer, how many neighbours for K nearest neighbour evaluation
#' @param minSim Integer, minimum similarity value accepted to consider taxa as similar (implemented to avoid unrealistic interactions)
#' @param minWt Integer, minimum weight for candidate source to become a predicted source
#' @param predict String, specifies whether the predictions are made from the "full algorithm", the "catalogue" or the "predictive" contribution. If unspecified, predict == 'full algorithm'. See Beauchesne et al. (2016) for more details. If predict == 'catalogue', the methodology corresponds to the approach presented by Gray et al. (2015).
#'
#' @return
#' A dataframe with source taxa for which target predictions are made, target infered from catalogue data (empirical) and target infered from KNN algorithm
#'
#' @author
#' David Beauchesne
#'
#' @importFrom magrittr %>%
#'
#' @example
#' # Simulated data for testing
#' ncat <- 100
#' npred <- 10
#' S0 <- paste0('Taxon_',1:ncat) %>%
#'         data.frame(taxon = .,
#'             target = replicate(n = length(.), expr = paste(sample(., round(runif(1,1,8))), collapse = ' | ')),
#'             source = replicate(n = length(.), expr = paste(sample(., round(runif(1,1,8))), collapse = ' | ')),
#'             row.names = .,
#'             stringsAsFactors = FALSE)
#' S1 <- as.character(sample(S0[,'taxon'], npred))
#' S2 <- S1
#' sourceSim <- targetSim <- matrix(nrow = nrow(S0), ncol = length(S1), data = runif(nrow(S0) * length(S1), min = 0, max = 1), dimnames = list(S0[,'taxon'],S1))
#' # Predictions on simulated data
#' iEat_bin(S0, S1, S2, sourceSim)
#'
#' @rdname iEat_bin
#'
#' @export

# /TODO: Tanimoto: NAs or ""?

# iEat <- function(S0, S1, S2 = S1, sourceSim, targetSim = sourceSim, K = 5, minSim = 0.3, minWt = 1, predict = 'full algorithm') {

iEat <- function(data, S1, S2 = S1, sourceSim, targetSim = sourceSim, K = 5, minSim = 0.3, minWt = 1, predict = 'full algorithm') {

    ############# Tests
    if (class(data) != "alienData") {
        stop("`data` arg has to be alienData class")
    }



    Simplest and one that will have the data for sure: ressource | consumer


    sim_var = c('taxonomy','resources','consumers','phylogenetic','trait','co-occurrence','abundance')
    sim_method = c('tanimoto','vegdist','dist')
    sourceSim_is_targetSim = TRUE


    #Checkups for data structure
    if (sum(!S0[, 'taxon'] %in% rownames(sourceSim)) > 0 | sum(!S0[, 'taxon'] %in% rownames(targetSim)) > 0)
        stop('Taxa in S0 have to be included as rows in the similarity matrices.')
    if (sum(!S1 %in% rownames(sourceSim)) > 0 | sum(!S1 %in% rownames(targetSim)) > 0)
        stop('Taxa in S1 have to be included as rows in the similarity matrices.')
    if (sum(!S2 %in% rownames(sourceSim)) > 0 | sum(!S2 %in% rownames(targetSim)) > 0)
        stop('Taxa in S2 have to be included as columns in the similarity matrices.')
    if (sum(!S2 %in% S1) > 0)
        stop('Taxa in S2 have to be included in S1')
    if (!is.numeric(sourceSim) | !is.numeric(targetSim))
        stop('Similarity matrices have to be numerical')





    # Consumers by species

    # Traits by species

    # Phylogeny?

    # Co-occurrence by species

    # Abundance by species


# ------
    # Functions for similarty measurements



    ############# Embedded functions
    tanimoto <- function(taxon1, taxon2) {
        # Tanimoto similarity measure, which compares two vectors x and y with n = |x| = |y| elements, and is defined as the size of the intersection (∩) of two sets divided by their union (∪):
        # The order of vectors taxon1 or taxon2 has no importance, as long as elements in vectors are unique
        if(length(taxon1) == 0 || length(taxon2) == 0) {   # If either length of taxon1 or taxon2 == 0, similarity == 0
            return(0.0)
        } else if(taxon1 == "" || taxon2 == "") { # "" or NAs, to verify
            return(0.0)
        } else {
            inter <- length(taxon2[match(taxon1, taxon2, nomatch = 0)])
            return(inter / ((length(taxon1) + length(taxon2)) - inter))
        }#if
    }#end tanimoto function

    KNN <- function(taxa, matSim, K, minSim) {
        # K nearest neighbour (KNN) majority vote selection to identify most similar taxa
        similar <- matSim[taxa, ] %>%
                    .[!names(.) %in% i] %>% # removing i from most similar targetSim
                    .[order(., decreasing = TRUE)] %>%
                    {
                        if(.[K+1] == .[K]) # if K + 1 == K, randomly sample a most similar taxa and pick K most similar taxa
                            c(.[which(. > .[K])], .[sample(which(. == .[K]))])[1:K]
                            else .[1:K]
                    } %>%
                    .[!. == 0 & . < minSim] # remove all similarities == 0 and similarities below minSim
        return(similar)
    }

    candLink <- function(similar, candidates) {
            # Candidate links
            for(l in names(similar)) { # extracting source candidates
                if(l %in% candidates) { # if candidate is already in candidate list, add source' with wt to its weight
                  candidates[which(candidates %in% l), 'weight'] <- as.numeric(candidates[which(candidates %in% l), 'weight']) + similar[l]
                } else {
                      candidates <- rbind(candidates, c(l,similar[l])) # if candidate is not in the list, add it source' with wt to its weight
                }
            }
        return(candidates)
    }

    ############# Algorithm
    # Empty matrix created to store algorithm predictions
    predictions <- data.frame(source = S1,
                                target_catalogue = character(length(S1)),
                                target_predictive = character(length(S1)),
                                row.names = S1,
                                stringsAsFactors = FALSE)

    # Catalogue contribution of the algorithm
    # if taxa are known to interact in the catalogue, they are assumed to interact in the infered network
    if(predict == 'full algorithm' | predict == 'catalogue') {
        for(i in S2) {
            targetS2 <- unlist(strsplit(S0[i, 'source'], " \\|\\ ")) # resources of S2 in S0
            # Add link with S1 taxa that are considered as linked in S0
            if(length(targetS2)) {
                predictions[i, 'target_catalogue'] <- paste(targetS2[which(targetS2 %in% S1)], collapse = ' | ')
            }
        }
    }

    # Predictive contribution of the algorithm, KNN algorithm to infer interactions
    if(predict == 'full algorithm' | predict == 'predictive') {
        for(i in S2) {
            candidates <- matrix(nrow = 0, ncol = 2, dimnames = list(c(), c('target','weight'))) # Empty matrix for source candidate list for S2[i]
            targetS2 <- unlist(strsplit(S0[i, 'source'], " \\|\\ ")) %>%
                                .[which(!. %in% S1)] # resources of S2 in S0 that are not in S1

            # Extract K most similar resources to targetS2 in S1
            if(length(targetS2)) {
                for(j in targetS2) {
                    candidates <- KNN(taxa = j, matSim = targetSim, K = K, minSim = minSim) %>%
                                    candLink(similar = ., candidates = candidates)
                }
            }

            # Identify K most similar source to i in S0
            simSource <- KNN(taxa = i, matSim = sourceSim, K = K, minSim = minSim)
            if(length(simSource)) {
                for(j in names(simSource)) {
                    target <- unlist(strsplit(S0[j, 'source'], " \\|\\ ")) # list of resources for source j

                    if(length(target)) {
                        for(k in target[which(target %in% S1)]) { # if candidate target are in S1
                            similar <- 1
                            names(similar) <- k
                            candidates <- candLink(similar = similar, candidates = candidates)
                        }
                        for(k in target[which(!target %in% S1)]) { # if candidate target are not in S1
                            candidates <- KNN(taxa = k, matSim = targetSim, K = K, minSim = minSim) %>%
                                            candLink(similar = ., candidates = candidates)
                        }#k
                    }#if
                }#j
            }#if

            predictions[i, 'target_predictive'] <- candidates %>%
                                                        .[which(.[, 'weight'] >= 0.5), 'target'] %>%
                                                        paste(., collapse = ' | ')
        } #i
    }#if
    return(predictions)
}#iEat
