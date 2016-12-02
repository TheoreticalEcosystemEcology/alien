iEat_bin <- function(S0, S1, S2 = S1, sourceSim, targetSim = sourceSim, K = 5, minSim = 0.3, minWt = 1, predict = 'full algorithm') {

    # Parameters:
        # S0        Matrix, catalogue of empirical data used to infer predictions
        # S1        Vector, list of taxa forming networking for which topology is predicted
        # S2        Vector, list of taxa in S1 for which we wish to predict resources
        #               (if unspecified, S2 == S1 and the whole network is predicted)
        # sourceSim   Matrix (numeric), source similarity matrix between S1 taxa and the union of S0 and S1 taxa,
        #               structure sourceSim[unique(S0,S1), S1]
        # targetSim    Matrix (numeric), source similarity matrix between S1 taxa and the union of S0 and S1 taxa,
        #               structure sourceSim[unique(S0,S1), S1] (if unspecified,
        #               targetSim == sourceSim and no similarity distinction between resources and consumers)
        # K         Integer, how many neighbours for K nearest neighbour evaluation
        # minSim    Integer, minimum similarity value accepted to consider taxa as similar (implemented to
        #               avoid unrealistic interactions)
        # minWt     Integer, minimum weight for candidate source to become a predicted source
        # predict   String, specifies whether the predictions are made from the "full algorithm", the "catalogue"
        #               or the "predictive" contribution. If unspecified, predict == 'full algorithm'. See
        #               Beauchesne et al. (2016) for more details. If predict == 'catalogue', the methodology
        #               corresponds to the approach presented by Gray et al. (2015).

    # Output:
        # A topology matrix of structure output[S1, S2]

    # TODO: Stops:
        # if all S1 and S0 not in sourceSim & targetSim rows, stop
        # if all S2 not in sourceSim & targetSim columns, stop
        # matrices need to be numeric

    #Embedded functions
    KNN <- function(taxa, matSim, K, minSim) {
        # K nearest neighbout (KNN) selection
        library(magrittr)
        # Most similar taxa
        similar <- matSim[taxa, ] %>%
                    .[!names(.) %in% i] %>% # removing i from most similar targetSim
                    .[order(., decreasing = TRUE)] %>%
                    {
                        if(.[K+1] == .[K]) # if K + 1 == K, randomly sample a most similar taxa and pick K most similar taxa
                            c(.[which(. > .[K])], .[sample(which(. == .[K]))])[1:K]
                            else .[1:K]
                    } %>%
                    .[!. == 0 & . > minSim] # remove all similarities == 0 and similarities below minSim
        return(similar)
    }

    candLink <- function(similar, candidates) {
            # Candidate links
            for(l in names(similar)) { # extracting source candidates
                # if((l %in% candidates[, 'target']) == TRUE) { # if candidate is already in candidate list, add source' with wt to its weight
                if(l %in% candidates) { # if candidate is already in candidate list, add source' with wt to its weight
                # candidates[which(candidates[, 'target'] == l), 'weight'] <- as.numeric(candidates[which(candidates[, 'target'] == l), 'weight']) + as.numeric(similar[l])
                  candidates[which(candidates %in% l), 'weight'] <- as.numeric(candidates[which(candidates %in% l), 'weight']) + similar[l]
                } else {
                      candidates <- rbind(candidates, c(l,similar[l])) # if candidate is not in the list, add it source' with wt to its weight
                }
            }
        return(candidates)
    }

    # Code
    library(magrittr)

    # Algorithm
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

            candidates <- candidates %>% .[which(.[, 'weight'] >= minWt), ] # remove candidates with a weight below MW
            predictions[i, 'target_predictive'] <- paste(candidates[,'target'], collapse = ' | ')
        } #i
    }#if
    return(predictions)
}#iEat

# Simulated data for testing
S0 <- paste0('Taxon_',1:100) %>%
        data.frame(taxon = .,
            source = replicate(n = length(.), expr = paste(sample(., round(runif(1,1,8))), collapse = ' | ')),
            source = replicate(n = length(.), expr = paste(sample(., round(runif(1,1,8))), collapse = ' | ')),
            row.names = .,
            stringsAsFactors = FALSE)
S1 <- as.character(sample(S0[,'taxon'], 10))
S2 <- S1
predict = 'full algorithm'
K = 5
minSim = 0.3
minWt = 1
sourceSim <- targetSim <- matrix(nrow = nrow(S0), ncol = length(S1), data = runif(nrow(S0) * length(S1), min = 0, max = 1), dimnames = list(S0[,'taxon'],S1))
