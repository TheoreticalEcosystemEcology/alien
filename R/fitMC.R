#' @name fitMC
#'
#' @title Esimation of iteractions probability using the matching centrality
#' approach.
#'
#' @description Esimation of iteractions probability using the matching centrality
#' approach as described in Rohr 2014 and Rohr 2016.
#'
#' @param data an object of the class alienData, see \code{alienData} function.
#' @param d dimensionnality.
#' @param mxt Numeric. Maximum running time in seconds.
#'
#' @author
#' Kevin Cazelles
#'
#' @details
#' \code{fitMC} implements the matching-centrality method as described in
#' Rohr 2014 and Rohr 2016. Briefly, the method uses latent traits to fit
#' interactions. The first step requires the definition of two sets of interacting
#' species: set1 and set2 (respectively of size \code{nset1} and \code{nset2}).
#' The first category of latent trait are the matching traits. (to be developped)
#' The second category include the centrality terms. (to be developped).
#' Latents traits are buitl under certain topological constraints:
#' 1- all vectors belongs to the orthogonal basis of the unit vector.
#' 2- all matching vector of a particluar set of specues are orthogonal to each other.
#'
#'
#' @return
#' An object of class \code{alienPredict}.
#'
#' @references
#' Rohr, R. P. & Bascompte, J. Components of Phylogenetic Signal in Antagonistic and Mutualistic Networks. Am. Nat. 184, 556--564 (2014).
#'
#' Rohr, R. P., Naisbit, R. E., Mazza, C. & Bersier, L.-F. Matching-centrality decomposition and the forecasting of new links in networks. Proc. R. Soc. B Biol. Sci. 283, 20152702 (2016).
#'
#' @export


fitMC <- function(data, d = 1, mxt = 10) {
    netObs <- getAdjacencyMatrix(data, binary = TRUE, bipartite = TRUE)
    ## 
    nset1 <- nrow(netObs)
    nset2 <- ncol(netObs)
    ## check if fitMC is available => 2 be added check the number of parameters
    npar <- getNumParamMC(nset1, nset2)
    stopifnot(d >= 1)
    stopifnot(npar < prod(dim(netObs)))
    ## Total number of latent traits
    nlatpar <- getNumParamMC(nset1, nset2, latentOnly = TRUE)
    latpar <- paste0("lat_", 1:nlatpar)
    lam <- paste0("lambda_", 1:d)
    ## 
    pars <- matrix(0, 3, npar)
    rownames(pars) <- c("start", "lower", "upper")
    colnames(pars) <- c(latpar, lam, "delta1", "delta2", "m")
    ## m
    pars[, 1L] <- c(1000 * stats::runif(1), -1000, 1000)
    ## delta and lambda are positive
    pars[1L, 2:(3 + d)] <- 10 * stats::runif(2 + d)
    pars[2L, 2:(3 + d)] <- 0
    pars[3L, 2:(3 + d)] <- 1000
    ## 
    pars[1L, (4 + d):npar] <- -1 + 2 * stats::runif(nlatpar)
    pars[2L, (4 + d):npar] <- -1
    pars[3L, (4 + d):npar] <- 1
    ## Total number of latent traits Get orthogonal basis (prevents from keeping
    ## computing them more than once).
    B1 <- getNullOne(nset1)
    B2 <- getNullOne(nset2)
    ## Simulated Annealing
    tmp <- GenSA::GenSA(par = pars[1L, ], fn = coreMC, lower = pars[2L, ], upper = pars[3L, 
        ], control = list(verbose = TRUE, max.time = mxt, smooth = FALSE), netObs = netObs, 
        nset1 = nset1, nset2 = nset2, d = d, B1 = B1, B2 = B2)
    # 
    params <- tidyParamMC(nset1, nset2, B1, B2, d, tmp$par)
    out <- alienPredict(-tmp$value, estimateMC(netObs, params), netObs = netObs, 
        params = params)
    out
}


## Compute the number of parameters.
getNumParamMC <- function(nset1, nset2, d = 1, latentOnly = FALSE) {
    ## Matching traits
    sum <- getNumLatMatParMC(nset1) + getNumLatMatParMC(nset2)
    ## Centrality traits
    sum <- sum + nset1 + nset2 - 2
    ## 
    if (!latentOnly) {
        # m, delta1, delta2 and lambda
        sum <- sum + d + 3
    }
    sum
}

## Get the number paramters needed to generate matching trait.
getNumLatMatParMC <- function(nset, d = 1) {
    # Given the constrains on vector's orthogonality, the dimension of the linear
    # subspace where a new vector is drawn decreases.
    d * nset - sum(1:d)
}

## tidy parameters and retunr the likelyhoog
coreMC <- function(netObs, nset1, nset2, d = 1, B1, B2, ...) {
    out <- NULL
    ## get parameters to be used in likelihoodMC
    tmp <- tidyParamMC(nset1, nset2, B1, B2, d, ...)
    ## compute the likelyhood
    out <- -likelihoodMC(netObs, tmp$M1, tmp$M2, tmp$c1, tmp$c2, tmp$Lambda, tmp$delta1, 
        tmp$delta2, tmp$m)
    # print(out)
    out
}


## tidy parameters
tidyParamMC <- function(nset1, nset2, B1, B2, d = 1, ...) {
    args <- list(...)[[1L]]  # a vector
    tmp <- list()
    ## 
    tmp$m <- args[1L]
    tmp$delta1 <- args[2L]
    tmp$delta2 <- args[3L]
    k <- 3 + d
    tmp$Lambda <- unlist(args[4:k])
    ## get c1 and c2
    tmp$c1 <- prodNorm(nset1, B1, unlist(args[k + (1:(nset1 - 1))]))
    tmp$c2 <- prodNorm(nset2, B2, unlist(args[k + nset1 - 1 + (1:(nset2 - 1))]))
    ## drop all parameters already used
    args <- args[-(1:(1 + d + nset1 + nset2))]
    ## get Matching vectors
    tmp$M1 <- getMiMC(B1, nset1, d, args)
    args <- args[-(1:getNumLatMatParMC(nset1))]
    tmp$M2 <- getMiMC(B2, nset2, d, args)
    ## 
    tmp
}

## get Matching paramters
getMiMC <- function(B, nset, d, args) {
    ## 
    M <- matrix(0, d, nset)
    ls_vec <- list()
    for (i in 1:d) {
        idi <- (i - 1) * nset - (i - 1)
        ls_vec[[i]] <- unlist(args[idi + 1:(nset - i)])
    }
    ## 
    Ba <- B
    ## 
    M[1L, ] <- prodNorm(nset, Ba, ls_vec[[1L]])
    ## 
    if (d >= 2) {
        ## keep track of vectors to which the next one should be orthogonal to
        K <- matrix(0, d, nset)
        K[1L, ] <- rep(1, nset)
        for (i in 2:d) {
            K[i, ] <- M[i - 1, ]
            Ba <- getNull(K)
            M[i, ] <- prodNorm(nset, Ba, ls_vec[[i]])
        }
    }
    M
}


## Compute likelihood
likelihoodMC <- function(netObs, M1, M2, c1, c2, Lambda, delta1, delta2, m) {
    #### test size ensures M1, M2 and Lambda use the same dimension d)
    stopifnot(nrow(M1) == length(Lambda))
    stopifnot(nrow(M2) == length(Lambda))
    ## 
    stopifnot(ncol(M1) == length(c1))
    stopifnot(ncol(M2) == length(c2))
    ## 
    stopifnot(nrow(netObs) == length(c1))
    stopifnot(ncol(netObs) == length(c2))
    ## 
    cent1 <- c1 * delta1
    cent2 <- c2 * delta2
    
    #### Get probabilities estimated by the model
    logLik <- 0
    ## logit values
    for (i in 1:nrow(netObs)) {
        for (j in 1:ncol(netObs)) {
            val <- 0
            for (k in 1:nrow(M1)) {
                tmp <- M1[k, i] - M2[k, j]
                val <- val + Lambda[k] * tmp * tmp
            }
            val <- val + cent1[i] + cent2[j] + m
            ## get the inverse logit
            val <- 1/(1 + exp(-val))
            ## 
            if (netObs[i, j]) {
                logLik <- logLik + log(val)
            } else {
                logLik <- logLik + log(1 - val)
            }
        }
    }
    
    logLik
}


## return a network of probabilities
estimateMC <- function(netObs, lsArgs) {
    out <- netObs * 0
    cent1 <- lsArgs$c1 * lsArgs$delta1
    cent2 <- lsArgs$c2 * lsArgs$delta2
    ## logit values
    for (i in 1:nrow(netObs)) {
        for (j in 1:ncol(netObs)) {
            val <- 0
            for (k in 1:nrow(lsArgs$M1)) {
                tmp <- lsArgs$M1[k, i] - lsArgs$M2[k, j]
                val <- val + lsArgs$Lambda[k] * tmp * tmp
            }
            val <- val + cent1[i] + cent2[j] + lsArgs$m
            ## get the inverse logit
            out[i, j] <- 1/(1 + exp(-val))
        }
    }
    out
}
