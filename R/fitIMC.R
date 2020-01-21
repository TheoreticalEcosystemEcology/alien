#' @name fitIMC
#'
#' @title Estimation of interaction probabilities using the indirect matching centrality approach.
#'
#' @description Esimation of iteractions probability using the indirect matching centrality
#' approach as described in Rohr (2014) and Rohr (2016).
#'
#' @param data an object of the class alienData.
#' @param d dimensionnality.
#' @param verbose Logical. Should extra information be reported on progress?
#' @param control list that can be used to control the behavior of the algorithm including the value for initial temperature and maximum runiing time(see [GenSA::GenSA()] for more details).
#' @author
#' Kevin Cazelles
#'
#' @details
#' `fitIMC` implements the indirect matching-centrality method as described in
#' Rohr 2014 and Rohr 2016. Briefly, the method uses latent traits to fit
#' interactions. The first step requires the definition of two sets of interacting
#' species: set1 and set2 (respectively of size `nset1` and `nset2`).
#' The first category of latent trait are the matching traits. (to be developped)
#' The second category include the centrality terms. (to be developped).
#' Latents traits are buitl under certain topological constraints:
#' * 1- all vectors belongs to the orthogonal basis of the unit vector.
#' * 2- all matching vector of a particluar set of specues are orthogonal to each other.
#'
#'
#' @return
#' An object of class alienFit.
#'
#' @references
#' Rohr, R. P. & Bascompte, J. Components of Phylogenetic Signal in Antagonistic and Mutualistic Networks. Am. Nat. 184, 556--564 (2014).
#'
#' Rohr, R. P., Naisbit, R. E., Mazza, C. & Bersier, L.-F. Matching-centrality decomposition and the forecasting of new links in networks. Proc. R. Soc. B Biol. Sci. 283, 20152702 (2016).
#'
#' @export


fitIMC <- function(data, d = 1, verbose = TRUE, control = list()) {
  
  # General check
  stopifnot(d >= 1)
  stopifnot(class(data) == "alienData")

  netObs <- getAdjacencyMatrix(data, binary = TRUE, bipartite = TRUE)
  # number of species in the two sets of species
  nset1 <- nrow(netObs)
  nset2 <- ncol(netObs)
  nsp <- nset1 + nset2
  if (verbose) {
      cat("* set1 has", nset1, "elements \n* set2 has", nset2, "elements \n")
  }
  # Parameters 
  ## Centrality latent traits: 1 per species 
  ## IIRC -2 => cause we can set 1 value aand adjust the other values
  nbc <- nsp - 2
  ## Matching latent traits
  ## Given the constrains on vector's orthogonality, dimension of the linear 
  ## subspace where a new vector is drawn decreases.
  nbm <- d * nsp - 2 * sum(seq_len(d))
  ## number of 'fixed' parameters (d lambda(s), delta1, delta2 and m), 
  ## see eq (2.1) in 2016 paper for more details
  npr <- 3 + d
  ## total number of paramters 
  npar <- nbc + nbm + npr
  ## overfit check
  stopifnot(npar < prod(dim(netObs)))
  if (verbose) cat("*", npar, "parameters to be fitted\n")
  ## parameters order: m, delta1 (>0), delta2(>0), d lambda values (>0), 
  ## latent traits for centrality and matching
  ## lower boundary of paramter values
  low_bound <- c(-10, rep(-10, 2+d), rep(-10, nbm + nbc))
  ## upper boundary
  upp_bound <- c(10, rep(10, 2+d), rep(10, nbm + nbc))
  ## Get orthogonal basis (needs to be calculated only once).
  B1 <- getNullOne(nset1)
  B2 <- getNullOne(nset2)
  ## Simulated Annealing
  tmp <- GenSA(lower = low_bound, upper = upp_bound, fn = coreMC, 
    netObs = netObs, nset1 = nset1, nset2 = nset2, B1 = B1, B2 = B2, d = d,
    control = control)
  #
  params <- tidyParamMC(nset1, nset2, B1, B2, d, tmp$par)
  out <- IMCPredict(-tmp$value, estimateMC(netObs, params), netObs = netObs,
      params = params)
    print(out$logLik)
    print(out$methodsSpecific$params[c("m", "delta1", "delta2", "Lambda")])
  
  # Standardize results
  res <- out$netEstim
  
  # Format results attributes
  baseAttr <- attributes(res)
  
  # Define object class
  attributes(res) <- list(dim = baseAttr$dim, dimnames = baseAttr$dimnames,
                          model = out$methodsSpecific$params, adjMat = netObs, 
                          LL = tmp$value)
  class(res) <- "alienFit"
  res
}



## tidy parameters and return the likelihood
coreMC <- function(netObs, nset1, nset2, B1, B2, d, ...) {
    ## parsing parameters (values passed as ...)
    tmp <- tidyParamMC(nset1, nset2, B1, B2, d, ...)
    
    # compute -log(likelihood)
    likelihoodMC(netObs, tmp$M1, tmp$M2, tmp$c1, tmp$c2, tmp$Lambda, 
        tmp$delta1, tmp$delta2, tmp$m)
}


## tidy parameters
tidyParamMC <- function(nset1, nset2, B1, B2, d, ...) {
    args <- list(...)[[1L]] # vector of latent parameter
    tmp <- list()
    ## number of parameter 
    nsp <- nset1 + nset2
    nbc <- nsp - 2
    nbm <- d * nsp - 2 * sum(seq_len(d))
    npr <- 3 + d
    ##-- 'fixed parameters'
    tmp$m <- args[1L]
    tmp$delta1 <- args[2L]
    tmp$delta2 <- args[3L]
    tmp$Lambda <- args[4:npr]
    ##-- get c1 and c2 using the nbc centrality traits 
    args2 <- args[npr + seq_len(nbc)]
    tmp$c1 <- prodNorm(nset1, B1, args2[seq_len(nset1 - 1)])
    tmp$c2 <- prodNorm(nset2, B2, tail(args2, nset2 - 1))
    # ## get Matching vectors using the ncm macting traits
    args3 <- tail(args, nbm)
    tmp$M1 <- getMiMC(B1, nset1, d, args3[seq_len(d * nset1 - sum(seq_len(d)))])
    tmp$M2 <- getMiMC(B2, nset2, d, tail(args3, d * nset2 - sum(seq_len(d))))
    ##
    tmp
}


## get Matching parameters
getMiMC <- function(B, nset, d, args) {
    ##
    M <- matrix(0, d, nset)
    ls_vec <- list()
    k <- 0
    for (i in seq_len(d)) {
        inc <- nset - i
        ls_vec[[i]] <- args[k + (1:inc)]
        k <- k + inc
    }
    ##
    Ba <- B 
    M[1L, ] <- prodNorm(nset, Ba, ls_vec[[1L]])
    ##
    if (d >= 2) {
        ## keep track of vectors to which the next one should be orthogonal to
        K <- matrix(0, d, nset)
        K[1L, ] <- rep(1, nset)
        for (i in 2:d) {
            K[i, ] <- M[i - 1, ]
            Ba <- getNull(K[seq_len(i), ])
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
    stopifnot(ncol(M1) == length(c1))
    stopifnot(ncol(M2) == length(c2))
    stopifnot(nrow(netObs) == length(c1))
    stopifnot(ncol(netObs) == length(c2))
    ##
    cent1 <- c1 * delta1
    cent2 <- c2 * delta2

    ## We look for max(likelihood) so max(log(likelihood)) 
    ## so we look for min(-log(likelihood)) and that's what we use cause
    # GenSA minimizes the objectif function.
    -likelihoodMC_core(netObs, M1, M2, cent1, cent2, Lambda, m) 
}

## return a network of probabilities
estimateMC <- function(netObs, lsArgs) {
    out <- netObs * 0
    cent1 <- lsArgs$c1 * lsArgs$delta1
    cent2 <- lsArgs$c2 * lsArgs$delta2
    ## logit values
    for (i in seq_len(nrow(netObs))) {
        for (j in seq_len(ncol(netObs))) {
            val <- 0
            for (k in seq_len(nrow(lsArgs$M1))) {
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
