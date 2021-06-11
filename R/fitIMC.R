#' @name fitIMC
#'
#' @title Fit indirect matching centrality model
#'
#' @description This method estimate matching and centrality latent traits to model the interactions in an adjacency matrix. 
#'
#' @param data An object of the class \code{\link{alienData}}.
#' @param d Numeric. The dimension of the latent traits. Default is 2. 
#' @param verbose Logical. Whether information on the progress of the analysis is reported in the console.
#' @param control List passed to \code{\link[GenSA]{GenSA}} to control the behavior of the algorithm.
#'
#' @author
#' Kevin Cazelles, Dominique Gravel and F. Guillaume Blanchet
#'
#' @details
#' 
#'  As can be hinted by the name of the method, there are two types of latent traits.  : (1) matching latent traits that are designed to quantify the strength of the interaction between two species and (2) centrality latent traits, which quantify the number of relations a species has with other species. Mathematically, these latent traits (both matching and centrality) are all orthonormal with each other, within and outside of their category.
#' 
#' When deciding on the dimension of the lantent traits, aside from technical issues (i.e. the number of parameters to estimate and the size of the data) it is important to also consider what is gained (or loss) from increasing (or decreasing) the dimension of the latent traits. The default was set to 2 because it is often of interest to study latent traits in pairs in an ordination-type graphic. 
#'
#' @return
#' An object with a class alienFit and a class fitIMC.
#'
#' @references
#' Rohr, R. P. & Bascompte, J. (2014) Components of Phylogenetic Signal in Antagonistic and Mutualistic Networks. Am. Nat. 184, 556--564.
#'
#' Rohr, R. P., Naisbit, R. E., Mazza, C. & Bersier, L.-F. (2016) Matching-centrality decomposition and the forecasting of new links in networks. Proc. R. Soc. B Biol. Sci. 283, 20152702.
#'
#' @importFrom GenSA GenSA
#'
#' @export
fitIMC <- function(data, d = 2, verbose = TRUE, control = list()){

  # General check
  stopifnot(d >= 1)
  stopifnot(class(data) == "alienData")

  # Adjacency matrix
  adjMat <- data$adjMat

  # Check if the data is presence-absence
  adjMatUnique <- unique(as.vector(adjMat))
  if(any(is.na(adjMatUnique))){
    # Remove NAs for check
    adjMatUnique <- adjMatUnique[-which(is.na(adjMatUnique))]
  }
  
  if(!all(adjMatUnique %in% c(0,1))){
    stop("'fitIMC is only developped for presence-absence data'")
  }

  # Number of species per sets and in total (predator and prey together)
  nset1 <- nrow(adjMat)
  nset2 <- ncol(adjMat)

  nsp <- nset1 + nset2

  # Verbose
  if (verbose) {
    print(paste(nset1, "'From' species -",nset2, "'To' species"))
  }

  # Parameters
  ## Centrality latent traits: 1 per species
  ## IIRC - 2 => because we can set 1 value and adjust the other values
  nbc <- nsp - 2

  ## Matching latent traits
  ## Given the constrains on vector's orthogonality, dimension of the linear
  ## subspace where a new vector is drawn decreases.
  nbm <- d * nsp - 2 * sum(seq_len(d))

  ## number of 'fixed' parameters (d lambda(s), delta1, delta2 and m),
  ## Equation 2.1 in Rohr et al. (2016) for more details
  npr <- 3 + d

  ## Total number of paramters
  npar <- nbc + nbm + npr

  ## overfit check
  stopifnot(npar < prod(dim(adjMat)))

  # If verbose ON
  if (verbose){
    print(paste(npar, "parameters need to be fitted"))
  }

  ## parameters order: m, delta1 (>0), delta2(>0), d lambda values (>0),
  ## latent traits for centrality and matching

  ## lower boundary of paramter values
  low_bound <- c(-2.5, rep(-2.5, 2+d), rep(-2.5, nbm + nbc)) 

  ## upper boundary
  upp_bound <- c(2.5, rep(2.5, 2+d), rep(2.5, nbm + nbc))

  ## Get orthogonal basis (needs to be calculated only once).
  B1 <- getNullOne(nset1)
  B2 <- getNullOne(nset2)

  ## Simulated Annealing
  genSARes <- GenSA::GenSA(lower = low_bound, upper = upp_bound,
                    fn = coreMC, adjMat = adjMat, nset1 = nset1,
                    nset2 = nset2, B1 = B1, B2 = B2, d = d,
                    control = control)

  #
  params <- tidyParamMC(nset1, nset2, B1, B2, d, genSARes$par)
  out <- IMCPredict(-genSARes$value, estimateMC(adjMat, params),
                    adjMat = adjMat, params = params)

  # Standardize results
  res <- out$netEstim

  # Format results attributes
  baseAttr <- attributes(res)

  # Define object class
  attributes(res) <- list(dim = baseAttr$dim,
                          dimnames = baseAttr$dimnames,
                          model = out$methodsSpecific$params,
                          alienData = data,
                          logLike = -genSARes$value)

  class(res) <- c("alienFit", "fitIMC")
  res
}

## tidy parameters and return the likelihood
coreMC <- function(adjMat, nset1, nset2, B1, B2, d, ...) {
    ## parsing parameters (values passed as ...)
    tmp <- tidyParamMC(nset1, nset2, B1, B2, d, ...)

    # compute -log(likelihood)
    likelihoodMC(adjMat, tmp$M1, tmp$M2, tmp$c1, tmp$c2, tmp$Lambda,
        tmp$delta1, tmp$delta2, tmp$m)
}


## tidy parameters
#' @importFrom utils tail
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
        ls_vec[[i]] <- args[k + seq_len(inc)]
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
likelihoodMC <- function(adjMat, M1, M2, c1, c2, Lambda, delta1, delta2, m) {
    #### test size ensures M1, M2 and Lambda use the same dimension d)
    stopifnot(nrow(M1) == length(Lambda))
    stopifnot(nrow(M2) == length(Lambda))
    stopifnot(ncol(M1) == length(c1))
    stopifnot(ncol(M2) == length(c2))
    stopifnot(nrow(adjMat) == length(c1))
    stopifnot(ncol(adjMat) == length(c2))
    ##
    cent1 <- c1 * delta1
    cent2 <- c2 * delta2

    ## We look for max(likelihood) so max(log(likelihood))
    ## so we look for min(-log(likelihood)) and that's what we use cause
    # GenSA minimizes the objectif function.
    -likelihoodMC_core(adjMat, M1, M2, cent1, cent2, Lambda, m)
}

## return a network of probabilities
estimateMC <- function(adjMat, lsArgs) {
    out <- adjMat * 0
    cent1 <- lsArgs$c1 * lsArgs$delta1
    cent2 <- lsArgs$c2 * lsArgs$delta2
    ## logit values
    for (i in seq_len(nrow(adjMat))) {
        for (j in seq_len(ncol(adjMat))) {
            out[i, j] <- interaction_proba(lsArgs$M1[, i], lsArgs$M2[, j],
                cent1[i], cent2[j], lsArgs$Lambda, lsArgs$m)
        }
    }
    out
}

IMCPredict <- function(logLik, netEstim, ...) {
  out <- list()
  out$logLik <- logLik
  ##--
  stopifnot(all(netEstim <= 1) & all(netEstim >= 0))
  out$netEstim <- netEstim
  out$connec <- list(expectation = sum(netEstim),
                     variance = sum(netEstim * (1 - netEstim)))
  ##--
  out$methodsSpecific <- list(...)
  ##--
  class(out) <- "IMCPredict"
  out
}
