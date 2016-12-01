#' @name niche.prob
#' @aliases integrated.model
#' @aliases niche.model
#' @aliases neutral.model
#'
#' @title Bayesian models for characterizing trait-based species interactions.
#'
#' @description This models are described in Bartomeus et al. 2016 (Functional Ecology) and are based
#' on Williams et al. 2000 models. They are intended to be used with fit.it function. The integrated model
#' considers both neutral and niche constraints, while the neutral and niche models only consider its respective
#' components.
#'
#' @param pars parameters obtained from fit.niche.prob function or a vector of the form c(a0 = 0, a1 = 0, b0 = 0, b1 = 0) with known parameters.
#' @param Tlevel1 Vector of trait values of the first interaction partner.
#' @param Tlevel2 Vector of trait values of the second interaction partner.
#' @param mean_Tlevel1 Mean of trait values of the first interaction partner. Can be weighted or not,
#'  and can use independent information on the trait distribution to be calculated.
#' @param sd_Tlevel1 Standard deviation of trait values of the first interaction partner. Can be weighted or not,
#'  and can use independent information on the trait distribution to be calculated.
#'
#' @author
#' Dominique Gravel
#'
#' @references
#'Williams, R.J., Anandanadesan, A. and Purves, D. (2010) The probabilistic niche model reveals the niche structure and role of body size in a complex food web. PloS one, 5, e12092.
#'Williams, R.J. and Martinez, N.D. (2000) Simple rules yield complex food webs. Nature, 404, 180–183.
#'
#' @rdname niche.prob
#' @export
integrated.model = function(pars, Tlevel1, Tlevel2, mean_Tlevel1, sd_Tlevel1) {
    a0 = pars[1]
    a1 = pars[2]
    b0 = pars[3]
    b1 = pars[4]
    # Optimum and range
    o = a0 + a1 * Tlevel2
    r = b0 + b1 * Tlevel2
    # Compute the conditional
    pLM = exp(-(o - Tlevel1)^2/2/r^2)
    # Compute the marginal
    pM = dnorm(x = Tlevel1, mean = mean_Tlevel1, sd = sd_Tlevel1)
    # Integrate the denominator
    pL = r/(r^2 + sd_Tlevel1^2)^0.5 * exp(-(o - mean_Tlevel1)^2/2/(r^2 + sd_Tlevel1^2))
    # Compute the posterior probability
    pML = pLM * pM/pL
    pML[pML <= 0] = .Machine$double.xmin  # Control to avoid computing issues
    return(-sum(log(pML)))
}
#' @export
neutral.model = function(pars = NULL, Tlevel1, Tlevel2, mean_Tlevel1, sd_Tlevel1) {
    if (!is.null(pars)) 
        warning("The neutral_model function does not use the content of the argument pars")
    # Compute the conditional
    pLM = 1
    # Compute the marginal
    pM = dnorm(x = Tlevel1, mean = mean_Tlevel1, sd = sd_Tlevel1)
    # Integrate the denominator
    pL = 1
    # Compute the posterior probability
    pML = pLM * pM/pL
    pML[pML <= 0] = .Machine$double.xmin  # Control to avoid computing issues
    return(-sum(log(pML)))
}
#' @export
niche.model = function(pars, Tlevel1, Tlevel2, mean_Tlevel1, sd_Tlevel1) {
    a0 = pars[1]
    a1 = pars[2]
    b0 = pars[3]
    b1 = pars[4]
    # Optimum and range
    o = a0 + a1 * Tlevel2  #Here we can try polimonial functions
    r = b0 + b1 * Tlevel2
    # Compute the conditional
    pLM = exp(-(o - Tlevel1)^2/2/r^2)
    # Compute the marginal
    pM = 1/(max(Tlevel1) - min(Tlevel1))
    # Integrate the denominator
    pL = r/sqrt(pi)
    # Compute the posterior probability
    pML = pLM * pM/pL
    pML[pML <= 0] = .Machine$double.xmin  # Control to avoid computing issues
    return(-sum(log(pML)))
}


