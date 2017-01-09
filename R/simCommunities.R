# #' @name simCommunities #' @aliases simulated communities #' #' @title
# Simulating communities along an environmental gradient for a set of interacting
# species.  #' #' @description A helper functional for generating a matrix of
# presence absence for a set of interacting species.  #' @param size_x The number
# of species in the lower level (eg. plants) #' @param size_y The number of
# species in the upper level (eg. pollinators) #' @param traits_x Vector of trait
# values for species in the lower level.  #' @param traits_y Vector of trait
# values for species in the lower level.  #' @param beta_sigma Variance among
# species in trait-matching slope.  #' @param alpha_sigma Variance among species
# in trait-matching intercept.  #' @param replicates The number of times the
# network was sampled. For example, Replicate = 1 will yield I*J data points #'
# @param type string binary or quantitative network?  #' @author #' Kevin
# Cazelles #' #' @export simCommunities <- function(nbsp, env, trait1, trait2 =
# NULL, trait3 = NULL, niche = NULL, metaweb = NULL, method='') { return(dat) }
