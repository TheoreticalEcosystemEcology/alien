

# OBJECT: phy ================================================

if (!is.null(phy)) {
  
  ## not sure if we should require(ape) at this point. Phylo objects are defined in package ape.
  #require(ape)
  
  ### Check that phy is of class phylo
  if (!inherits(phy, "phylo")) 
  stop("'", deparse(substitute(phy)), "' not of class 'phylo'")

  ### Check that phy is rooted
  if (!is.rooted(phy)) 
  stop("'", deparse(substitute(phy)), "' the tree is not rooted")

  ### Check that there are no duplicated tips
  if (any(duplicated(phy$tip.label))) 
  stop("Duplicate tip labels present in phylogeny")
  
  ### Warn if the number of species in the phylogeny differs from num species in the interaction matrix
  ### I'm masking this out as it would need to be checked first - which other dependencies provide the number of species?
  #if (!is.null(traitSp))
  #nsps<-length(unique(traitSp$idSp))
  
  #if (warn) {nsps=99
  #  if (abs(length(phy$tip.label)-nsps) > 0 ) 
  #    warning("The phylogeny and other data differ in the number of species")
}
  
  
  