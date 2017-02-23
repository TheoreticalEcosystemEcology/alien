#include <Rcpp.h>
using namespace Rcpp;

//' @name webFromTraitMatching
//'
//' @title Generate a web using a trait matching approach.
//'
//' @description
//' This function generates a food webs based on 1- a trait matching between
//' two traits, 2- a generality trait and 3- an efficiency trait.
//'
//' @author
//' Kevin Cazelles
//'
//' @param nbsp an integer giving the number of species considered.
//' @param trait1 an numeric vector giving the first trait to be matched with the second one. If NULL (the default values) then values are drawn from an uniform distribution.
//' @param trait2 an numeric vector giving the second trait to be matched with the first one. if NULL (the default values) then values are drawn from an uniform distribution.
//' @param trait3 an numeric vector giving the first trait to be matched if NULL (the default values) then values are drawn from an uniform distribution.
//' @param trait4 an numeric vector (between 0 and 1) giving the first trait to be matched if NULL (the default values) then values are drawn from an uniform distribution.
//'
//' @return A logical matrix describing pairwise interactions. A given line
//' describe the diet of a given species while a column decribes the set of
//' predator accociated to a particular species.
//'
//' @details
//' For any couple of species (i,j) the probability that i interact with
//' (\emph{e.g.} i feeds upon j) is as follows:
//' \deqn{p(M_{i,j}=1) = trait4_i e^{-\left(-\frac{trait1_i-trait2_j}{trait3_i}\right)^2}}{%
//'       p(M_{i,j}=1) = trait4[i] exp(-((trait1[i]-trait2[j])/trait3[i])^2)}
//' The length of all trait vectors must equal the number of species otherwise
//' an error is returned.
//' This apporach has been used with feeding traits in a version revisited of
//' niche model implemented (see \link[pkg:alienR]{webFromNicheModel}) by Williams
//' \emph{et al.} (2010) where \code{trait4} is common to all species.
//'
//' @references
//' Williams, R. J., Anandanadesan, A. & Purves, D. (2010) The probabilistic
//' niche model reveals the niche structure and role of body size in a complex
//' food web. \emph{PLoS One 5}.
//'
NumericVector checkTrait2(int nbsp, Nullable<NumericVector> vec = R_NilValue){
  NumericVector out(nbsp);
  if (vec.isNotNull()){
    NumericVector tmp(vec);
    if (tmp.size() != nbsp){
      stop("The size of the trait vector is not equal to the number of species.");
    } else {
      out = clone(tmp);
    }
  } else {
    out = runif(nbsp, 0 , 1);
  }
  return out;
}
//'
//' @export
// [[Rcpp::export]]
LogicalMatrix webFromTraitMatching(int nbsp, Nullable<NumericVector> trait1 = R_NilValue,
Nullable<NumericVector> trait2 = R_NilValue, Nullable<NumericVector> trait3 = R_NilValue,
Nullable<NumericVector> trait4 = R_NilValue){
  LogicalMatrix metaweb(nbsp, nbsp);
  NumericVector tr1(nbsp), tr2(nbsp), tr3(nbsp), tr4(nbsp);
  double val, tmp;
  int i, j;
  // Traits checking
  tr1 = checkTrait2(nbsp, trait1);
  tr2 = checkTrait2(nbsp, trait2);
  tr3 = checkTrait2(nbsp, trait3);
  tr4 = checkTrait2(nbsp, trait4);
  //
  for (i=0; i<nbsp; i++){
    if (tr4[i]<0 || tr4[i]>1) stop("Inadmissible value for trait4");
  }
  // Interactions
  for (i=0; i<nbsp; i++){
    for (j=0; j<nbsp; j++){
      metaweb(i, j) = false;
      val = (tr1[i]-tr2[j])/tr3[i];
      tmp = tr4[i]*exp(-(val*val));
      if (tmp < runif(1)[0]) metaweb(i, j) = true;
    }
  }

  return metaweb;
}
