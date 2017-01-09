#include <Rcpp.h>
using namespace Rcpp;

//' @name webFromNicheModel
//'
//' @title Generate a web using the niche model.
//'
//' @description
//' This function is an implementation of the niche model which generates
//' food webs based on 1- a number of species, 2- a niche axis and 3- a value of
//' comnectance.
//'
//' @author
//' Kevin Cazelles
//'
//' @param nbsp an integer giving the number of species considered.
//' @param connec a real positive between 0 and .5 indicating the connectance
//' of the network to be generated.
//' @param connec_all logical. If TRUE, then all species in the network have a
//' least one prey (but the niche with the lowest niche value).
//' @param unbias logical. If TRUE, then the first species may not be a basal species.
//' @param niche a vector real positive between 0 and 1 standing for the niche axis.
//' Default is set to \code{NULL}, in such case the niche axis is automatically generated.
//'
//' @return A logical matrix describing pairwise interactions. A given line
//' describe the diet of a given species while a column decribes the set of
//' predator accociated to a particular species.
//'
//' @details
//' Three remarks. First, according to Williams and Martinez (2000),
//' the species with the lowest niche value is considered as a basal and thus
//' has no trait. This introduces a slight bias (\emph{e.g} the
//' expected connectance is lower than the expected values \code{connec}). Second,
//' forcing all the species to be connected introduces another biais (connectance)
//' tends to be more connected than expected.
//' Third, if one uses its own customed niche axis, values should be between 0 and
//' and 1 and the expected connectance (\code{connec}) can vary significantly if the
//' distribution of niche values differ from the uniform distribution used in
//' Williams and Martinez (2000)
//'
//' @references
//' Williams, R.J. and Martinez, N.D. (2000) Simple rules yield complex food webs.
//' \emph{Nature}, 404, 180–183.
//'
//' @export
//'
// [[Rcpp::export]]
LogicalMatrix webFromNicheModel(int nbsp, double connec, bool connect_all = false,
  bool unbias = false, Nullable<NumericVector> niche = R_NilValue){
  if ( (connec < 0) || (connec > 0.5) ) {
    stop("Inadmissible value for connectance");
  }
  LogicalMatrix metaweb(nbsp, nbsp);
  NumericVector vec_tmp(nbsp);
  double c, r, rg1, rg2, beta;
  int i, j, k, l, m, count;
  //
  if (unbias) {
    m = 0;
  } else {
    m = 1;
  }
  //
  if (niche.isNotNull()) {
    NumericVector vec_tmp0(niche);
    vec_tmp = clone(vec_tmp0);
    if (vec_tmp.size() != nbsp){
      stop("Niche axis dimension is not equal to the number of species.");
    }
    for (i = 1; i<nbsp; i++) {
      if ( (vec_tmp[i] < 0) || (vec_tmp[i] > 1) ){
        stop("Inadmissible value for the niche axis");
      }
    }
  } else {
    vec_tmp = runif(nbsp, 0, 1);
  }
  //
  NumericVector niche_sorted = clone(vec_tmp);
  // Sorting the niche axis (using standard library)
  std::sort(niche_sorted.begin(), niche_sorted.end());
  //
	beta = .5/connec - 1;
  count = 0;
  k = 0;
  while (k == 0) {
    if (count>1000000){
      stop("1000000 unsucessful attempts.");
    }
    // if unbias, then the first species is a basal species
	  for (i = m; i<nbsp; i++) {
		  r = rbeta(1, 1, beta)[0]*niche_sorted[i];
		  c = runif(1, .5*r, niche_sorted[i])[0];
		  rg1 = c-.5*r;
		  rg2 = c+.5*r;
      l = 0;
		  for (j = 0; j<nbsp; j++) {
        metaweb(i, j) = FALSE;
	      if ( (niche_sorted[j]>rg1) && (niche_sorted[j]<rg2) ){
		      metaweb(i, j) = TRUE;
          l++;
		    }
      }
      // If a species has no interaction, we break the loop if required
      if (!l && connect_all) {
        k--; break;
      }
	  }
    count++;
    k++;
  }
  //
  return metaweb;
}
