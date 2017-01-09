#include <Rcpp.h>
using namespace Rcpp;

//' @name traitsBasedJointCooc
//'
//' @title Traits-based joint co-occurrence.
//'
//' @description
//' This function generates occurrence probabilities of a set of interacting
//' given for a given set of environnmental values.
//'
//' @author
//' Kevin Cazelles
//'
//' @param env a numerical vector including the environmental values.
//' @param metaweb a numerical matrix depicting the biotic interactions.
//' @param alpha a real positive number determining the importance of interactions: the
//' higher it is the more interactions impact occurrence values.
//' @param trait1 a numerical vector giving the environmental optima of species.
//' @param trait2 a optional numerical vector giving the probabilities of presence at the optimal value
//' @param trait3 a optional numerical vector giving the generalities of species.
//'
//' @return
//' A numeric gradient x species matrix of presence probabilities of the
//' interacting species associated to the environmental values.

NumericVector checkTrait(int nbsp, Nullable<NumericVector> vec = R_NilValue){
  NumericVector out(nbsp);
  if (vec.isNotNull()){
    NumericVector tmp(vec);
    if (tmp.size() != nbsp){
      stop("The size of the trait vector is not equal to the number of species.");
    } else {
      out = clone(tmp);
    }
  } else {
    std::fill(out.begin(), out.end(), 1);
  }
  return out;
}

double extinter(double inter, double prob0, double mn, double mx, double shape){
	double tmp, val;
  val = 0;
  tmp = 1/(mx-mn);
  val = 1/(tmp + (1/(prob0-mn)-tmp)*exp(shape*inter));
	val += mn;
	return val;
}

//' @export
// [[Rcpp::export]]
NumericMatrix traitsBasedJointCooc(NumericVector env, NumericMatrix metaweb, double alpha,
  NumericVector trait1, Nullable<NumericVector> trait2 = R_NilValue,
  Nullable<NumericVector> trait3 = R_NilValue){
  if (alpha<0) {
    stop("Inadmissible values for alpha.");
  }
  int i, j, k, nbsp;
  double tmp, score;
  nbsp = metaweb.ncol();
  NumericVector vec_tmp(nbsp);
  if (trait1.size() != nbsp) {
    stop("The size of the trait vector is not equal to the number of species.");
  }
  NumericVector tr2 = checkTrait(nbsp, trait2);
  NumericVector tr3 = checkTrait(nbsp, trait3);
  for (i=0; i<env.size(); i++){
    if (tr2[i]<0) stop("Inadmissible values for trait2.");
    if (tr3[i]<0 || tr3[i]>1) stop("Inadmissible values for trait3.");
  }
  NumericMatrix out(nbsp, env.size());

  for (i=0; i<env.size(); i++){
    // Occurrence based on traits + environmental value
    for (j=0; j<nbsp; j++){
      tmp = env[i]-trait1[j];
      vec_tmp[j] = tr3[j]*exp(-(tmp*tmp)/tr2[j]);
    }
    // Add the effect of biotic interactions
    for (j=0; j<nbsp; j++){
      // Compute the score of interactions for species j
      score = 0;
      for (k=0; k<nbsp; k++){
        score += metaweb(j,k)*vec_tmp[k];
        score -= metaweb(k,j)*vec_tmp[k];
      }
      out(j,i) = extinter(score, vec_tmp[j], 0, 1, alpha);
    }
  }
  return out;
}
