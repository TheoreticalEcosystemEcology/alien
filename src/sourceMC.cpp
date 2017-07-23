// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
//' @name sourceMC
//' @title sourceMC
//'
//' @author
//' Kevin Cazelles
//'
//' @description Linear algebra operations needed for fitMC.
//'
//' @param nbsp1 Number of species belonging to the first set.
//' @param nbsp2 Number of species belonging to the second set.
//'
//' @return Returns the eigen values associated to the vector and eigen values
//' transition matrix of the markov chain to be solved.
//'
//' @export
//
// [[Rcpp::export]]

Rcpp::List sourceMC(int nbsp1, int nbsp2) {
  // column vectors of 1
  arma::mat vecU1 = arma::ones<arma::mat>(1,nbsp1);
  arma::mat vecU2 = arma::ones<arma::mat>(1,nbsp2);
  //
  arma::mat B1 = sqrt(nbsp1)*arma::null(vecU1);
  arma::mat B2 = sqrt(nbsp2)*arma::null(vecU2);
  //
  arma::mat S1 = arma::randu<arma::mat>(nbsp1-1,1);
  arma::mat S2 = arma::randu<arma::mat>(nbsp2-1,1);
  S1 = B1*arma::normalise(S1);//B1*normalizs(S1);
  S2 = B2*arma::normalise(S2);
  //
  return Rcpp::List::create(
    Rcpp::Named("B1") = B1,
    Rcpp::Named("B2") = B2,
    Rcpp::Named("S1") = S1,
    Rcpp::Named("S2") = S2
  );
}
