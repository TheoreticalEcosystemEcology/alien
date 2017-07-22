#include <RcppArmadillo.h>
// it pulls Rcpp in

//' @name sourceMC
//' @title sourceMC
//'
//' @author
//' Kevin Cazelles
//'
//' @description few linear algebra operations needed for fitMC.
//'
//' @param nbsp1 Number of species belonging to the first set.
//' @param nbsp2 Number of species belonging to the second set.
//'
//' @return Returns the eigen values associated to the vector and eigen values
//' transition matrix of the markov chain to be solved.
//'
//' @export
//
// [[Rcpp::depends(RcppArmadillo)]]
//
// [[Rcpp::export]]

Rcpp::List sourceMC(int nbsp1, int nbsp2) {
  // column vectors of 1
  arma::mat vecU1;
  arma::mat vecU2;
  vecU1.ones(1, nbsp1);
  vecU2.ones(1, nbsp2);
  //
  arma::mat B1 = sqrt(nbsp1)*null(vecU1);
  arma::mat B2 = sqrt(nbsp2)*null(vecU2);
  //
  arma::mat S1 = arma::randu<arma::mat>(nbsp1-1,1);
  arma::mat S2 = arma::randu<arma::mat>(nbsp2-1,1);
  S1 = B1*normalise(S1);//B1*normalizs(S1);
  S2 = B2*normalise(S2);
  //
  return Rcpp::List::create(
    Rcpp::Named("B1") = B1,
    Rcpp::Named("B2") = B2,
    Rcpp::Named("S1") = S1,
    Rcpp::Named("S2") = S2
  );
}
