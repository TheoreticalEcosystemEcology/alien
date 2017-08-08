// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
//' @name getNull
//' @title getNull
//'
//' @author
//' Kevin Cazelles
//'
//' @description A wrapper around the \code{null} function of Armadillo library
//' that finds the orthonormal basis of the null space of matrix A. Note that
//' this wrapper do not allow the user to use any other parameters the
//' original function can.
//'
//' @param A a matrix.
//' @param B a matrix (it should actually is a orthogonal basis).
//' @param V a vector.
//' @param nbsp an integer (from an ecological standpoint, it denote a number
//' of species).
//'
//' @return Returns the null basis.
//'
//' @importFrom Rcpp evalCpp
//'
// [[Rcpp::export]]

arma::mat getNull(arma::mat A) {
  return arma::null(A);
}

//' @describeIn getNull Special case of \code{getNull} for which the orthonormal
//' basis of the unit vector is required.
// [[Rcpp::export]]
arma::mat getNullOne(int nbsp) {
  // column vectors of 1
  arma::mat vecU1 = arma::ones<arma::mat>(1, nbsp);
  return arma::null(vecU1);
}

//' @describeIn getNull A repeated matrix produc in \code{fitMC} that requires
//' to normalise one vector.
// [[Rcpp::export]]
arma::vec prodNorm(int nbsp, arma::mat B, arma::vec V) {
  return sqrt(nbsp)*B*arma::normalise(V);
}
