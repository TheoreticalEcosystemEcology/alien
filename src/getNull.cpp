// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
//' @name getNull
//' @title getNull
//'
//' @author
//' Kevin Cazelles
//'
//' @description Wrapper around the `null` function of Armadillo library
//' that finds the orthonormal basis of the null space of matrix A. Note that
//' this wrapper do not allow the user to use any other parameters the
//' original function can.
//'
//' @param A a matrix.
//' @param B a matrix (an orthogonal basis of A).
//' @param V a vector.
//' @param nbsp an integer (from an ecological standpoint, a number
//' of species).
//'
//' @return The null basis of `A`.
//'
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]

arma::mat getNull(arma::mat A) {
  return arma::null(A);
}

//' @describeIn getNull Special case of `getNull` for which the orthonormal
//' basis of the unit vector is required.
// [[Rcpp::export]]
arma::mat getNullOne(int nbsp) {
  // column vectors of 1
  arma::mat vecU1 = arma::ones<arma::mat>(1, nbsp);
  return arma::null(vecU1);
}

//' @describeIn getNull Matrix product that requires to normalise one vector 
//' (see [fitIMC()]).
// [[Rcpp::export]]
arma::vec prodNorm(int nbsp, arma::mat B, arma::vec V) {
  return sqrt(nbsp)*B*arma::normalise(V);
}

// [[Rcpp::export]]
double interaction_proba(NumericVector M1_i, NumericVector M2_j, double cent1_i, double cent2_j, NumericVector Lambda, double m) {
  int k;
  double val = cent1_i + cent2_j + m;
  double tmp;
  for (k=0; k<M1_i.size(); k++) {
      tmp = M1_i(k) - M2_j(k);
      val -= Lambda(k) * tmp * tmp;
  }
  return 1/(1 + exp(-val));
}

// [[Rcpp::export]]
double likelihoodMC_core(NumericMatrix netObs, NumericMatrix M1, NumericMatrix M2, NumericVector cent1, NumericVector cent2, NumericVector Lambda, double m) {

    double ll = 0;
    double tmp;
    int i, j;
    // logit values
    for (i=0; i<netObs.nrow(); i++) {
      for (j=0; j<netObs.ncol(); j++) {
        if (!NumericMatrix::is_na(netObs(i, j))) {
          tmp = interaction_proba(M1(_,i), M2(_,j), cent1(i), cent2(j), Lambda, 
            m);
          ll += netObs(i, j) ? log(tmp) : log(1 - tmp);
        }
      }
    }
    return ll;
  }

