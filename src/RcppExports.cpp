// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// sourceMC
Rcpp::List sourceMC(int nbsp1, int nbsp2);
RcppExport SEXP alien_sourceMC(SEXP nbsp1SEXP, SEXP nbsp2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nbsp1(nbsp1SEXP);
    Rcpp::traits::input_parameter< int >::type nbsp2(nbsp2SEXP);
    rcpp_result_gen = Rcpp::wrap(sourceMC(nbsp1, nbsp2));
    return rcpp_result_gen;
END_RCPP
}
// webFromNicheModel
LogicalMatrix webFromNicheModel(int nbsp, double connec, bool connect_all, bool unbias, Nullable<NumericVector> niche);
RcppExport SEXP alien_webFromNicheModel(SEXP nbspSEXP, SEXP connecSEXP, SEXP connect_allSEXP, SEXP unbiasSEXP, SEXP nicheSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nbsp(nbspSEXP);
    Rcpp::traits::input_parameter< double >::type connec(connecSEXP);
    Rcpp::traits::input_parameter< bool >::type connect_all(connect_allSEXP);
    Rcpp::traits::input_parameter< bool >::type unbias(unbiasSEXP);
    Rcpp::traits::input_parameter< Nullable<NumericVector> >::type niche(nicheSEXP);
    rcpp_result_gen = Rcpp::wrap(webFromNicheModel(nbsp, connec, connect_all, unbias, niche));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"alien_sourceMC", (DL_FUNC) &alien_sourceMC, 2},
    {"alien_webFromNicheModel", (DL_FUNC) &alien_webFromNicheModel, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_alien(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
