// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// frequencyDistributionCpp
arma::mat frequencyDistributionCpp(const arma::mat& cold);
RcppExport SEXP _DCM_frequencyDistributionCpp(SEXP coldSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type cold(coldSEXP);
    rcpp_result_gen = Rcpp::wrap(frequencyDistributionCpp(cold));
    return rcpp_result_gen;
END_RCPP
}
// createConceptsCpp
Rcpp::List createConceptsCpp(const arma::mat& data_matrix, int nmax_choiceset_size);
RcppExport SEXP _DCM_createConceptsCpp(SEXP data_matrixSEXP, SEXP nmax_choiceset_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type data_matrix(data_matrixSEXP);
    Rcpp::traits::input_parameter< int >::type nmax_choiceset_size(nmax_choiceset_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(createConceptsCpp(data_matrix, nmax_choiceset_size));
    return rcpp_result_gen;
END_RCPP
}
// llCalc3
double llCalc3(const arma::vec& working_values, Rcpp::List model, Rcpp::List processed, const arma::mat& gq_int_matrix);
RcppExport SEXP _DCM_llCalc3(SEXP working_valuesSEXP, SEXP modelSEXP, SEXP processedSEXP, SEXP gq_int_matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type working_values(working_valuesSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type model(modelSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type processed(processedSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type gq_int_matrix(gq_int_matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(llCalc3(working_values, model, processed, gq_int_matrix));
    return rcpp_result_gen;
END_RCPP
}
// llMax2
Rcpp::List llMax2(Rcpp::List model, Rcpp::List processed, const arma::mat& gq_int_matrix, Rcpp::List nlm_params);
RcppExport SEXP _DCM_llMax2(SEXP modelSEXP, SEXP processedSEXP, SEXP gq_int_matrixSEXP, SEXP nlm_paramsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type model(modelSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type processed(processedSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type gq_int_matrix(gq_int_matrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type nlm_params(nlm_paramsSEXP);
    rcpp_result_gen = Rcpp::wrap(llMax2(model, processed, gq_int_matrix, nlm_params));
    return rcpp_result_gen;
END_RCPP
}
// llMax_ghq
Rcpp::List llMax_ghq(Rcpp::List model, Rcpp::List processed, const arma::mat& ghq_matrix1, Rcpp::List nlm_params);
RcppExport SEXP _DCM_llMax_ghq(SEXP modelSEXP, SEXP processedSEXP, SEXP ghq_matrix1SEXP, SEXP nlm_paramsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type model(modelSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type processed(processedSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type ghq_matrix1(ghq_matrix1SEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type nlm_params(nlm_paramsSEXP);
    rcpp_result_gen = Rcpp::wrap(llMax_ghq(model, processed, ghq_matrix1, nlm_params));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_DCM_frequencyDistributionCpp", (DL_FUNC) &_DCM_frequencyDistributionCpp, 1},
    {"_DCM_createConceptsCpp", (DL_FUNC) &_DCM_createConceptsCpp, 2},
    {"_DCM_llCalc3", (DL_FUNC) &_DCM_llCalc3, 4},
    {"_DCM_llMax2", (DL_FUNC) &_DCM_llMax2, 4},
    {"_DCM_llMax_ghq", (DL_FUNC) &_DCM_llMax_ghq, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_DCM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
