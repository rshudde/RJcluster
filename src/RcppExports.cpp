// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// getIndividualList
arma::vec getIndividualList(const arma::colvec& classification, const arma::colvec& temp_index, int i);
RcppExport SEXP _RJcluster_getIndividualList(SEXP classificationSEXP, SEXP temp_indexSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type classification(classificationSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type temp_index(temp_indexSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(getIndividualList(classification, temp_index, i));
    return rcpp_result_gen;
END_RCPP
}
// getCCmatrix_c
List getCCmatrix_c(const arma::colvec& classification, const arma::colvec& temp_index, int G);
RcppExport SEXP _RJcluster_getCCmatrix_c(SEXP classificationSEXP, SEXP temp_indexSEXP, SEXP GSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type classification(classificationSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type temp_index(temp_indexSEXP);
    Rcpp::traits::input_parameter< int >::type G(GSEXP);
    rcpp_result_gen = Rcpp::wrap(getCCmatrix_c(classification, temp_index, G));
    return rcpp_result_gen;
END_RCPP
}
// getMatrixMeans_c
arma::mat getMatrixMeans_c(const List CC, const arma::mat X, const int d);
RcppExport SEXP _RJcluster_getMatrixMeans_c(SEXP CCSEXP, SEXP XSEXP, SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type CC(CCSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< const int >::type d(dSEXP);
    rcpp_result_gen = Rcpp::wrap(getMatrixMeans_c(CC, X, d));
    return rcpp_result_gen;
END_RCPP
}
// assignGroups_c
arma::vec assignGroups_c(const int N, const int G, const arma::colvec& classification, const List CC);
RcppExport SEXP _RJcluster_assignGroups_c(SEXP NSEXP, SEXP GSEXP, SEXP classificationSEXP, SEXP CCSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type N(NSEXP);
    Rcpp::traits::input_parameter< const int >::type G(GSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type classification(classificationSEXP);
    Rcpp::traits::input_parameter< const List >::type CC(CCSEXP);
    rcpp_result_gen = Rcpp::wrap(assignGroups_c(N, G, classification, CC));
    return rcpp_result_gen;
END_RCPP
}
// getFinalMeans_c
arma::mat getFinalMeans_c(const int G, const arma::vec& Group, const arma::mat& X);
RcppExport SEXP _RJcluster_getFinalMeans_c(SEXP GSEXP, SEXP GroupSEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type G(GSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type Group(GroupSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(getFinalMeans_c(G, Group, X));
    return rcpp_result_gen;
END_RCPP
}
// getImportantGenes_c
arma::colvec getImportantGenes_c(const arma::mat& X, const double cluster);
RcppExport SEXP _RJcluster_getImportantGenes_c(SEXP XSEXP, SEXP clusterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const double >::type cluster(clusterSEXP);
    rcpp_result_gen = Rcpp::wrap(getImportantGenes_c(X, cluster));
    return rcpp_result_gen;
END_RCPP
}
// tcrossprod_c
arma::mat tcrossprod_c(const arma::mat& X, const arma::mat& Y);
RcppExport SEXP _RJcluster_tcrossprod_c(SEXP XSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(tcrossprod_c(X, Y));
    return rcpp_result_gen;
END_RCPP
}
// scale_c
arma::mat scale_c(const arma::mat& X, bool medians);
RcppExport SEXP _RJcluster_scale_c(SEXP XSEXP, SEXP mediansSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< bool >::type medians(mediansSEXP);
    rcpp_result_gen = Rcpp::wrap(scale_c(X, medians));
    return rcpp_result_gen;
END_RCPP
}
// get_percentages_c
arma::vec get_percentages_c(const arma::mat& X);
RcppExport SEXP _RJcluster_get_percentages_c(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(get_percentages_c(X));
    return rcpp_result_gen;
END_RCPP
}
// replace_zeroes_c
void replace_zeroes_c(arma::mat& X);
RcppExport SEXP _RJcluster_replace_zeroes_c(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type X(XSEXP);
    replace_zeroes_c(X);
    return R_NilValue;
END_RCPP
}
// rcpparma_hello_world
arma::mat rcpparma_hello_world();
RcppExport SEXP _RJcluster_rcpparma_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpparma_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_outerproduct
arma::mat rcpparma_outerproduct(const arma::colvec& x);
RcppExport SEXP _RJcluster_rcpparma_outerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_outerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_innerproduct
double rcpparma_innerproduct(const arma::colvec& x);
RcppExport SEXP _RJcluster_rcpparma_innerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_innerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_bothproducts
Rcpp::List rcpparma_bothproducts(const arma::colvec& x);
RcppExport SEXP _RJcluster_rcpparma_bothproducts(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_bothproducts(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_RJcluster_getIndividualList", (DL_FUNC) &_RJcluster_getIndividualList, 3},
    {"_RJcluster_getCCmatrix_c", (DL_FUNC) &_RJcluster_getCCmatrix_c, 3},
    {"_RJcluster_getMatrixMeans_c", (DL_FUNC) &_RJcluster_getMatrixMeans_c, 3},
    {"_RJcluster_assignGroups_c", (DL_FUNC) &_RJcluster_assignGroups_c, 4},
    {"_RJcluster_getFinalMeans_c", (DL_FUNC) &_RJcluster_getFinalMeans_c, 3},
    {"_RJcluster_getImportantGenes_c", (DL_FUNC) &_RJcluster_getImportantGenes_c, 2},
    {"_RJcluster_tcrossprod_c", (DL_FUNC) &_RJcluster_tcrossprod_c, 2},
    {"_RJcluster_scale_c", (DL_FUNC) &_RJcluster_scale_c, 2},
    {"_RJcluster_get_percentages_c", (DL_FUNC) &_RJcluster_get_percentages_c, 1},
    {"_RJcluster_replace_zeroes_c", (DL_FUNC) &_RJcluster_replace_zeroes_c, 1},
    {"_RJcluster_rcpparma_hello_world", (DL_FUNC) &_RJcluster_rcpparma_hello_world, 0},
    {"_RJcluster_rcpparma_outerproduct", (DL_FUNC) &_RJcluster_rcpparma_outerproduct, 1},
    {"_RJcluster_rcpparma_innerproduct", (DL_FUNC) &_RJcluster_rcpparma_innerproduct, 1},
    {"_RJcluster_rcpparma_bothproducts", (DL_FUNC) &_RJcluster_rcpparma_bothproducts, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_RJcluster(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
