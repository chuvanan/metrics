#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector clip_(NumericVector x, double mi, double ma) {
    return clamp(mi, x, ma);
}
