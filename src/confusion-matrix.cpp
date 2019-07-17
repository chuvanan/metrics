#include <Rcpp.h>
using namespace Rcpp;

List confusion_list_(NumericVector actual, NumericVector predicted,
                     double cutoff) {

  /* True Positive */
  double TP = sum((predicted > cutoff) & (actual == 1));

  /* False Positive */
  double FP = sum((predicted > cutoff) & (actual == 0));

  /* True Negative */
  double TN = sum((predicted <= cutoff) & (actual == 0));

  /* False Negative */
  double FN = sum((predicted <= cutoff) & (actual == 1));

  return List::create(_["TP"] = TP, _["FP"] = FP, _["TN"] = TN, _["FN"] = FN);
}

// [[Rcpp::export]]
NumericMatrix confusion_matrix_(NumericVector actual, NumericVector predicted,
                                double cutoff) {

  List conf_list = confusion_list_(actual, predicted, cutoff);

  NumericMatrix conf_mat = NumericMatrix(Dimension(2, 2));

  conf_mat(0, 0) = conf_list["TP"];
  conf_mat(0, 1) = conf_list["FP"];
  conf_mat(1, 0) = conf_list["FN"];
  conf_mat(1, 1) = conf_list["TN"];

  colnames(conf_mat) = CharacterVector::create("True", "False");
  rownames(conf_mat) = CharacterVector::create("Positive", "Negative");

  return conf_mat;
}

// [[Rcpp::export]]
double true_positive_rate_(NumericVector actual, NumericVector predicted,
                           double cutoff) {

  NumericMatrix conf_mat = confusion_matrix_(actual, predicted, cutoff);

  double TPR = conf_mat(0, 0) / (conf_mat(0, 0) + conf_mat(1, 0));

  return TPR;
}

// [[Rcpp::export]]
double true_negative_rate_(NumericVector actual, NumericVector predicted,
                           double cutoff) {

  NumericMatrix conf_mat = confusion_matrix_(actual, predicted, cutoff);

  double TNR = conf_mat(1, 1) / (conf_mat(1, 1) + conf_mat(0, 1));

  return TNR;
}

// [[Rcpp::export]]
double false_positive_rate_(NumericVector actual, NumericVector predicted,
                            double cutoff) {

  NumericMatrix conf_mat = confusion_matrix_(actual, predicted, cutoff);

  double FPR = conf_mat(0, 1) / (conf_mat(0, 1) + conf_mat(1, 1));

  return FPR;
}
