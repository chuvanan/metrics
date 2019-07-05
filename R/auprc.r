##' @title
##' Area Under Precision-Recall Curve
##'
##' @description
##'
##' \code{mtr_auc_prc}
##'
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @seealso \code{\link{mtr_auc_roc}}
##' @author An Chu
##' @examples
##'
##' set.seed(101)
##' pred <- runif(1000)
##' act <- round(pred)
##' pred[sample(1000, 500)] <- runif(500)   # noise
##' mtr_auc_prc(act, pred)
##'
##'
##'
##' @export
mtr_auc_prc <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    check_binary(actual)

    thresholds <- predicted
    recall <- map_dbl(thresholds, function(x) mtr_recall(actual, predicted, cutoff = x))
    precision <- map_dbl(thresholds, function(x) mtr_precision(actual, predicted, cutoff = x))

    trapezoid(precision, recall)
}
