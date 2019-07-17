

##' @title
##' Classification Optimal Cutoff
##'
##' @description Optimal cutoff
##'
##' @inheritParams classification_params
##' @param objective \code{[character]}
##' @return A scalar numeric output
##' @author An Chu
mtr_optimal_cutoff <- function(actual, predicted,
                               objective = c("positive_class", "negative_class",
                                             "both", "missclassified_error")) {

    check_equal_length(actual, predicted)
    check_binary(actual)

    objective <- match.arg(objective)
    thresholds <- seq(min(predicted), max(actual), 0.01)

    pos_idx <- which()
    neg_idx <- which()
    both_idx <- which()
    mce_idx <- which()

    optimal_cutoff <- switch(
        objective,
        positive_class = {thresholds[pos_idx]},
        negative_class = {thresholds[neg_idx]},
        both = {thresholds[both_idx]},
        missclassified_error = {thresholds[mce_idx]}
    )

    optimal_cutoff
}
