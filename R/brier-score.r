

##' @export
mtr_brier_score <- function(actual, predicted, cutoff = 0.5) {

    check_equal_length(actual, predicted)
    check_binary(actual)
    check_cutoff_range(cutoff)

    N <- length(predicted)
    brier_score <- sum((predicted - actual)^2) / N

    brier_score
}
