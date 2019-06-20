


##' @export
mtr_brier_score <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    check_binary(actual)

    n <- length(predicted)
    brier_score <- sum((actual - predicted)^2) / n

    brier_score
}
