

##' @export
mtr_auc_roc <- function(actual, predicted) {

    check_equal_length(actual, predicted)

    r <- rank(predicted)
    n_pos <- sum(actual == 1)
    n_neg <- length(actual) - n_pos
    sum_r <- sum(r[actual == 1])

    auc <- (sum_r - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
    auc
}


##' @export
mtr_ar <- function(actual, predicted) {
    auc <- mtr_auc(actual, predicted)
    ar <- 2 * auc - 1
    ar
}

##' @export
mtr_accuracy_ratio <- mtr_ar

##' @export
mtr_gini_coef <- mtr_ar
