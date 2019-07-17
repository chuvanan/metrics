


##' @title
##' Area Under ROC
##'
##' @description
##'
##' \code{mtr_auc_roc}
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @seealso \code{\link{mtr_ar}} \code{\link{mtr_auc_prc}}
##' @author An Chu
##' @include helper-functions.r
##' @examples
##'
##' set.seed(101)
##' pred <- runif(1000)
##' act <- round(pred)
##' pred[sample(1000, 500)] <- runif(500)   # noise
##' mtr_auc_prc(act, pred)
##'
##'
##' @export
mtr_auc_roc <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    check_binary(actual)

    ## This implementation uses Mann–Whitney U test for speed
    ## r <- rank(predicted)            ##
    ## n_pos <- sum(actual == 1)       ##
    ## n_neg <- length(actual) - n_pos ##
    ## sum_r <- sum(r[actual == 1])    ##
    ## auc <- (sum_r - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg) ##

    auc <- auc_(actual, predicted)
    auc
}


##' @title
##' Accuracy Ratio
##'
##' @description
##'
##' \code{mtr_ar}
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @name accuracy_ratio
##' @examples
##'
##' set.seed(101)
##' pred <- runif(1000)
##' act <- round(pred)
##' pred[sample(1000, 500)] <- runif(500)   # noise
##' mtr_accuracy_ratio(act, pred)
##'
##'
##' @export
mtr_ar <- function(actual, predicted) {
    auc <- mtr_auc_roc(actual, predicted)
    ar <- 2 * auc - 1
    ar
}

##' @rdname accuracy_ratio
##' @export
mtr_accuracy_ratio <- mtr_ar


##' @rdname accuracy_ratio
##' @export
mtr_gini_coef <- mtr_ar
