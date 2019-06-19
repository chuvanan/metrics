

##' @description
##' Log loss, also known as logistic loss or cross-entropy loss.
##'
##'
##'
##'
##' @title Log Loss
##' @param actual Numeric. A binary vector of actual outcome.
##' @param predicted Numeric. A vector of predicted values.
##' @param eps Numeric. Log loss is undefined for
##' @return A length-1 numeric value.
##' @author An Chu
mtr_log_loss <- function(actual, predicted, eps = 1e-15) {

    check_equal_length(actual, predicted)

    predicted <- clip(predicted, eps, 1 - eps)

    logloss <- -1 * (actual * log(predicted) + (1 - actual) * log(1 - predicted))

    logloss
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param actual
##' @param predicted
##' @param eps
##' @return
##' @author An Chu
mtr_mean_log_loss <- function(actual, predicted, eps = 1e-15) {

    logloss <- mtr_log_loss(actual, predicted, eps = eps)

    mean(logloss)
}
