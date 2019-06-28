

##' @title
##' Negative log loss, also known as logarithmic loss or cross-entropy loss.
##'
##'
##' @description
##' Log loss is an effective metric for measuring the performance of a
##' classification model where the prediction output is a probability value
##' between 0 and 1.
##'
##' Log loss quantifies the accuracy of a classifier by penalizing false
##' classifications. A perfect model would have a log loss of 0. Log loss
##' increases as the predicted probability diverges from the actual label.
##' This is the cost function used in logistic regression and neural networks.
##'
##' \code{mtr_log_loss} computes the elementwise log loss between two numeric
##' vectors. While \code{mtr_mean_log_loss} computes the average log loss between
##' two numeric vectors
##'
##'
##' @note
##' The logarithm used is the natural logarithm (base-e)
##'
##'
##' @param actual \code{[numeric]} Ground truth binary numeric vector containing
##'     1 for the positive class and 0 for the negative class.
##' @param predicted \code{[numeric]} A vector of estimated probabilities.
##' @param eps \code{[numeric]} In case of predicted probability is equal zero
##'     or one, log loss is undefined, so probabilities are clipped to max(eps,
##'     min(1 - eps, p)). The default value of eps is 1e-15.
##' @return A numeric vector output
##' @name logloss
##' @author An Chu
##' @examples
##'
##' ## log loss for scalar inputs, see how log loss is converging to zero
##' mtr_log_loss(1, 0.1)
##' mtr_log_loss(1, 0.5)
##' mtr_log_loss(1, 0.9)
##' mtr_log_loss(1, 1)
##'
##' ## mean log loss
##' act <- c(0, 1, 1, 0, 0)
##' pred <- c(0.12, 0.45, 0.9, 0.3, 0.4)
##' mtr_mean_log_loss(actual = act, predicted = pred)
##'
##' @export
##'
##' act <- c(1, 0, 0, 1, 1, 1)
##' pred <- c(0.1, 0.7, 0.3, 0.9, 0.2, 0.1)
##'
##' ## log loss vector
##' mtr_log_loss(act, pred)
##' mtr_cross_entropy(act, pred)
##'
##' ## mean log loss
##' mtr_mean_log_loss(act, pred)
##' mtr_mean_cross_entropy(act, pred)
##'
##'
##' @export
mtr_log_loss <- function(actual, predicted, eps = 1e-15) {

    check_equal_length(actual, predicted)
    check_binary(actual)

    predicted <- clip(predicted, mi = eps, ma = 1 - eps)

    logloss <- (-1) * (actual * log(predicted) + (1 - actual) * log(1 - predicted))

    logloss
}


##' @rdname logloss
##' @export
mtr_cross_entropy <- mtr_log_loss



##' @rdname logloss
##' @export
mtr_mean_log_loss <- function(actual, predicted, eps = 1e-15) {

    logloss <- mtr_log_loss(actual, predicted, eps = eps)

    mean(logloss)
}


##' @rdname logloss
##' @export
mtr_mean_cross_entropy <- mtr_mean_log_loss
