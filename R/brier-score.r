

##' @title
##' Brier score
##'
##'
##' @description
##'
##' Brier score measures the accuracy of probabilistic predictions; the smaller
##' the Brier score, the better. The Brier score always takes on a value between
##' zero and one.
##'
##' Given N predictions, the Brier score is defined by the mean squared
##' difference between (1) the estimated probability assigned to the possible
##' outcome and (2) the actual outcome.
##'
##' Here is a few examples that help the interpretation of Brier score.
##'
##' Suppose that one is forecasting the probability P that it will rain on a
##' given day. Then the Brier score is calculated as follows:
##'
##' \itemize{
##'
##'   \item If the forecast is 100\% (P = 1) and it rains, then the Brier Score
##'   is 0, the best score achievable.
##'
##'   \item If the forecast is 100\% and it does not rain, then the Brier Score
##'   is 1, the worst score achievable.
##'
##'   \item If the forecast is 70\% (P = 0.70) and it rains, then the Brier Score
##'   is (0.70 − 1)^2 = 0.09.
##'
##'   \item If the forecast is 30\% (P = 0.30) and it rains, then the Brier Score
##'   is (0.30 − 1)^2 = 0.49.
##'
##' }
##'
##'
##'
##' @param actual \code{[numeric]} Ground truth binary numeric vector containing
##'     1 for the positive class and 0 for the negative class.
##' @param predicted \code{[numeric]} A vector of estimated probabilities.
##' @return A scalar numeric output
##' @author An Chu
##' @seealso \code{\link{mtr_log_loss}} \code{\link{mtr_mean_log_loss}}
##' @examples
##'
##' act <- c(0, 1, 1, 0)
##' pred <- c(0.1, 0.9, 0.8, 0.3)
##' mtr_brier_score(act, pred)
##'
##' @export
mtr_brier_score <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    check_binary(actual)

    if (max(predicted) > 1) {
        stop("`predicted` cannot contain values greater than 1.", call. = FALSE)
    }

    if (min(predicted) < 0) {
        stop("`predicted` cannot contain values less than 0.", call. = FALSE)
    }

    n <- length(predicted)
    brier_score <- sum((actual - predicted)^2) / n

    brier_score
}
