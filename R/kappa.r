

##' @title
##' Cohen’s Kappa
##'
##' @description
##'
##' \code{mtr_cohen_kappa} computes Kappa statistic which measures inter-rater
##' agreement for categorical items.
##'
##' The form of Kappa statistic is: \eqn{Kappa = (O - E) / (1 - E)} where O is
##' the observed accuracy and E is the expected accuracy based on the marginal
##' total of the confusion matrix. The statistic can take on values between -1
##' and 1; a value of 0 means there is no agreement between the actual and the
##' predicted, while a value of 1 indicates perfect concordance of the model
##' prediction and the observed classes.
##'
##' @references
##'
##' \itemize{
##'   \item Max Kuhn and Kjell Johnson, Applied Predictive Modeling (New York: Springer-Verlag, 2013)
##'   \item \href{https://stats.stackexchange.com/questions/82162/cohens-kappa-in-plain-english}{"Classification - Cohen’s Kappa in Plain English, Cross Validated"}
##' }
##'
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @export
##' @examples
##'
##' act <- c(1, 0, 1, 0, 1)
##' pred <- c(0.1, 0.9, 0.3, 0.5, 0.2)
##' mtr_cohen_kappa(act, pred)
##'
##'
##' set.seed(2093)
##' pred <- runif(1000)
##' act <- round(pred)
##' pred[sample(1000, 300)] <- runif(300) # noises
##' mtr_cohen_kappa(act, pred)
##'
mtr_cohen_kappa <- function(actual, predicted, cutoff = 0.5) {

    check_equal_length(actual, predicted)
    check_cutoff_range(cutoff)
    check_binary(actual)

    conf_mat <- mtr_confusion_matrix(actual, predicted, cutoff = cutoff)
    n <- sum(conf_mat)

    observed_accuracy <- mtr_accuracy(actual, predicted, cutoff = cutoff)

    expected_accuracy <- sum((rowSums(conf_mat) * colSums(conf_mat)) / n) / n

    kappa <- (observed_accuracy - expected_accuracy) / (1 - expected_accuracy)
    kappa
}
