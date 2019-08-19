##' @title
##' Clustering Metrics Parameters
##'
##' @description
##' Documentation for shared parameters of functions that compute clustering
##' metrics.
##'
##' @param actual \code{[numeric]} The ground truth numeric vector.
##' @param predicted \code{[numeric]} The predicted numeric vector, where each
##'     element in the vector is a prediction of the corresponding elements in
##'     \code{actual}.
##' @name clustering_params
##' @include helper-functions.r
NULL


##' @title
##' Adjusted Mutual Information Score / Mututal Information Score
##'
##'
##' @description
##'
##' \code{mtr_mutual_info_score} measures the similarity, or mutual dependence 
##' between two variable. The worst possible score is 0, higher values are 
##' better.
##' 
##' 
##' @inheritParams clustering_params
##' @importFrom stats var
##' @seealso \code{\link{mtr_r2}}
##' @return A numeric scalar output
##' @author Phuc Nguyen
##' @examples
##'
##' act <- sample(1:10, 100, replace = T)
##' pred <- sample(1:10, 100, replace = T)
##' mtr_mutual_info_score(act, pred)
##'
##' act <- rep(c('a', 'b', 'c'), times = 4)
##' pred <- rep(c('a', 'b', 'c'), each = 4)
##' mtr_mutual_info_score(act, pred)
##'
##' @export
mtr_mutual_info_score <- function(actual, predicted) {
    chec_empty_vec(actual)
    check_equal_length(actual, predicted)
    entropy(actual) + entropy(predicted) - joint_entropy(vec_1 = actual, 
                                                         vec_2 = predicted)
}

mtr_normalized_mutual_info_score <- function(actual, predicted) {
    mtr_mutual_info_score(actual = actual, predicted = predicted) / 
        mean(c(entropy(vec = actual), entropy(vec = predicted)))
}

mtr_adjusted_mutual_info_score <- function(actual, predicted) {
    (mtr_mutual_info_score(actual, predicted) - expected_mutual_info(actual, predicted)) / 
        (mean(c(entropy(actual), entropy(predicted))) - expected_mutual_info(actual, predicted))
}
