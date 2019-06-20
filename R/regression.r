

##' @export
mtr_explained_variance <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    exp_var <- 1 - (var(actual - predicted) / var(actual))
    exp_var
}


##' @export
mtr_max_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    me <- max(abs(actual - predicted))
    me
}


##' @export
mtr_mean_absolute_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    mae <- mean(abs(actual - predicted))
    mae
}


##' @export
mtr_mean_squared_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    mse <- mean((actual - predicted)^2)
    mse
}


##' @export
mtr_mean_squared_log_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    msle <- mtr_mean_squared_error(log1p(actual), log1p(predicted))
    msle
}


##' @export
mtr_median_absolute_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    mdae <- median(abs(actual - predicted))
    mdae
}


##' @export
mtr_r2 <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    r2 <- 1 - (sum(actual - predicted)^2) / (sum(actual - mean(actual))^2)
    r2
}
