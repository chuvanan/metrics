##' @title
##' Regression Metrics Parameters
##'
##' @description
##' Documentation for shared parameters of functions that compute regression
##' metrics.
##'
##' @param actual \code{[numeric]} The ground truth numeric vector.
##' @param predicted \code{[numeric]} The predicted numeric vector, where each
##'     element in the vector is a prediction of the corresponding elements in
##'     \code{actual}.
##' @name regression_params
NULL


##' @title
##' Explained Variance
##'
##'
##' @description
##'
##' \code{mtr_explained_variance} computes explained variance, also known as
##' explained variation, which is interpreted as the percentage of variation in
##' one numeric vector explained by another. The best possible score is 1.0,
##' lower values are worse.
##'
##' @inheritParams regression_params
##' @importFrom stats var
##' @seealso \code{\link{mtr_r2}}
##' @return A numeric scalar output
##' @author An Chu
##' @examples
##'
##' act <- c(3, -0.5, 2, 7)
##' pred <- c(2.5, 0.0, 2, 8)
##' mtr_explained_variance(act, pred)
##'
##' act <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
##' pred <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
##' mtr_explained_variance(act, pred)
##'
##' @export
mtr_explained_variance <- function(actual, predicted) {

    check_equal_length(actual, predicted)

    exp_var <- 1 - (var(actual - predicted) / var(actual))
    exp_var
}


##' @title
##' Max Error
##'
##'
##' @description
##'
##' \code{mtr_max_error} computes the maximum residual error, a metric that
##' captures the worst error between the predicted value and the true value.
##'
##' @inheritParams regression_params
##' @return A numeric scalar output
##' @author An Chu
##' @examples
##'
##' act <- c(3, 2, 7, 1)
##' pred <- c(9, 2, 7, 1)
##' mtr_max_error(act, pred)
##'
##'
##' @export
mtr_max_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    me <- max(abs(actual - predicted), na.rm = TRUE)
    me
}



##' @title
##' Mean Absolute Error
##'
##'
##' @description
##'
##' \code{mtr_mean_absolute_error} is a measure of difference between the
##' predicted value and the true value.
##'
##'
##'
##' @inheritParams regression_params
##' @return A numeric scalar output
##' @author An Chu
##' @name mae
##' @examples
##'
##' act <- c(3, -0.5, 2, 7)
##' pred <- c(2.5, 0.0, 2, 8)
##' mtr_mean_absolute_error(act, pred)
##' mtr_mae(act, pred)
##'
##'
##' @export
mtr_mean_absolute_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    mae <- mean(abs(actual - predicted), na.rm = TRUE)
    mae
}


##' @export
##' @rdname mae
mtr_mae <- mtr_mean_absolute_error



##' @title
##' Mean Squared Error
##'
##'
##'
##' @description test
##'
##'
##'
##'
##' @inheritParams regression_params
##' @return A numeric scalar output
##' @author An Chu
##' @name mse
##' @examples
##'
##'
##'
##' @export
##'
##'
##'
##'
##' @export
mtr_mean_squared_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    mse <- mean((actual - predicted)^2, na.rm = TRUE)
    mse
}

##' @export
##' @rdname mse
mtr_mse <- mtr_mean_squared_error



##' @title
##' Root Mean Squared Error Function
##'
##'
##' @description test
##'
##'
##'
##'
##' @inheritParams regression_params
##' @return A numeric scalar output
##' @author An Chu
##' @examples
##'
##' act <- c(3, -0.5, 2, 7)
##' pred <- c(2.5, 0.0, 2, 8)
##' mtr_mean_squared_error(act, pred)
##'
##'
##' @export
mtr_root_mean_squared_error <- function(actual, predicted) {
    sqrt(mtr_mean_squared_error(actual, predicted))
}



##' @title
##' Mean Squared Log Error
##'
##'
##'
##' @description test
##'
##'
##'
##'
##'
##' @inheritParams regression_params
##' @return A numeric scalar output
##' @name msle
##' @author An Chu
##' @examples
##'
##' act <- c(3, 5, 2.5, 7)
##' pred <- c(2.5, 5, 4, 8)
##' mtr_mean_squared_log_error(act, pred)
##'
##'
##' @export
mtr_mean_squared_log_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    msle <- mtr_mean_squared_error(log1p(actual), log1p(predicted))
    msle
}


##' @export
##' @rdname msle
mtr_msle <- mtr_mean_squared_log_error




##' @title
##' Median Absolute Error
##'
##'
##'
##' @description test
##'
##'
##' @details test
##'
##'
##'
##' @inheritParams regression_params
##' @importFrom stats median
##' @return A numeric scalar output
##' @name mdae
##' @author An Chu
##' @examples
##'
##' act <- c(3, -0.5, 2, 7)
##' pred <- c(2.5, 0.0, 2, 8)
##' mtr_median_absolute_error(act, pred)
##'
##'
##' @export
mtr_median_absolute_error <- function(actual, predicted) {

    check_equal_length(actual, predicted)
    mdae <- median(abs(actual - predicted), na.rm = TRUE)
    mdae
}


##' @export
##' @rdname mdae
mtr_mdae <- mtr_median_absolute_error



##' @title R2 Score
##'
##'
##'
##' @description test
##'
##'
##'
##'
##' @inheritParams regression_params
##' @return A numeric scalar output
##' @author An Chu
##' @examples
##'
##' act <- c(3, -0.5, 2, 7)
##' pred <- c(2.5, 0.0, 2, 8)
##' mtr_r2(act, pred)
##'
##'
##' @export
mtr_r2 <- function(actual, predicted) {

    check_equal_length(actual, predicted)

    if (length(predicted) < 2) {
        warning("R^2 is not applicable for less than two instances", call. = FALSE)
        return(NA_real_)
    }

    numerator <- sum((actual - predicted) ^ 2)
    denominator <- sum((actual - mean(actual)) ^ 2)

    zero_numerator <- numerator == 0
    zero_denominator <- denominator == 0

    if (zero_numerator || zero_denominator) return(0)

    r2 <- 1 - numerator / denominator
    r2
}
