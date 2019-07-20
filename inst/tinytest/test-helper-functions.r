
expect_error(
    check_equal_length(c(1, 0), c(0.1, 0.2, 0.3))
)

expect_silent(
    check_equal_length(c(1, 0), c(0.1, 0.2))
)

expect_error(
    check_cutoff_range(1.1)
)

expect_error(
    check_cutoff_range(-0.1)
)

expect_silent(
    check_cutoff_range(0.5)
)

expect_error(
    check_binary(c(0, 1, 2))
)

expect_error(
    check_binary(c(-1, 0))
)

expect_error(
    check_binary(c(-1, 1))
)

expect_silent(
    check_binary(c(1, 0))
)

## https://gallery.rcpp.org/articles/sugar-function-clamp/
## https://stackoverflow.com/questions/13868963/clip-values-between-a-minimum-and-maximum-allowed-value-in-r
set.seed(42)
x <- rnorm(100)
a <- -1.0
b <- 1.0

expect_equal(
    clip(x, mi = a, ma = b),
    target = pmax(a, pmin(x, b))
)

## https://github.com/tidymodels/yardstick/blob/master/tests/testthat/test-auc.R
expect_equal(
    trapezoid(c(1, 1.2, 1.6, 2), c(4, 3.8, 4.2, 5)),
    target = 4.22
)
