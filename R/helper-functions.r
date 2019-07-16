


check_equal_length <- function(actual, predicted) {

    if (length(actual) != length(predicted)) {
        stop("`actual` and `predicted` must have equal length.", call. = FALSE)
    }

    invisible()
}

check_cutoff_range <- function(cutoff) {

    if (cutoff > 1 || cutoff < 0) {
        stop("`cutoff` values must be in range of [0,1].", call. = FALSE)
    }

    invisible()
}

check_binary <- function(actual) {

    if (!all(actual %in% c(0, 1))) {
        stop("`actual` only supports binary values: 0 & 1.", call. = FALSE)
    }

    invisible()
}

clip <- function(x, mi, ma) {
    clip_(x, mi, ma)
}

map_dbl <- function(.x, .f, ...) {
    vapply(X = .x, FUN = .f, FUN.VALUE = double(1), USE.NAMES = FALSE, ...)
}


## Trapezoidal rule
## https://en.wikipedia.org/wiki/Trapezoidal_rule
trapezoid <- function(x, y) {
    ## assumes x is a partition and that x & y are the same length

    if (anyNA(x) || anyNA(y)) {
        comp <- complete.cases(x, y)
        x <- x[comp]
        y <- y[comp]
    }

    x_ord <- order(x)
    x <- x[x_ord]
    y <- y[x_ord]

    ## dx
    dx <- diff(x)
    ## mid height of y
    height <- (y[-1] + y[-length(y)]) / 2

    sum(dx * height)
}
