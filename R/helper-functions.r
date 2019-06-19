


check_equal_length <- function(actual, predicted) {

    if (length(actual) != length(predicted)) {
        stop("`actual` and `predicted` must have equal length", call. = FALSE)
    }

    invisible()
}

check_cutoff_range <- function(cutoff) {

    if (cutoff > 1 || cutoff < 0) {
        stop("`cutoff` values must be in range of [0,1]", call. = FALSE)
    }

    invisible()
}

check_binary <- function(actual) {

    if (!all(actual %in% c(0, 1))) {
        stop("`actual` only supports binay values: 0 & 1", call. = FALSE)
    }

    invisible()
}

clip <- function(x, mi, ma) {
    clip_(x, mi, ma)
}

map_dbl <- function(.x, .f, ...) {
    vapply(X = .x, FUN = .f, FUN.VALUE = double(1), USE.NAMES = FALSE, ...)
}
