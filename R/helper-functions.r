
chec_empty_vec <- function(vec) {
    if (length(vec) == 0) {
        stop("vector must have positive length.", call. = FALSE)
    }
    
    invisible()
}

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
##' @importFrom stats complete.cases
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

class_prob <- function(vec, class) {
    chec_empty_vec(vec)
    length(which(vec == class)) / length(vec)
}

entropy <- function(vec) {
    chec_empty_vec(vec)
    li = c()
    for (cl in unique(vec)) {
        m = class_prob(vec = vec, class = cl)
        li = c(li, -1 * m * log(m))
    }
    etp = sum(li, na.rm = TRUE)
    etp
}

joint_class_prob <- function(vec_1, vec_2, class_1, class_2) {
    chec_empty_vec(vec_1)
    check_equal_length(vec_1, vec_2)
    length(which(vec_1 == class_1 & vec_2 == class_2)) / length(vec_1)
}

joint_entropy <- function(vec_1, vec_2) {
    check_equal_length(vec_1, vec_2)
    li = c()
    for(cl_1 in unique(vec_1)) {
        for(cl_2 in unique(vec_2)) {
            m = joint_class_prob(vec_1 = vec_1, vec_2 = vec_2, 
                                 class_1 = cl_1, class_2 = cl_2)
            li = c(li, - 1 * m * log(m))
        }
    }
    joint_etp = sum(li, na.rm = TRUE)
    joint_etp
}

expected_mutual_info <- function(vec_1, vec_2) {
    check_equal_length(vec_1, vec_2)
    N = length(vec_1)
    li = c()
    for (i in unique(vec_1)) {
        a = length(which(vec_1 == i))
        for (j in unique(vec_2)) {
            b = length(which(vec_2 == j))
            for (nij in max(a + b - N, 0, na.rm = TRUE): min(a, b, na.rm = TRUE)) {
                li = c(li, (nij / N) * 
                           log((N * nij) / (a * b)) * 
                           (factorial(a) * factorial(b) * factorial(N - a) * factorial(N - b)) /
                           (factorial(N) * factorial(nij) * factorial(a - nij) * factorial(b - nij) * factorial(N - a - b + nij)))
            }
        }
    }
    emi = sum(li, na.rm = TRUE)
    emi
}

