


##' @export
mtr_confusion_matrix <- function(actual, predicted, cutoff = 0.5) {

    check_length(actual, predicted)
    check_cutoff_range(cutoff)

    confusion_matrix_(actual, predicted, cutoff = cutoff)
}


##' @export
mtr_tpr <- function(actual, predicted, cutoff = 0.5) {

    check_length(actual, predicted)
    check_cutoff_range(cutoff)

    true_positive_rate_(actual, predicted, cutoff = cutoff)
}

##' @export
mtr_hit_rate <- mtr_tpr

##' @export
mtr_sensitivity <- mtr_tpr

##' @export
mtr_detection_rate <- mtr_tpr

##' @export
mtr_recall <- mtr_tpr


##' @export
mtr_tnr <- function(actual, predicted, cutoff = 0.5) {

    check_length(actual, predicted)
    check_cutoff_range(cutoff)

    true_negative_rate_(actual, predicted, cutoff = cutoff)

}

##' @export
mtr_specificity <- mtr_tnr

##' @export
mtr_selectivity <- mtr_tnr


##' @export
mtr_fpr <- function(actual, predicted, cutoff = 0.5) {

    check_length(actual, predicted)
    check_cutoff_range(cutoff)

    false_positive_rate_(actual, predicted, cutoff = cutoff)
}

##' @export
mtr_fallout <- mtr_fpr


## Helper functions ------------------------------------------------------------

check_length <- function(actual, predicted) {

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
