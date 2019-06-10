
confusion_list <- function(actual, predicted, cutoff = 0.5) {

    check_equal_length(actual, predicted)
    check_cutoff_range(cutoff)

    TP <- sum((predicted > cutoff) & (actual == 1))
    TN <- sum((predicted <= cutoff) & (actual == 0))
    FP <- sum((predicted > cutoff) & (actual == 0))
    FN <- sum((predicted <= cutoff) & (actual == 1))

    list(TP = TP, TN = TN, FP = FP, FN = FN)

}

##' @export
mtr_confusion_matrix <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    conf_mat <- matrix(numeric(4), ncol = 2,
                       dimnames = list(c("Positive", "Negative"),
                                       c("True", "False")))

    conf_mat["Positive", "True"] = conf_list[["TP"]]
    conf_mat["Positive", "False"] = conf_list[["FP"]]
    conf_mat["Negative", "True"] = conf_list[["FN"]]
    conf_mat["Negative", "False"] = conf_list[["TN"]]

    conf_mat
}


## True Positive Rate ----------------------------------------------------------

##' @export
mtr_tpr <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TP <- conf_list[["TP"]]
    FN <- conf_list[["FN"]]
    FP <- conf_list[["FP"]]
    P <- TP + FN

    if (P == 0 && FP == 0) {
        return(1)
    } else if (P == 0 && FP != 0) {
        return(0)
    } else {
        TPR = TP / P
    }

    TPR
}

##' @export
mtr_hit_rate <- mtr_tpr

##' @export
mtr_sensitivity <- mtr_tpr

##' @export
mtr_detection_rate <- mtr_tpr

##' @export
mtr_recall <- mtr_tpr

## True Negative Rate ----------------------------------------------------------

##' @export
mtr_tnr <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TN = conf_list[["TN"]]
    FP = conf_list[["FP"]]
    FN = conf_list[["FN"]]
    N = TN + FP

    if (N == 0 && FN == 0) {
        return(1)
    } else if (N == 0 && FN != 0) {
        return(0)
    } else {
        TNR = TN / N
    }

    TNR
}

##' @export
mtr_specificity <- mtr_tnr

##' @export
mtr_selectivity <- mtr_tnr


## False Negative Rate ---------------------------------------------------------

##' @export
mtr_fnr <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_tnr(actual, predicted, cutoff = cutoff)
}

##' @export
mtr_miss_rate <- mtr_fnr

## False Positive Rate ---------------------------------------------------------


##' @export
mtr_fpr <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_tnr(actual, predicted, cutoff = cutoff)
}

##' @export
mtr_fallout <- mtr_fpr

##' @export
mtr_far <- mtr_fpr


## Accuracy --------------------------------------------------------------------

##' @export
mtr_accuracy <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    accuracy <- (conf_list[["TP"]] + conf_list[["TN"]]) / Reduce(sum, conf_list)

    accuracy
}

##' @export
mtr_error_rate <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_accuracy(actual, predicted, cutoff = cutoff)
}

##' @export
mtr_balanced_accuracy <- function(actual, predicted, cutoff = 0.5) {

    sensitivity <- mtr_sensitivity(actual, predicted, cutoff = cutoff)
    specificity <- mtr_specificity(actual, predicted, cutoff = cutoff)

    (sensitivity + specificity) / 2
}

## Positive Predicted Value ----------------------------------------------------


##' @export
mtr_ppv <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TP <- conf_list[["TP"]]
    FP <- conf_list[["FP"]]
    PPV <- TP / (TP + FP)

    PPV
}

##' @export
mtr_precision <- mtr_ppv

##' @export
mtr_fdr <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_ppv(actual, predicted, cutoff = cutoff)
}

## Negative Predicted Value ----------------------------------------------------


##' @export
mtr_npv <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TN <- conf_list[["TN"]]
    FN <- conf_list[["FN"]]
    NPV <- TN / (TN + FN)

    NPV
}

##' @export
mtr_for <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_npv(actual, predicted, cutoff = cutoff)
}

## Helper functions ------------------------------------------------------------

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
