##' @title
##' Classification Metrics Parameters
##'
##' @description
##'
##' Documentation for shared parameters of functions that computes
##' classification metrics.
##'
##' @param actual \code{[numeric]} Ground truth binary numeric vector containing
##'     1 for the positive class and 0 for the negative class.
##' @param predicted \code{[numeric]} A vector of estimated probabilities.
##' @param cutoff \code{[numeric]} A cutoff value for \code{predicted} vector
##'     which classify a sample into a given class. Default value is 0.5
##'
##'
##' @name classification_params
NULL


##' @title
##' Confusion Matrix
##'
##'
##' @description
##'
##' Confusion matrix - a special kind of contingency table - is a specific table
##' layout that allows the visualization of performance of a classification
##' model (or classifier). It composes of four different combinations of
##' predicted and actual values.
##'
##' \code{confusion_list} computes and returns those combinations as a
##' \code{named list} while \code{mtr_confusion_matrix} returns a \code{named
##' matrix}.
##'
##' Here's elements of confusion matrix:
##'
##' \itemize{
##'   \item True Positive (TP)
##'   \item False Positive (FP)
##'   \item True Negative (TN)
##'   \item False Negative (FN)
##' }
##'
##' @inheritParams classification_params
##' @return A named list or a named two dimensions matrix
##' @author An Chu
##' @examples
##'
##' act <- c(1, 0, 0, 1, 1)
##' pred <- c(0.9, 0.3, 0.6, 0.5, 0.2)
##'
##' ## output as a R's list
##' ## metrics:::confusion_list(act, pred) # default value of cutoff = 0.5
##' ## metrics:::confusion_list(act, pred, cutoff = 0.7)
##'
##' ## output as a R's matrix
##' mtr_confusion_matrix(act, pred)
##' mtr_confusion_matrix(act, pred, cutoff = 0.7)
##'
##'
##' @name confusion_matrix
confusion_list <- function(actual, predicted, cutoff = 0.5) {

    check_equal_length(actual, predicted)
    check_binary(actual)
    check_cutoff_range(cutoff)

    TP <- sum((predicted > cutoff) & (actual == 1))
    TN <- sum((predicted <= cutoff) & (actual == 0))
    FN <- sum((predicted <= cutoff) & (actual == 1))
    FP <- sum((predicted > cutoff) & (actual == 0))

    list(TP = TP, TN = TN, FP = FP, FN = FN)
}


##' @rdname confusion_matrix
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


##' @title
##' True Positive Rate
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @name tpr
##' @export
mtr_tpr <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TP <- conf_list[["TP"]]
    FN <- conf_list[["FN"]]
    FP <- conf_list[["FP"]]
    denominator <- TP + FN

    if (denominator == 0 && FP != 0) return(0)
    if (denominator == 0 && FP == 0) return(1)

    TPR <- TP / denominator
    TPR
}


##' @rdname tpr
##' @export
mtr_true_positive_rate <- mtr_tpr



##' @rdname tpr
##' @export
mtr_hit_rate <- mtr_tpr



##' @rdname tpr
##' @export
mtr_sensitivity <- mtr_tpr


##' @rdname tpr
##' @export
mtr_detection_rate <- mtr_tpr



##' @rdname tpr
##' @export
mtr_recall <- mtr_tpr

## True Negative Rate ----------------------------------------------------------


##' @title
##' True Negative Rate
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @name tnr
##' @export
mtr_tnr <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TN <- conf_list[["TN"]]
    FP <- conf_list[["FP"]]
    FN <- conf_list[["FN"]]
    denominator <- TN + FP

    if (denominator == 0 && FN == 0) return(1)
    if (denominator == 0 && FN != 0) return(0)

    TNR <- TN / denominator
    TNR
}


##' @rdname tnr
##' @export
mtr_true_negative_rate <- mtr_tnr


##' @rdname tnr
##' @export
mtr_specificity <- mtr_tnr


##' @rdname tnr
##' @export
mtr_selectivity <- mtr_tnr


## False Negative Rate ---------------------------------------------------------


##' @title
##' False Negative Rate
##'
##' @inheritParams classification_params
##' @return A numeric output
##' @author An Chu
##' @name fnr
##' @export
mtr_fnr <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_tnr(actual, predicted, cutoff = cutoff)
}


##' @rdname fnr
##' @export
mtr_miss_rate <- mtr_fnr

##' @rdname fnr
##' @export
mtr_false_negative_rate <- mtr_fnr

## False Positive Rate ---------------------------------------------------------

##' @title
##' False Positive Rate
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @name fpr
##' @export
mtr_fpr <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_tnr(actual, predicted, cutoff = cutoff)
}

##' @rdname fpr
##' @export
mtr_fallout <- mtr_fpr

##' @rdname fpr
##' @export
mtr_false_alarm_rate <- mtr_fpr

##' @rdname fpr
##' @export
mtr_far <- mtr_fpr

##' @rdname fpr
##' @export
mtr_false_positive_rate <- mtr_fpr

## Accuracy --------------------------------------------------------------------


##' @title
##' Accuracy
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @seealso \code{\link{mtr_balanced_accuracy}}
##' @author An Chu
##' @name acc
##' @export
mtr_accuracy <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TP <- conf_list[["TP"]]
    TN <- conf_list[["TN"]]

    accuracy <- (TP + TN) / Reduce(sum, conf_list)

    accuracy
}

##' @rdname acc
##' @export
mtr_ccr <- mtr_accuracy

## Misclassification Rate ------------------------------------------------------

##' @title
##' Misclassification rate
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @export
mtr_misclassification_rate <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_accuracy(actual, predicted, cutoff = cutoff)
}

## Balanced Accuracy -----------------------------------------------------------


##' @title
##' Balanced Accuracy
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @seealso \code{\link{mtr_accuracy}}
##' @author An Chu
##' @export
mtr_balanced_accuracy <- function(actual, predicted, cutoff = 0.5) {

    sensitivity <- mtr_sensitivity(actual, predicted, cutoff = cutoff)
    specificity <- mtr_specificity(actual, predicted, cutoff = cutoff)

    (sensitivity + specificity) / 2
}

## Positive Predicted Value ----------------------------------------------------

##' @title
##' Positive Predicted Value
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @name ppv
##' @author An Chu
##' @export
mtr_ppv <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TP <- conf_list[["TP"]]
    FP <- conf_list[["FP"]]
    denominator <- TP + FP

    if (denominator == 0) return(0)

    PPV <- TP / (TP + FP)
    PPV
}


##' @rdname ppv
##' @export
mtr_positive_predicted_value <- mtr_ppv

##' @rdname ppv
##' @export
mtr_precision <- mtr_ppv

## Average Precision -----------------------------------------------------------

##' @title
##' Average Precision
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @export
mtr_average_precision <- function(actual, predicted) {

    ## keep distinct index of prediction and truth  vector
    ordering <- order(predicted)
    thresholds <- predicted[ordering]
    predicted <- predicted[ordering]
    actual <- actual[ordering]

    precision <- map_dbl(thresholds, function(x) mtr_precision(actual, predicted, x))
    recall <- map_dbl(thresholds, function(x) mtr_recall(actual, predicted, x))

    avg_prec <- sum(diff(recall) * precision[-1], na.rm = TRUE) * (-1)
    avg_prec
}


## False Discovery Rate --------------------------------------------------------

##' @title
##' False Discovery Rate
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @name fdr
##' @author An Chu
##' @export
mtr_fdr <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_ppv(actual, predicted, cutoff = cutoff)
}


##' @rdname fdr
##' @export
mtr_false_discovery_rate <- mtr_fdr

## Negative Predicted Value ----------------------------------------------------

##' @title
##' Negative Predicted Value
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @name npv
##' @author An Chu
##' @export
mtr_npv <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TN <- conf_list[["TN"]]
    FN <- conf_list[["FN"]]
    denominator <- TN + FN

    if (denominator == 0) return(0)

    NPV <- TN / (TN + FN)
    NPV
}

##' @rdname npv
##' @export
mtr_negative_predicted_value <- mtr_npv

## False Omission Rate ---------------------------------------------------------


##' @title
##' False Omission Rate
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @name for
##' @author An Chu
##' @export
mtr_for <- function(actual, predicted, cutoff = 0.5) {
    1 - mtr_npv(actual, predicted, cutoff = cutoff)
}


##' @rdname for
##' @export
mtr_false_omission_rate <- mtr_for

## F1 Score --------------------------------------------------------------------


##' @title
##' F1 Score
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @seealso \code{\link{mtr_fbeta_score}}
##' @author An Chu
##' @export
mtr_f1score <- function(actual, predicted, cutoff = 0.5) {

    precision <- mtr_precision(actual, predicted, cutoff = cutoff)
    recall <- mtr_recall(actual, predicted, cutoff = cutoff)
    f1score <- (2 * precision * recall) / (precision + recall)

    f1score
}

## F-beta Score ----------------------------------------------------------------

##' @title
##' F-beta Score
##'
##'
##' @param beta \code{[numeric]} To be filled
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @seealso \code{\link{mtr_f1score}}
##' @author An Chu
##' @export
mtr_fbeta_score <- function(actual, predicted, cutoff = 0.5, beta = 1) {

    precision <- mtr_precision(actual, predicted, cutoff = cutoff)
    recall <- mtr_recall(actual, predicted, cutoff = cutoff)
    fbeta <- (1 + beta^2) * ((precision * recall) / (beta^2 * precision + recall))

    fbeta
}


## Fowlkes-Mallows Index -------------------------------------------------------


##' @title
##' Fowlkes-Mallows Index
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @name gscore
##' @export
mtr_gscore <- function(actual, predicted, cutoff = 0.5) {

    conf_list = confusion_list(actual, predicted, cutoff = cutoff)

    TP <- conf_list[["TP"]]
    FP <- conf_list[["FP"]]
    FN <- conf_list[["FN"]]
    gscore <- sqrt((TP / (TP + FP)) * (TP / (TP + FN)))

    gscore
}


##' @rdname gscore
##' @export
mtr_fowlkes_mallows <- mtr_gscore

## Prevalence ------------------------------------------------------------------

##' @title
##' Prevalence
##'
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @export
mtr_prevalence <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)

    TP <- conf_list[["TP"]]
    FN <- conf_list[["FN"]]
    prevalence <- (TP + FN) / Reduce(sum, conf_list)

    prevalence
}


##' @title
##' Detection Prevalence
##'
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @export
mtr_detection_prevalence <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)
    TP <- conf_list[["TP"]]
    FP <- conf_list[["FP"]]
    dec_pre <- (TP + FP) / Reduce(sum, conf_list)

    dec_pre
}


## KS statistic ----------------------------------------------------------------


##' @title
##' KS Statistic
##'
##' @description
##'
##' Kolmogorov–Smirnov statistic quantifies a distance between the empirical
##' distribution function of the sample and the cumulative distribution function
##' of the reference distribution.
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @export
mtr_ks_statistic <- function(actual, predicted) {

    thresholds <- seq(min(predicted), max(predicted), 0.01)
    thresholds <- predicted

    TPR <- map_dbl(thresholds, function(x) mtr_tpr(actual, predicted, cutoff = x))
    FPR <- map_dbl(thresholds, function(x) mtr_fpr(actual, predicted, cutoff = x))

    max(TPR - FPR)
}

## Informedness ----------------------------------------------------------------


##' @title
##' Informedness
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @name yindex
##' @export
mtr_informedness <- function(actual, predicted, cutoff = 0.5) {

    sensitivity <- mtr_sensitivity(actual, predicted, cutoff = cutoff)
    specificity <- mtr_specificity(actual, predicted, cutoff = cutoff)

    sensitivity + specificity - 1
}


##' @rdname yindex
##' @export
mtr_youden_index <- mtr_informedness


## Markedness ------------------------------------------------------------------


##' @title
##' Markedness
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @export
mtr_markedness <- function(actual, predicted, cutoff = 0.5) {

    ppv <- mtr_positive_predicted_value(actual, predicted, cutoff = cutoff)
    npv <- mtr_negative_predicted_value(actual, predicted, cutoff = cutoff)

    ppv + npv - 1
}

## Matthews Correlation Coefficient --------------------------------------------


##' @title
##' Matthews Correlation Coefficient
##'
##' @inheritParams classification_params
##' @return A numeric scalar output
##' @author An Chu
##' @export
mtr_matthews_corr_coef <- function(actual, predicted, cutoff = 0.5) {

    conf_list <- confusion_list(actual, predicted, cutoff = cutoff)
    TP <- conf_list[["TP"]]
    TN <- conf_list[["TN"]]
    FP <- conf_list[["FP"]]
    FN <- conf_list[["FN"]]

    denom1 <- TP + FP
    denom2 <- TP + FN
    denom3 <- TN + FP
    denom4 <- TN + FN

    if (denom1 == 0 || denom2 == 0 || denom3 == 0 || denom4 == 0) return(NA_real_)

    mcc <- ((TP * TN) - (FP * FN)) / sqrt(prod(denom1, denom2, denom3, denom4))
    mcc
}


## Hamming Loss ----------------------------------------------------------------

mtr_hamming_loss <- function(actual, predicted, cutoff = 0.5) {

}


## Hinge Loss ------------------------------------------------------------------


mtr_hinge_loss <- function(actual, predicted, cutoff = 0.5) {

}

## Jaccard Score ---------------------------------------------------------------


mtr_jaccard_score <- function(actual, predicted, cutoff = 0.5) {

}
