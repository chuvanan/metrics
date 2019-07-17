

two_class_sample_data <- read.csv("../extdata/two-class-sample-data.csv")
act <- two_class_sample_data$act
pred <- two_class_sample_data$pred

## Test correctness ------------------------------------------------------------

## test confusion matrix
expect_equivalent(
    mtr_confusion_matrix(act, pred),
    matrix(c(410, 84, 78, 428), ncol = 2)
)

## test accuracy / correctly classified rate
expect_equal(
    mtr_accuracy(act, pred),
    target = (410 + 428) / length(act)
)

## test true posotive rate / sensitivity / hit rate / detection rate / recall
expect_equal(
    mtr_sensitivity(act, pred),
    target = 410 / (410 + 84)
)

## test true negative rate / specificity/ selectivity
expect_equal(
    mtr_specificity(act, pred),
    target = 428 / (428 + 78)
)

## test false negative rate / miss rate
expect_equal(
    mtr_false_negative_rate(act, pred),
    target = 78 / (78 + 428)
)

## test positive predicted value / precision
expect_equal(
    mtr_positive_predicted_value(act, pred),
    target = 410 / (410 + 78)
)

## test false discovery rate
expect_equal(
    mtr_false_discovery_rate(act, pred),
    target = 78 / (410 + 78)
)

## test negative predicted value
expect_equal(
    mtr_negative_predicted_value(act, pred),
    target = 428 / (428 + 84)
)

## test prevalence
expect_equal(
    mtr_prevalence(act, pred),
    target = (410 + 84) / length(act)
)

## test detection prevalence
expect_equal(
    mtr_detection_prevalence(act, pred),
    target = (410 + 78) / length(act)
)

## test balanced accuracy
expect_equal(
    mtr_balanced_accuracy(act, pred),
    target = ((410 / (410 + 84)) + (428 / (428 + 78))) / 2
)

## test Matthews correlation coefficient
expect_equal(
    mtr_matthews_corr_coef(act, pred),
    target = ((410 * 428) - (84 * 78)) / sqrt(prod(410 + 84, 410 + 78, 428 + 84, 428 + 78))
)

## test markedness
expect_equal(
    mtr_markedness(act, pred),
    target = mtr_ppv(act, pred) + mtr_npv(act, pred) - 1
)

## test informedness
expect_equal(
    mtr_informedness(act, pred),
    target = mtr_sensitivity(act, pred) + mtr_specificity(act, pred) - 1
)

## Test edge cases -------------------------------------------------------------

expect_equal(
    mtr_hit_rate(c(0, 0, 0, 0, 0), c(0.1, 0.3, 0.5, 1, 0), 0.5),
    target = 0
)

expect_equal(
    mtr_hit_rate(c(0, 0, 0, 0, 0), c(0.1, 0.3, 0.5, 1, 0), 1),
    target = 1
)

expect_equal(
    mtr_hit_rate(c(0, 0, 0, 0, 0), c(0.1, 0.3, 0.5, 1, 0), 0),
    target = 0
)

## Test error handling ---------------------------------------------------------

## raise error if length of input vectors is inconsistent
expect_error(
    confusion_list(c(0, 1, 0, 0), c(0.1, 0.3, 0.4))
)

## raise error if value of `cutoff` is out of range [0,1]
expect_error(
    confusion_list(c(0, 1, 1), c(0.1, 0.4, 0.8), 1.1)
)

## raise error if `actual` is multi-class
expect_error(
    confusion_list(c(0, 1, -1), c(0.3, 0.2, 0.9))
)
