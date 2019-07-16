

## Classification Metrics ------------------------------------------------------

two_class_sample_data <- read.csv("two-class-sample-data.csv")
sklearn_metrics_two_class <- read.csv("sklearn-metrics-two-class.csv")

expect_equal(
    with(two_class_sample_data, mtr_accuracy(act, pred)),
    target = sklearn_metrics_two_class$accuracy_score
)

expect_equal(
    with(two_class_sample_data, mtr_auc_roc(act, pred)),
    target = sklearn_metrics_two_class$auc
)

## expect_equal(
##     with(two_class_sample_data, mtr_average_precision(act, pred)),
##     target = sklearn_metrics_two_class$average_precision_score
## )

expect_equal(
    with(two_class_sample_data, mtr_balanced_accuracy(act, pred)),
    target = sklearn_metrics_two_class$balanced_accuracy
)

expect_equal(
    with(two_class_sample_data, mtr_brier_score(act, pred)),
    target = sklearn_metrics_two_class$brier_score
)

expect_equal(
    with(two_class_sample_data, mtr_cohen_kappa(act, pred)),
    target = sklearn_metrics_two_class$cohen_kappa_score
)

expect_equal(
    with(two_class_sample_data, mtr_f1score(act, pred)),
    target = sklearn_metrics_two_class$f1_score
)

expect_equal(
    with(two_class_sample_data, mtr_mean_log_loss(act, pred)),
    target = sklearn_metrics_two_class$log_loss
)

expect_equal(
    with(two_class_sample_data, mtr_matthews_corr_coef(act, pred)),
    target = sklearn_metrics_two_class$matthews_corrcoef
)

expect_equal(
    with(two_class_sample_data, mtr_precision(act, pred)),
    target = sklearn_metrics_two_class$precision
)

expect_equal(
    with(two_class_sample_data, mtr_recall(act, pred)),
    target = sklearn_metrics_two_class$recall
)

## Regression Metrics ----------------------------------------------------------

regression_sample_data <- read.csv("regression-sample-data.csv")
sklearn_metrics_regression <- read.csv("sklearn-metrics-regression.csv")

expect_equal(
    with(regression_sample_data, mtr_explained_variance(act, pred)),
    target = sklearn_metrics_regression$explained_variation
)

expect_equal(
    with(regression_sample_data, mtr_max_error(act, pred)),
    target = sklearn_metrics_regression$max_error
)

expect_equal(
    with(regression_sample_data, mtr_mean_absolute_error(act, pred)),
    target = sklearn_metrics_regression$mean_absolute_error
)

expect_equal(
    with(regression_sample_data, mtr_mean_squared_error(act, pred)),
    target = sklearn_metrics_regression$mean_squared_error
)

expect_equal(
    with(regression_sample_data, mtr_mean_squared_log_error(act, pred)),
    target = sklearn_metrics_regression$mean_squared_log_error
)

expect_equal(
    with(regression_sample_data, mtr_mean_absolute_error(act, pred)),
    target = sklearn_metrics_regression$mean_absolute_error
)

expect_equal(
    with(regression_sample_data, mtr_r2(act, pred)),
    target = sklearn_metrics_regression$r2
)
