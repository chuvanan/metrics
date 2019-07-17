

set.seed(101)
pred <- runif(1000)
act <- round(pred)
pred[sample(1000, 500)] <- runif(500)   # noise


## Test AUROC ------------------------------------------------------------------

## Metrics::auc(act, pred)
## 0.7568871
## ROCR::performance(ROCR::prediction(pred, act), "auc")
## 0.7568871
expect_equal(
    mtr_auc_roc(act, pred),
    target = 0.7568871,
    tol = 1e-7
)


## AUC helper ------------------------------------------------------------------

## https://github.com/tidymodels/yardstick/blob/master/tests/testthat/test-auc.R
## expect_equal(
##     trapezoid(c(1, 1.2, 1.6, 2), c(4, 3.8, 4.2, 5)),
##     target = 4.22
## )

## Test AUPRC ------------------------------------------------------------------

## PRROC::pr.curve(pred[act == 1], pred[act == 0], curve = FALSE)
## Area under curve (Integral): 0.7149685
## Area under curve (Davis & Goadrich): 0.7149619

## expect_equal(
##     mtr_auc_prc(act, pred),
##     target = 0.7149619,
##     tol = 1e-2
## )

## Test AUC on large dataset ---------------------------------------------------

## preds <- c(rbeta(1e6, 25, 30), rbeta(1e6, 30, 25))
## target <- rep(c(0, 1), each = 1e6)
## mtr_auc_roc(target, preds)
## ModelMetrics::auc(target, preds)

## microbenchmark::microbenchmark(                            ##
##                     mtr = mtr_auc_roc(target, preds),      ##
##                     mm = ModelMetrics::auc(target, preds), ##
##                     times = 5                              ##
##                 )                                          ##
