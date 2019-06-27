
act <- c(0, 1, 1, 0, 0)
pred <- c(0.12, 0.45, 0.9, 0.3, 0.4)


## Test correctness ------------------------------------------------------------

## check eps and handling of absolute zero and one probabilities
expect_equal(mtr_mean_log_loss(act, act), 0)

## Metrics::logLoss(act, pred)
## 0.3798404
expect_equal(
    round(mtr_mean_log_loss(act, pred), 7),
    0.3798404
)

## Metrics::ll(act, pred)
## c(0.1278334, 0.7985077, 0.1053605, 0.3566749, 0.5108256)
expect_equal(
    round(mtr_log_loss(act, pred), 7),
    c(0.1278334, 0.7985077, 0.1053605, 0.3566749, 0.5108256)
)

## Test error ------------------------------------------------------------------

## raise error if outcome is not binary class
expect_error(
    mtr_log_loss(c(0, 1, 2, 0, 1), pred)
)

## raise error if length of input vectors are not equal
expect_error(
    mtr_log_loss(act, pred[-1])
)
