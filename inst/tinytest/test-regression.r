

## Test correctness ------------------------------------------------------------

act <- seq(50)
pred <- act + 1

expect_equal(
    mtr_explained_variance(act, pred),
    target = 1
)

expect_equal(
    mtr_mean_squared_error(act, pred),
    target = 1
)

expect_equal(
    mtr_root_mean_squared_error(act, pred),
    target = 1
)

expect_equal(
    mtr_mean_squared_log_error(act, pred),
    mtr_mean_squared_error(log1p(act), log1p(pred))
)

expect_equal(
    mtr_mean_absolute_error(act, pred),
    target = 1
)

expect_equal(
    mtr_median_absolute_error(act, pred),
    target = 1
)

expect_equal(
    mtr_max_error(act, pred),
    target = 1
)

expect_equal(
    mtr_r2(act, pred),
    target = 0.9951981,
    tol = 1e-7
)


## Test exception --------------------------------------------------------------

expect_warning(mtr_r2(1, 0))
