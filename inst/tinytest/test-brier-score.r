
## Test correctness ------------------------------------------------------------

act <- rep(1, 4)
pred1 <- rep(1, 4)
pred2 <- rep(0, 4)
pred3 <- c(0, 0, 1, 1)

expect_equal(mtr_brier_score(act, pred1), 0)

expect_equal(mtr_brier_score(act, pred2), 1)

expect_equal(mtr_brier_score(act, pred3), 0.5)

expect_equal(mtr_brier_score(1, 0.4), 0.36)

expect_equal(mtr_brier_score(0, 0.4), 0.16)


## Test error ------------------------------------------------------------------

## raise error if actual is multiclass
expect_error(mtr_brier_score(c(0, 1, 2), c(0, 0, 0)))

## raise error if length of input vectors is inconsistent
expect_error(mtr_brier_score(act, pred1[-1]))

## raise error if predicted probabilities > 1
expect_error(mtr_brier_score(c(0, 1), predicted = c(2, 0.5)))

## raise error if predicted probabilities < 0
expect_error(mtr_brier_score(c(0, 1), predicted = c(-1, 0.5)))

## raise error if actual is different than 0 and 1
expect_error(mtr_brier_score(actual = c(0, -1), c(0.1, 0.1)))
