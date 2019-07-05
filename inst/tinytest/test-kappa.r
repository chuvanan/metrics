

set.seed(2093)
pred <- runif(1000)
act <- round(pred)
pred[sample(1000, 300)] <- runif(300) # noises

## Test correctness ------------------------------------------------------------

## act_fct <- factor(act, levels = c("1", "0"))
## pred_fct <- factor(ifelse(pred >= 0.5, 1, 0), levels = c("1", "0"))
## caret::confusionMatrix(act_fct, pred_fct)
## kappa=0.692
## irr::kappa2(cbind(act_fct, pred_fct), weight = "unweighted")
## kappa=0.692
expect_equal(mtr_cohen_kappa(act, pred), target = 0.692, tol = 1e-4)


## Test error ------------------------------------------------------------------

## raise error if cutoff is out of range
expect_error(
    mtr_cohen_kappa(act, pred, cutoff = 1.1)
)

## raise error if actual is multi-class
expect_error(
    mtr_cohen_kappa(c(0, 1, 2), c(0.1, 0.2, 0.3))
)

## raise error if actual is different than 0 and 1
expect_error(
    mtr_cohen_kappa(c(0, -1), c(0.1, 0.2))
)
