

set.seed(101)
pred <- runif(1000)
act <- round(pred)
pred[sample(1000, 500)] <- runif(500)   # noise


## Test AUROC ------------------------------------------------------------------

## Metrics::auc(act, pred)
## 0.7568871
## ROCR::performance(ROCR::prediction(pred, act), "auc")
## 0.7568871
expect_equal(round(mtr_auc_roc(act, pred), 7), 0.7568871)

## Test AUPRC ------------------------------------------------------------------


## PRROC::pr.curve(pred[act == 1], pred[act == 0], curve = FALSE)
## Area under curve (Integral): 0.7149685
## Area under curve (Davis & Goadrich): 0.7149619
## expect_equal(mtr_auc_prc(act, pred), 0.7149685)
