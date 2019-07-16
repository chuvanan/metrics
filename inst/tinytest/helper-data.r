

## Two-class sample data for classification metrics ----------------------------


rm(list = ls())
set.seed(7)
options(scipen = 999)
pred <- runif(1000)
act <- round(pred)
pred[sample(1000, 300)] <- runif(300) # noise

write.csv(data.frame(act, pred),
          file = "two-class-sample-data.csv",
          row.names = FALSE)

## Multi-class sample data for classification metrics --------------------------





## Sample data for regression metrics ------------------------------------------


rm(list = ls())
data(swiss)
fit <- lm(Fertility ~ ., data = swiss)
pred <- predict(fit)

write.csv(data.frame(act = swiss$Fertility,
                     pred = pred),
          file = "regression-sample-data.csv",
          row.names = FALSE)
