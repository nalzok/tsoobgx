require(tsoobgx)

context("Garbage Collection Safety Check")

test_that("train and prediction when gctorture is on", {
  data(agaricus.train, package='tsoobgx')
  data(agaricus.test, package='tsoobgx')
  train <- agaricus.train
  test <- agaricus.test
  gctorture(TRUE)
  bst <- tsoobgx(data = train$data, label = train$label, max.depth = 2,
  eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
  pred <- predict(bst, test$data)
  gctorture(FALSE)
})
