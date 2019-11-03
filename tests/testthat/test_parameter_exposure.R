context('Test model params and call are exposed to R')

require(tsoobgx)

data(agaricus.train, package='tsoobgx')
data(agaricus.test, package='tsoobgx')

dtrain <- bgx.DMatrix(agaricus.train$data, label = agaricus.train$label)
dtest <- bgx.DMatrix(agaricus.test$data, label = agaricus.test$label)

bst <- tsoobgx(data = dtrain,
               max_depth = 2,
               eta = 1,
               nrounds = 10,
               nthread = 1,
               verbose = 0,
               objective = "binary:logistic")

test_that("call is exposed to R", {
  expect_false(is.null(bst$call))
  expect_is(bst$call, "call")
})

test_that("params is exposed to R", {
  model_params <- bst$params
  expect_is(model_params, "list")
  expect_equal(model_params$eta, 1)
  expect_equal(model_params$max_depth, 2)
  expect_equal(model_params$objective, "binary:logistic")
})
