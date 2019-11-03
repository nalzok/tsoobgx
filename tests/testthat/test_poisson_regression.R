context('Test poisson regression model')

require(tsoobgx)
set.seed(1994)

test_that("poisson regression works", {
  data(mtcars)
  bst <- tsoobgx(data = as.matrix(mtcars[,-11]), label = mtcars[,11],
                objective = 'count:poisson', nrounds=10, verbose=0)
  expect_equal(class(bst), "bgx.Booster")
  pred <- predict(bst, as.matrix(mtcars[, -11]))
  expect_equal(length(pred), 32)
  expect_lt(sqrt(mean( (pred - mtcars[,11])^2 )), 1.2)
})
