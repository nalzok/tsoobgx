require(tsoobgx)

context("basic functions")

data(agaricus.train, package='tsoobgx')
data(agaricus.test, package='tsoobgx')
train <- agaricus.train
test <- agaricus.test
set.seed(1994)

# disable some tests for Win32
windows_flag = .Platform$OS.type == "windows" &&
               .Machine$sizeof.pointer != 8
solaris_flag = (Sys.info()['sysname'] == "SunOS")

test_that("train and predict binary classification", {
  nrounds = 2
  expect_output(
    bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2,
                  eta = 1, nthread = 2, nrounds = nrounds, objective = "binary:logistic")
  , "train-error")
  expect_equal(class(bst), "bgx.Booster")
  expect_equal(bst$niter, nrounds)
  expect_false(is.null(bst$evaluation_log))
  expect_equal(nrow(bst$evaluation_log), nrounds)
  expect_lt(bst$evaluation_log[, min(train_error)], 0.03)

  pred <- predict(bst, test$data)
  expect_length(pred, 1611)
  
  pred1 <- predict(bst, train$data, ntreelimit = 1)
  expect_length(pred1, 6513)
  err_pred1 <- sum((pred1 > 0.5) != train$label)/length(train$label)
  err_log <- bst$evaluation_log[1, train_error]
  expect_lt(abs(err_pred1 - err_log), 10e-6)
})

test_that("train and predict softprob", {
  lb <- as.numeric(iris$Species) - 1
  set.seed(11)
  expect_output(
    bst <- tsoobgx(data = as.matrix(iris[, -5]), label = lb,
                   max_depth = 3, eta = 0.5, nthread = 2, nrounds = 5,
                   objective = "multi:softprob", num_class=3)
  , "train-merror")
  expect_false(is.null(bst$evaluation_log))
  expect_lt(bst$evaluation_log[, min(train_merror)], 0.025)
  expect_equal(bst$niter * 3, bgx.ntree(bst))
  pred <- predict(bst, as.matrix(iris[, -5]))
  expect_length(pred, nrow(iris) * 3)
  # row sums add up to total probability of 1:
  expect_equal(rowSums(matrix(pred, ncol=3, byrow=TRUE)), rep(1, nrow(iris)), tolerance = 1e-7)
  # manually calculate error at the last iteration:
  mpred <- predict(bst, as.matrix(iris[, -5]), reshape = TRUE)
  expect_equal(as.numeric(t(mpred)), pred)
  pred_labels <- max.col(mpred) - 1
  err <- sum(pred_labels != lb)/length(lb)
  expect_equal(bst$evaluation_log[5, train_merror], err, tolerance = 5e-6)
  # manually calculate error at the 1st iteration:
  mpred <- predict(bst, as.matrix(iris[, -5]), reshape = TRUE, ntreelimit = 1)
  pred_labels <- max.col(mpred) - 1
  err <- sum(pred_labels != lb)/length(lb)
  expect_equal(bst$evaluation_log[1, train_merror], err, tolerance = 5e-6)
})

test_that("train and predict softmax", {
  lb <- as.numeric(iris$Species) - 1
  set.seed(11)
  expect_output(
    bst <- tsoobgx(data = as.matrix(iris[, -5]), label = lb,
                   max_depth = 3, eta = 0.5, nthread = 2, nrounds = 5,
                   objective = "multi:softmax", num_class=3)
  , "train-merror")
  expect_false(is.null(bst$evaluation_log))
  expect_lt(bst$evaluation_log[, min(train_merror)], 0.025)
  expect_equal(bst$niter * 3, bgx.ntree(bst))
  
  pred <- predict(bst, as.matrix(iris[, -5]))
  expect_length(pred, nrow(iris))
  err <- sum(pred != lb)/length(lb)
  expect_equal(bst$evaluation_log[5, train_merror], err, tolerance = 5e-6)
})

test_that("train and predict RF", {
  set.seed(11)
  lb <- train$label
  # single iteration
  bst <- tsoobgx(data = train$data, label = lb, max_depth = 5,
                 nthread = 2, nrounds = 1, objective = "binary:logistic",
                 num_parallel_tree = 20, subsample = 0.6, colsample_bytree = 0.1)
  expect_equal(bst$niter, 1)
  expect_equal(bgx.ntree(bst), 20)
  
  pred <- predict(bst, train$data)
  pred_err <- sum((pred > 0.5) != lb)/length(lb)
  expect_lt(abs(bst$evaluation_log[1, train_error] - pred_err), 10e-6)
  #expect_lt(pred_err, 0.03)
  
  pred <- predict(bst, train$data, ntreelimit = 20)
  pred_err_20 <- sum((pred > 0.5) != lb)/length(lb)
  expect_equal(pred_err_20, pred_err)

  #pred <- predict(bst, train$data, ntreelimit = 1)
  #pred_err_1 <- sum((pred > 0.5) != lb)/length(lb)
  #expect_lt(pred_err, pred_err_1)
  #expect_lt(pred_err, 0.08)
})

test_that("train and predict RF with softprob", {
  lb <- as.numeric(iris$Species) - 1
  nrounds <- 15
  set.seed(11)
  bst <- tsoobgx(data = as.matrix(iris[, -5]), label = lb,
                 max_depth = 3, eta = 0.9, nthread = 2, nrounds = nrounds,
                 objective = "multi:softprob", num_class=3, verbose = 0,
                 num_parallel_tree = 4, subsample = 0.5, colsample_bytree = 0.5)
  expect_equal(bst$niter, 15)
  expect_equal(bgx.ntree(bst), 15*3*4)
  # predict for all iterations:
  pred <- predict(bst, as.matrix(iris[, -5]), reshape=TRUE)
  expect_equal(dim(pred), c(nrow(iris), 3))
  pred_labels <- max.col(pred) - 1
  err <- sum(pred_labels != lb)/length(lb)
  expect_equal(bst$evaluation_log[nrounds, train_merror], err, tolerance = 5e-6)
  # predict for 7 iterations and adjust for 4 parallel trees per iteration
  pred <- predict(bst, as.matrix(iris[, -5]), reshape=TRUE, ntreelimit = 7 * 4)
  err <- sum((max.col(pred) - 1) != lb)/length(lb)
  expect_equal(bst$evaluation_log[7, train_merror], err, tolerance = 5e-6)
})

test_that("use of multiple eval metrics works", {
  expect_output(
    bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2,
                  eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic",
                  eval_metric = 'error', eval_metric = 'auc', eval_metric = "logloss")
  , "train-error.*train-auc.*train-logloss")
  expect_false(is.null(bst$evaluation_log))
  expect_equal(dim(bst$evaluation_log), c(2, 4))
  expect_equal(colnames(bst$evaluation_log), c("iter", "train_error", "train_auc", "train_logloss"))
})


test_that("training continuation works", {
  dtrain <- bgx.DMatrix(train$data, label = train$label)
  watchlist = list(train=dtrain)
  param <- list(objective = "binary:logistic", max_depth = 2, eta = 1, nthread = 2)

  # for the reference, use 4 iterations at once:
  set.seed(11)
  bst <- bgx.train(param, dtrain, nrounds = 4, watchlist, verbose = 0)
  # first two iterations:
  set.seed(11)
  bst1 <- bgx.train(param, dtrain, nrounds = 2, watchlist, verbose = 0)
  # continue for two more:
  bst2 <- bgx.train(param, dtrain, nrounds = 2, watchlist, verbose = 0, bgx_model = bst1)
  if (!windows_flag && !solaris_flag)
    expect_equal(bst$raw, bst2$raw)
  expect_false(is.null(bst2$evaluation_log))
  expect_equal(dim(bst2$evaluation_log), c(4, 2))
  expect_equal(bst2$evaluation_log, bst$evaluation_log)
  # test continuing from raw model data
  bst2 <- bgx.train(param, dtrain, nrounds = 2, watchlist, verbose = 0, bgx_model = bst1$raw)
  if (!windows_flag && !solaris_flag)
    expect_equal(bst$raw, bst2$raw)
  expect_equal(dim(bst2$evaluation_log), c(2, 2))
  # test continuing from a model in file
  bgx.save(bst1, "tsoobgx.model")
  bst2 <- bgx.train(param, dtrain, nrounds = 2, watchlist, verbose = 0, bgx_model = "tsoobgx.model")
  if (!windows_flag && !solaris_flag)
    expect_equal(bst$raw, bst2$raw)
  expect_equal(dim(bst2$evaluation_log), c(2, 2))
})


test_that("bgx.cv works", {
  set.seed(11)
  expect_output(
    cv <- bgx.cv(data = train$data, label = train$label, max_depth = 2, nfold = 5,
                 eta = 1., nthread = 2, nrounds = 2, objective = "binary:logistic",
                 verbose=TRUE)
  , "train-error:")
  expect_is(cv, 'bgx.cv.synchronous')
  expect_false(is.null(cv$evaluation_log))
  expect_lt(cv$evaluation_log[, min(test_error_mean)], 0.03)
  expect_lt(cv$evaluation_log[, min(test_error_std)], 0.008)
  expect_equal(cv$niter, 2)
  expect_false(is.null(cv$folds) && is.list(cv$folds))
  expect_length(cv$folds, 5)
  expect_false(is.null(cv$params) && is.list(cv$params))
  expect_false(is.null(cv$callbacks))
  expect_false(is.null(cv$call))
})

test_that("train and predict with non-strict classes", {
  # standard dense matrix input
  train_dense <- as.matrix(train$data)
  bst <- tsoobgx(data = train_dense, label = train$label, max_depth = 2,
                 eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 0)
  pr0 <- predict(bst, train_dense)
  
  # dense matrix-like input of non-matrix class
  class(train_dense) <- 'shmatrix'
  expect_true(is.matrix(train_dense))
  expect_error(
    bst <- tsoobgx(data = train_dense, label = train$label, max_depth = 2,
                   eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 0)
    , regexp = NA)
  expect_error(pr <- predict(bst, train_dense), regexp = NA)
  expect_equal(pr0, pr)
  
  # dense matrix-like input of non-matrix class with some inheritance
  class(train_dense) <- c('pphmatrix','shmatrix')
  expect_true(is.matrix(train_dense))
  expect_error(
    bst <- tsoobgx(data = train_dense, label = train$label, max_depth = 2,
                   eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 0)
    , regexp = NA)
  expect_error(pr <- predict(bst, train_dense), regexp = NA)
  expect_equal(pr0, pr)
  
  # when someone inhertis from bgx.Booster, it should still be possible to use it as bgx.Booster
  class(bst) <- c('super.Booster', 'bgx.Booster')
  expect_error(pr <- predict(bst, train_dense), regexp = NA)
  expect_equal(pr0, pr)
})

test_that("max_delta_step works", {
  dtrain <- bgx.DMatrix(agaricus.train$data, label = agaricus.train$label)
  watchlist <- list(train = dtrain)
  param <- list(objective = "binary:logistic", eval_metric="logloss", max_depth = 2, nthread = 2, eta = 0.5)
  nrounds = 5
  # model with no restriction on max_delta_step
  bst1 <- bgx.train(param, dtrain, nrounds, watchlist, verbose = 1)
  # model with restricted max_delta_step
  bst2 <- bgx.train(param, dtrain, nrounds, watchlist, verbose = 1, max_delta_step = 1)
  # the no-restriction model is expected to have consistently lower loss during the initial interations
  expect_true(all(bst1$evaluation_log$train_logloss < bst2$evaluation_log$train_logloss))
  expect_lt(mean(bst1$evaluation_log$train_logloss)/mean(bst2$evaluation_log$train_logloss), 0.8)
})

test_that("colsample_bytree works", {
  # Randomly generate data matrix by sampling from uniform distribution [-1, 1]
  set.seed(1)
  train_x <- matrix(runif(1000, min = -1, max = 1), ncol = 100)
  train_y <- as.numeric(rowSums(train_x) > 0)
  test_x <- matrix(runif(1000, min = -1, max = 1), ncol = 100)
  test_y <- as.numeric(rowSums(test_x) > 0)
  colnames(train_x) <- paste0("Feature_", sprintf("%03d", 1:100))
  colnames(test_x) <- paste0("Feature_", sprintf("%03d", 1:100))
   dtrain <- bgx.DMatrix(train_x, label = train_y)
  dtest <- bgx.DMatrix(test_x, label = test_y)
  watchlist <- list(train = dtrain, eval = dtest)
   # Use colsample_bytree = 0.01, so that roughly one out of 100 features is
  # chosen for each tree
  param <- list(max_depth = 2, eta = 0, silent = 1, nthread = 2,
                colsample_bytree = 0.01, objective = "binary:logistic",
                eval_metric = "auc")
   set.seed(2)
  bst <- bgx.train(param, dtrain, nrounds = 100, watchlist, verbose = 0)
  bgx.importance(model = bst)
  # If colsample_bytree works properly, a variety of features should be used
  # in the 100 trees
  expect_gte(nrow(bgx.importance(model = bst)), 30)
})
