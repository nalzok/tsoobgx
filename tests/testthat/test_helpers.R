context('Test helper functions')

require(tsoobgx)
require(data.table)
require(Matrix)
require(vcd, quietly = TRUE)

float_tolerance = 5e-6

# disable some tests for Win32
win32_flag = .Platform$OS.type == "windows" && .Machine$sizeof.pointer != 8

set.seed(1982)
data(Arthritis)
df <- data.table(Arthritis, keep.rownames = F)
df[,AgeDiscret := as.factor(round(Age / 10,0))]
df[,AgeCat := as.factor(ifelse(Age > 30, "Old", "Young"))]
df[,ID := NULL]
sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)
label <- df[, ifelse(Improved == "Marked", 1, 0)]

# binary
nrounds <- 12
bst.Tree <- tsoobgx(data = sparse_matrix, label = label, max_depth = 9,
                    eta = 1, nthread = 2, nrounds = nrounds, verbose = 0,
                    objective = "binary:logistic", booster = "gbtree")

bst.GLM <- tsoobgx(data = sparse_matrix, label = label,
                   eta = 1, nthread = 1, nrounds = nrounds, verbose = 0,
                   objective = "binary:logistic", booster = "gblinear")

feature.names <- colnames(sparse_matrix)

# multiclass
mlabel <- as.numeric(iris$Species) - 1
nclass <- 3
mbst.Tree <- tsoobgx(data = as.matrix(iris[, -5]), label = mlabel, verbose = 0,
                     max_depth = 3, eta = 0.5, nthread = 2, nrounds = nrounds,
                     objective = "multi:softprob", num_class = nclass, base_score = 0)

mbst.GLM <- tsoobgx(data = as.matrix(iris[, -5]), label = mlabel, verbose = 0,
                    booster = "gblinear", eta = 0.1, nthread = 1, nrounds = nrounds,
                    objective = "multi:softprob", num_class = nclass, base_score = 0)


test_that("bgx.dump works", {
  if (!win32_flag)
    expect_length(bgx.dump(bst.Tree), 200)
  dump_file = file.path(tempdir(), 'bgx.model.dump')
  expect_true(bgx.dump(bst.Tree, dump_file, with_stats = T))
  expect_true(file.exists(dump_file))
  expect_gt(file.size(dump_file), 8000)

  # JSON format
  dmp <- bgx.dump(bst.Tree, dump_format = "json")
  expect_length(dmp, 1)
  if (!win32_flag)
    expect_length(grep('nodeid', strsplit(dmp, '\n')[[1]]), 188)
})

test_that("bgx.dump works for gblinear", {
  expect_length(bgx.dump(bst.GLM), 14)
  # also make sure that it works properly for a sparse model where some coefficients
  # are 0 from setting large L1 regularization:
  bst.GLM.sp <- tsoobgx(data = sparse_matrix, label = label, eta = 1, nthread = 2, nrounds = 1,
                        alpha=2, objective = "binary:logistic", booster = "gblinear")
  d.sp <- bgx.dump(bst.GLM.sp)
  expect_length(d.sp, 14)
  expect_gt(sum(d.sp == "0"), 0)

  # JSON format
  dmp <- bgx.dump(bst.GLM.sp, dump_format = "json")
  expect_length(dmp, 1)
  expect_length(grep('\\d', strsplit(dmp, '\n')[[1]]), 11)
})

test_that("predict leafs works", {
  # no error for gbtree
  expect_error(pred_leaf <- predict(bst.Tree, sparse_matrix, predleaf = TRUE), regexp = NA)
  expect_equal(dim(pred_leaf), c(nrow(sparse_matrix), nrounds))
  # error for gblinear
  expect_error(predict(bst.GLM, sparse_matrix, predleaf = TRUE))
})

test_that("predict feature contributions works", {
  # gbtree binary classifier
  expect_error(pred_contr <- predict(bst.Tree, sparse_matrix, predcontrib = TRUE), regexp = NA)
  expect_equal(dim(pred_contr), c(nrow(sparse_matrix), ncol(sparse_matrix) + 1))
  expect_equal(colnames(pred_contr), c(colnames(sparse_matrix), "BIAS"))
  pred <- predict(bst.Tree, sparse_matrix, outputmargin = TRUE)
  expect_lt(max(abs(rowSums(pred_contr) - pred)), 1e-5)
  # must work with data that has no column names
  X <- sparse_matrix
  colnames(X) <- NULL
  expect_error(pred_contr_ <- predict(bst.Tree, X, predcontrib = TRUE), regexp = NA)
  expect_equal(pred_contr, pred_contr_, check.attributes = FALSE,
               tolerance = float_tolerance)

  # gbtree binary classifier (approximate method)
  expect_error(pred_contr <- predict(bst.Tree, sparse_matrix, predcontrib = TRUE, approxcontrib = TRUE), regexp = NA)
  expect_equal(dim(pred_contr), c(nrow(sparse_matrix), ncol(sparse_matrix) + 1))
  expect_equal(colnames(pred_contr), c(colnames(sparse_matrix), "BIAS"))
  pred <- predict(bst.Tree, sparse_matrix, outputmargin = TRUE)
  expect_lt(max(abs(rowSums(pred_contr) - pred)), 1e-5)

  # gblinear binary classifier
  expect_error(pred_contr <- predict(bst.GLM, sparse_matrix, predcontrib = TRUE), regexp = NA)
  expect_equal(dim(pred_contr), c(nrow(sparse_matrix), ncol(sparse_matrix) + 1))
  expect_equal(colnames(pred_contr), c(colnames(sparse_matrix), "BIAS"))
  pred <- predict(bst.GLM, sparse_matrix, outputmargin = TRUE)
  expect_lt(max(abs(rowSums(pred_contr) - pred)), 1e-5)
  # manual calculation of linear terms
  coefs <- bgx.dump(bst.GLM)[-c(1,2,4)] %>% as.numeric
  coefs <- c(coefs[-1], coefs[1]) # intercept must be the last
  pred_contr_manual <- sweep(cbind(sparse_matrix, 1), 2, coefs, FUN="*")
  expect_equal(as.numeric(pred_contr), as.numeric(pred_contr_manual),
               tolerance = float_tolerance)

  # gbtree multiclass
  pred <- predict(mbst.Tree, as.matrix(iris[, -5]), outputmargin = TRUE, reshape = TRUE)
  pred_contr <- predict(mbst.Tree, as.matrix(iris[, -5]), predcontrib = TRUE)
  expect_is(pred_contr, "list")
  expect_length(pred_contr, 3)
  for (g in seq_along(pred_contr)) {
    expect_equal(colnames(pred_contr[[g]]), c(colnames(iris[, -5]), "BIAS"))
    expect_lt(max(abs(rowSums(pred_contr[[g]]) - pred[, g])), 1e-5)
  }

  # gblinear multiclass (set base_score = 0, which is base margin in multiclass)
  pred <- predict(mbst.GLM, as.matrix(iris[, -5]), outputmargin = TRUE, reshape = TRUE)
  pred_contr <- predict(mbst.GLM, as.matrix(iris[, -5]), predcontrib = TRUE)
  expect_length(pred_contr, 3)
  coefs_all <- bgx.dump(mbst.GLM)[-c(1,2,6)] %>% as.numeric %>% matrix(ncol = 3, byrow = TRUE)
  for (g in seq_along(pred_contr)) {
    expect_equal(colnames(pred_contr[[g]]), c(colnames(iris[, -5]), "BIAS"))
    expect_lt(max(abs(rowSums(pred_contr[[g]]) - pred[, g])), float_tolerance)
    # manual calculation of linear terms
    coefs <- c(coefs_all[-1, g], coefs_all[1, g]) # intercept needs to be the last
    pred_contr_manual <- sweep(as.matrix(cbind(iris[,-5], 1)), 2, coefs, FUN="*")
    expect_equal(as.numeric(pred_contr[[g]]), as.numeric(pred_contr_manual),
                 tolerance = float_tolerance)
  }
})

test_that("bgx-attribute functionality", {
  val <- "my attribute value"
  list.val <- list(my_attr=val, a=123, b='ok')
  list.ch <- list.val[order(names(list.val))]
  list.ch <- lapply(list.ch, as.character)
  # note: iter is 0-index in bgx attributes
  list.default <- list(niter = as.character(nrounds - 1))
  list.ch <- c(list.ch, list.default)
  # proper input:
  expect_error(bgx.attr(bst.Tree, NULL))
  expect_error(bgx.attr(val, val))
  # set & get:
  expect_null(bgx.attr(bst.Tree, "asdf"))
  expect_equal(bgx.attributes(bst.Tree), list.default)
  bgx.attr(bst.Tree, "my_attr") <- val
  expect_equal(bgx.attr(bst.Tree, "my_attr"), val)
  bgx.attributes(bst.Tree) <- list.val
  expect_equal(bgx.attributes(bst.Tree), list.ch)
  # serializing:
  bgx.save(bst.Tree, 'bgx.model')
  bst <- bgx.load('bgx.model')
  if (file.exists('bgx.model')) file.remove('bgx.model')
  expect_equal(bgx.attr(bst, "my_attr"), val)
  expect_equal(bgx.attributes(bst), list.ch)
  # deletion:
  bgx.attr(bst, "my_attr") <- NULL
  expect_null(bgx.attr(bst, "my_attr"))
  expect_equal(bgx.attributes(bst), list.ch[c("a", "b", "niter")])
  bgx.attributes(bst) <- list(a=NULL, b=NULL)
  expect_equal(bgx.attributes(bst), list.default)
  bgx.attributes(bst) <- list(niter=NULL)
  expect_null(bgx.attributes(bst))
})

if (grepl('Windows', Sys.info()[['sysname']]) ||
    grepl('Linux', Sys.info()[['sysname']]) ||
    grepl('Darwin', Sys.info()[['sysname']])) {
    test_that("bgx-attribute numeric precision", {
      # check that lossless conversion works with 17 digits
      # numeric -> character -> numeric
      X <- 10^runif(100, -20, 20)
      if (capabilities('long.double')) {
          X2X <- as.numeric(format(X, digits = 17))
          expect_identical(X, X2X)
      }
      # retrieved attributes to be the same as written
      for (x in X) {
        bgx.attr(bst.Tree, "x") <- x
        expect_equal(as.numeric(bgx.attr(bst.Tree, "x")), x, tolerance = float_tolerance)
        bgx.attributes(bst.Tree) <- list(a = "A", b = x)
        expect_equal(as.numeric(bgx.attr(bst.Tree, "b")), x, tolerance = float_tolerance)
      }
    })
}

test_that("bgx.Booster serializing as R object works", {
  saveRDS(bst.Tree, 'bgx.model.rds')
  bst <- readRDS('bgx.model.rds')
  if (file.exists('bgx.model.rds')) file.remove('bgx.model.rds')
  dtrain <- bgx.DMatrix(sparse_matrix, label = label)
  expect_equal(predict(bst.Tree, dtrain), predict(bst, dtrain), tolerance = float_tolerance)
  expect_equal(bgx.dump(bst.Tree), bgx.dump(bst))
  bgx.save(bst, 'bgx.model')
  if (file.exists('bgx.model')) file.remove('bgx.model')
  nil_ptr <- new("externalptr")
  class(nil_ptr) <- "bgx.Booster.handle"
  expect_true(identical(bst$handle, nil_ptr))
  bst <- bgx.Booster.complete(bst)
  expect_true(!identical(bst$handle, nil_ptr))
  expect_equal(predict(bst.Tree, dtrain), predict(bst, dtrain), tolerance = float_tolerance)
})

test_that("bgx.model.dt.tree works with and without feature names", {
  names.dt.trees <- c("Tree", "Node", "ID", "Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover")
  dt.tree <- bgx.model.dt.tree(feature_names = feature.names, model = bst.Tree)
  expect_equal(names.dt.trees, names(dt.tree))
  if (!win32_flag)
    expect_equal(dim(dt.tree), c(188, 10))
  expect_output(str(dt.tree), 'Feature.*\\"Age\\"')

  dt.tree.0 <- bgx.model.dt.tree(model = bst.Tree)
  expect_equal(dt.tree, dt.tree.0)

  # when model contains no feature names:
  bst.Tree.x <- bst.Tree
  bst.Tree.x$feature_names <- NULL
  dt.tree.x <- bgx.model.dt.tree(model = bst.Tree.x)
  expect_output(str(dt.tree.x), 'Feature.*\\"3\\"')
  expect_equal(dt.tree[, -4, with=FALSE], dt.tree.x[, -4, with=FALSE])

  # using integer node ID instead of character
  dt.tree.int <- bgx.model.dt.tree(model = bst.Tree, use_int_id = TRUE)
  expect_equal(as.integer(tstrsplit(dt.tree$Yes, '-')[[2]]), dt.tree.int$Yes)
  expect_equal(as.integer(tstrsplit(dt.tree$No, '-')[[2]]), dt.tree.int$No)
  expect_equal(as.integer(tstrsplit(dt.tree$Missing, '-')[[2]]), dt.tree.int$Missing)
})

test_that("bgx.model.dt.tree throws error for gblinear", {
  expect_error(bgx.model.dt.tree(model = bst.GLM))
})

test_that("bgx.importance works with and without feature names", {
  importance.Tree <- bgx.importance(feature_names = feature.names, model = bst.Tree)
  if (!win32_flag)
    expect_equal(dim(importance.Tree), c(7, 4))
  expect_equal(colnames(importance.Tree), c("Feature", "Gain", "Cover", "Frequency"))
  expect_output(str(importance.Tree), 'Feature.*\\"Age\\"')

  importance.Tree.0 <- bgx.importance(model = bst.Tree)
  expect_equal(importance.Tree, importance.Tree.0, tolerance = float_tolerance)

  # when model contains no feature names:
  bst.Tree.x <- bst.Tree
  bst.Tree.x$feature_names <- NULL
  importance.Tree.x <- bgx.importance(model = bst.Tree)
  expect_equal(importance.Tree[, -1, with=FALSE], importance.Tree.x[, -1, with=FALSE],
               tolerance = float_tolerance)

  imp2plot <- bgx.plot.importance(importance_matrix = importance.Tree)
  expect_equal(colnames(imp2plot), c("Feature", "Gain", "Cover", "Frequency", "Importance"))
  bgx.ggplot.importance(importance_matrix = importance.Tree)

  # for multiclass
  imp.Tree <- bgx.importance(model = mbst.Tree)
  expect_equal(dim(imp.Tree), c(4, 4))
  bgx.importance(model = mbst.Tree, trees = seq(from=0, by=nclass, length.out=nrounds))
})

test_that("bgx.importance works with GLM model", {
  importance.GLM <- bgx.importance(feature_names = feature.names, model = bst.GLM)
  expect_equal(dim(importance.GLM), c(10, 2))
  expect_equal(colnames(importance.GLM), c("Feature", "Weight"))
  bgx.importance(model = bst.GLM)
  imp2plot <- bgx.plot.importance(importance.GLM)
  expect_equal(colnames(imp2plot), c("Feature", "Weight", "Importance"))
  bgx.ggplot.importance(importance.GLM)

  # for multiclass
  imp.GLM <- bgx.importance(model = mbst.GLM)
  expect_equal(dim(imp.GLM), c(12, 3))
  expect_equal(imp.GLM$Class, rep(0:2, each=4))
})

test_that("bgx.model.dt.tree and bgx.importance work with a single split model", {
  bst1 <- tsoobgx(data = sparse_matrix, label = label, max_depth = 1,
                  eta = 1, nthread = 2, nrounds = 1, verbose = 0,
                  objective = "binary:logistic")
  expect_error(dt <- bgx.model.dt.tree(model = bst1), regexp = NA) # no error
  expect_equal(nrow(dt), 3)
  expect_error(imp <- bgx.importance(model = bst1), regexp = NA) # no error
  expect_equal(nrow(imp), 1)
  expect_equal(imp$Gain, 1)
})

test_that("bgx.plot.tree works with and without feature names", {
  bgx.plot.tree(feature_names = feature.names, model = bst.Tree)
  bgx.plot.tree(model = bst.Tree)
})

test_that("bgx.plot.multi.trees works with and without feature names", {
  bgx.plot.multi.trees(model = bst.Tree, feature_names = feature.names, features_keep = 3)
  bgx.plot.multi.trees(model = bst.Tree, features_keep = 3)
})

test_that("bgx.plot.deepness works", {
  d2p <- bgx.plot.deepness(model = bst.Tree)
  expect_equal(colnames(d2p), c("ID", "Tree", "Depth", "Cover", "Weight"))
  bgx.plot.deepness(model = bst.Tree, which = "med.depth")
  bgx.ggplot.deepness(model = bst.Tree)
})

test_that("bgx.plot.shap works", {
  sh <- bgx.plot.shap(data = sparse_matrix, model = bst.Tree, top_n = 2, col = 4)
  expect_equal(names(sh), c("data", "shap_contrib"))
  expect_equal(NCOL(sh$data), 2)
  expect_equal(NCOL(sh$shap_contrib), 2)
})

test_that("check.deprecation works", {
  ttt <- function(a = NNULL, DUMMY=NULL, ...) {
    check.deprecation(...)
    as.list((environment()))
  }
  res <- ttt(a = 1, DUMMY = 2, z = 3)
  expect_equal(res, list(a = 1, DUMMY = 2))
  expect_warning(
    res <- ttt(a = 1, dummy = 22, z = 3)
  , "\'dummy\' is deprecated")
  expect_equal(res, list(a = 1, DUMMY = 22))
  expect_warning(
    res <- ttt(a = 1, dumm = 22, z = 3)
  , "\'dumm\' was partially matched to \'dummy\'")
  expect_equal(res, list(a = 1, DUMMY = 22))
})
