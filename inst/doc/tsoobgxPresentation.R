## ----installGithub, eval=FALSE-------------------------------------------
#  install.packages("drat", repos="https://cran.rstudio.com")
#  drat:::addRepo("dmlc")
#  install.packages("tsoobgx", repos="http://dmlc.ml/drat/", type = "source")

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("tsoobgx")

## ----libLoading, results='hold', message=F, warning=F--------------------
require(tsoobgx)

## ----datasetLoading, results='hold', message=F, warning=F----------------
data(agaricus.train, package='tsoobgx')
data(agaricus.test, package='tsoobgx')
train <- agaricus.train
test <- agaricus.test

## ----dataList, message=F, warning=F--------------------------------------
str(train)

## ----dataSize, message=F, warning=F--------------------------------------
dim(train$data)
dim(test$data)

## ----dataClass, message=F, warning=F-------------------------------------
class(train$data)[1]
class(train$label)

## ----trainingSparse, message=F, warning=F--------------------------------
bstSparse <- tsoobgx(data = train$data, label = train$label, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

## ----trainingDense, message=F, warning=F---------------------------------
bstDense <- tsoobgx(data = as.matrix(train$data), label = train$label, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

## ----trainingDmatrix, message=F, warning=F-------------------------------
dtrain <- bgx.DMatrix(data = train$data, label = train$label)
bstDMatrix <- tsoobgx(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

## ----trainingVerbose0, message=T, warning=F------------------------------
# verbose = 0, no message
bst <- tsoobgx(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 0)

## ----trainingVerbose1, message=T, warning=F------------------------------
# verbose = 1, print evaluation metric
bst <- tsoobgx(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 1)

## ----trainingVerbose2, message=T, warning=F------------------------------
# verbose = 2, also print information about tree
bst <- tsoobgx(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 2)

## ----predicting, message=F, warning=F------------------------------------
pred <- predict(bst, test$data)

# size of the prediction vector
print(length(pred))

# limit display of predictions to the first 10
print(head(pred))

## ----predictingTest, message=F, warning=F--------------------------------
prediction <- as.numeric(pred > 0.5)
print(head(prediction))

## ----predictingAverageError, message=F, warning=F------------------------
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

## ----DMatrix, message=F, warning=F---------------------------------------
dtrain <- bgx.DMatrix(data = train$data, label=train$label)
dtest <- bgx.DMatrix(data = test$data, label=test$label)

## ----watchlist, message=F, warning=F-------------------------------------
watchlist <- list(train=dtrain, test=dtest)

bst <- bgx.train(data=dtrain, max_depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, objective = "binary:logistic")

## ----watchlist2, message=F, warning=F------------------------------------
bst <- bgx.train(data=dtrain, max_depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, eval_metric = "error", eval_metric = "logloss", objective = "binary:logistic")

## ----linearBoosting, message=F, warning=F--------------------------------
bst <- bgx.train(data=dtrain, booster = "gblinear", max_depth=2, nthread = 2, nrounds=2, watchlist=watchlist, eval_metric = "error", eval_metric = "logloss", objective = "binary:logistic")

## ----DMatrixSave, message=F, warning=F-----------------------------------
bgx.DMatrix.save(dtrain, "dtrain.buffer")
# to load it in, simply call bgx.DMatrix
dtrain2 <- bgx.DMatrix("dtrain.buffer")
bst <- bgx.train(data=dtrain2, max_depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, objective = "binary:logistic")

## ----DMatrixDel, include=FALSE-------------------------------------------
file.remove("dtrain.buffer")

## ----getinfo, message=F, warning=F---------------------------------------
label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

## ----dump, message=T, warning=F------------------------------------------
bgx.dump(bst, with_stats = T)

## ----saveModel, message=F, warning=F-------------------------------------
# save model to binary local file
bgx.save(bst, "tsoobgx.model")

## ----loadModel, message=F, warning=F-------------------------------------
# load binary model to R
bst2 <- bgx.load("tsoobgx.model")
pred2 <- predict(bst2, test$data)

# And now the test
print(paste("sum(abs(pred2-pred))=", sum(abs(pred2-pred))))

## ----clean, include=FALSE------------------------------------------------
# delete the created model
file.remove("./tsoobgx.model")

## ----saveLoadRBinVectorModel, message=F, warning=F-----------------------
# save model to R's raw vector
rawVec <- bgx.save.raw(bst)

# print class
print(class(rawVec))

# load binary model to R
bst3 <- bgx.load(rawVec)
pred3 <- predict(bst3, test$data)

# pred2 should be identical to pred
print(paste("sum(abs(pred3-pred))=", sum(abs(pred2-pred))))

