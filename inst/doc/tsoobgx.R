## ----knitropts,echo=FALSE,message=FALSE----------------------------------
if (require('knitr')) opts_chunk$set(fig.width = 5, fig.height = 5, fig.align = 'center', tidy = FALSE, warning = FALSE, cache = TRUE)

## ----prelim,echo=FALSE---------------------------------------------------
tsoobgx.version <- packageDescription("tsoobgx")$Version


## ----Training and prediction with iris-----------------------------------
library(tsoobgx)
data(agaricus.train, package='tsoobgx')
data(agaricus.test, package='tsoobgx')
train <- agaricus.train
test <- agaricus.test
bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2, eta = 1, 
               nrounds = 2, objective = "binary:logistic")
bgx.save(bst, 'model.save')
bst = bgx.load('model.save')
pred <- predict(bst, test$data)

## ----Dump Model----------------------------------------------------------
bgx.dump(bst, 'model.dump')

## ----bgx.DMatrix---------------------------------------------------------
dtrain <- bgx.DMatrix(train$data, label = train$label)
class(dtrain)
head(getinfo(dtrain,'label'))

## ----save model----------------------------------------------------------
bgx.DMatrix.save(dtrain, 'bgx.DMatrix')
dtrain = bgx.DMatrix('bgx.DMatrix')

## ----Customized loss function--------------------------------------------
logregobj <- function(preds, dtrain) {
   labels <- getinfo(dtrain, "label")
   preds <- 1/(1 + exp(-preds))
   grad <- preds - labels
   hess <- preds * (1 - preds)
   return(list(grad = grad, hess = hess))
}

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- sqrt(mean((preds-labels)^2))
  return(list(metric = "MSE", value = err))
}

dtest <- bgx.DMatrix(test$data, label = test$label)
watchlist <- list(eval = dtest, train = dtrain)
param <- list(max_depth = 2, eta = 1, silent = 1)

bst <- bgx.train(param, dtrain, nrounds = 2, watchlist, logregobj, evalerror, maximize = FALSE)

## ----Temp file cleaning, include=FALSE-----------------------------------
file.remove("bgx.DMatrix")
file.remove("model.dump")
file.remove("model.save")

