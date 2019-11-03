require(tsoobgx)
# load in the agaricus dataset
data(agaricus.train, package='tsoobgx')
data(agaricus.test, package='tsoobgx')
dtrain <- bgx.DMatrix(agaricus.train$data, label = agaricus.train$label)
dtest <- bgx.DMatrix(agaricus.test$data, label = agaricus.test$label)

param <- list(max_depth=2, eta=1, silent=1, objective='binary:logistic')
watchlist <- list(eval = dtest, train = dtrain)
nrounds = 2

# training the model for two rounds
bst = bgx.train(param, dtrain, nrounds, nthread = 2, watchlist)
cat('start testing prediction from first n trees\n')
labels <- getinfo(dtest,'label')

### predict using first 1 tree
ypred1 = predict(bst, dtest, ntreelimit=1)
# by default, we predict using all the trees
ypred2 = predict(bst, dtest)

cat('error of ypred1=', mean(as.numeric(ypred1>0.5)!=labels),'\n')
cat('error of ypred2=', mean(as.numeric(ypred2>0.5)!=labels),'\n')
