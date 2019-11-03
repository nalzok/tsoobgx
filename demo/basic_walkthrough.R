require(tsoobgx)
require(methods)

# we load in the agaricus dataset
# In this example, we are aiming to predict whether a mushroom is edible
data(agaricus.train, package='tsoobgx')
data(agaricus.test, package='tsoobgx')
train <- agaricus.train
test <- agaricus.test
# the loaded data is stored in sparseMatrix, and label is a numeric vector in {0,1}
class(train$label)
class(train$data)

#-------------Basic Training using tsooBGX-----------------
# this is the basic usage of tsoobgx you can put matrix in data field
# note: we are putting in sparse matrix here, tsoobgx naturally handles sparse input
# use sparse matrix when your feature is sparse(e.g. when you are using one-hot encoding vector)
print("Training tsoobgx with sparseMatrix")
bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic")
# alternatively, you can put in dense matrix, i.e. basic R-matrix
print("Training tsoobgx with Matrix")
bst <- tsoobgx(data = as.matrix(train$data), label = train$label, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic")

# you can also put in bgx.DMatrix object, which stores label, data and other meta datas needed for advanced features
print("Training tsoobgx with bgx.DMatrix")
dtrain <- bgx.DMatrix(data = train$data, label = train$label)
bst <- tsoobgx(data = dtrain, max_depth = 2, eta = 1, nrounds = 2, nthread = 2, 
               objective = "binary:logistic")

# Verbose = 0,1,2
print("Train tsoobgx with verbose 0, no message")
bst <- tsoobgx(data = dtrain, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic", verbose = 0)
print("Train tsoobgx with verbose 1, print evaluation metric")
bst <- tsoobgx(data = dtrain, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic", verbose = 1)
print("Train tsoobgx with verbose 2, also print information about tree")
bst <- tsoobgx(data = dtrain, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic", verbose = 2)

# you can also specify data as file path to a LibSVM format input
# since we do not have this file with us, the following line is just for illustration
# bst <- tsoobgx(data = 'agaricus.train.svm', max_depth = 2, eta = 1, nrounds = 2,objective = "binary:logistic")

#--------------------basic prediction using tsoobgx--------------
# you can do prediction using the following line
# you can put in Matrix, sparseMatrix, or bgx.DMatrix 
pred <- predict(bst, test$data)
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

#-------------------save and load models-------------------------
# save model to binary local file
bgx.save(bst, "tsoobgx.model")
# load binary model to R
bst2 <- bgx.load("tsoobgx.model")
pred2 <- predict(bst2, test$data)
# pred2 should be identical to pred
print(paste("sum(abs(pred2-pred))=", sum(abs(pred2-pred))))

# save model to R's raw vector
raw = bgx.save.raw(bst)
# load binary model to R
bst3 <- bgx.load(raw)
pred3 <- predict(bst3, test$data)
# pred3 should be identical to pred
print(paste("sum(abs(pred3-pred))=", sum(abs(pred3-pred))))

#----------------Advanced features --------------
# to use advanced features, we need to put data in bgx.DMatrix
dtrain <- bgx.DMatrix(data = train$data, label=train$label)
dtest <- bgx.DMatrix(data = test$data, label=test$label)
#---------------Using watchlist----------------
# watchlist is a list of bgx.DMatrix, each of them is tagged with name
watchlist <- list(train=dtrain, test=dtest)
# to train with watchlist, use bgx.train, which contains more advanced features
# watchlist allows us to monitor the evaluation result on all data in the list 
print("Train tsoobgx using bgx.train with watchlist")
bst <- bgx.train(data=dtrain, max_depth=2, eta=1, nrounds=2, watchlist=watchlist,
                 nthread = 2, objective = "binary:logistic")
# we can change evaluation metrics, or use multiple evaluation metrics
print("train tsoobgx using bgx.train with watchlist, watch logloss and error")
bst <- bgx.train(data=dtrain, max_depth=2, eta=1, nrounds=2, watchlist=watchlist,
                 eval_metric = "error", eval_metric = "logloss",
                 nthread = 2, objective = "binary:logistic")

# bgx.DMatrix can also be saved using bgx.DMatrix.save
bgx.DMatrix.save(dtrain, "dtrain.buffer")
# to load it in, simply call bgx.DMatrix
dtrain2 <- bgx.DMatrix("dtrain.buffer")
bst <- bgx.train(data=dtrain2, max_depth=2, eta=1, nrounds=2, watchlist=watchlist,
                 nthread = 2, objective = "binary:logistic")
# information can be extracted from bgx.DMatrix using getinfo
label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

# You can dump the tree you learned using bgx.dump into a text file
dump_path = file.path(tempdir(), 'dump.raw.txt')
bgx.dump(bst, dump_path, with_stats = T)

# Finally, you can check which features are the most important.
print("Most important features (look at column Gain):")
imp_matrix <- bgx.importance(feature_names = colnames(train$data), model = bst)
print(imp_matrix)

# Feature importance bar plot by gain
print("Feature importance Plot : ")
print(bgx.plot.importance(importance_matrix = imp_matrix))
