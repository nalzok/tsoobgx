#' Save tsoobgx model to R's raw vector,
#' user can call bgx.load to load the model back from raw vector
#' 
#' Save tsoobgx model from tsoobgx or bgx.train
#' 
#' @param model the model object.
#' 
#' @examples
#' data(agaricus.train, package='tsoobgx')
#' data(agaricus.test, package='tsoobgx')
#' train <- agaricus.train
#' test <- agaricus.test
#' bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2, 
#'                eta = 1, nthread = 2, nrounds = 2,objective = "binary:logistic")
#' raw <- bgx.save.raw(bst)
#' bst <- bgx.load(raw)
#' pred <- predict(bst, test$data)
#'
#' @export
bgx.save.raw <- function(model) {
  model <- bgx.get.handle(model)
  .Call(tsooBGXerModelToRaw_R, model)
}
