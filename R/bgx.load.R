#' Load tsoobgx model from binary file
#' 
#' Load tsoobgx model from the binary model file. 
#' 
#' @param modelfile the name of the binary input file.
#' 
#' @details 
#' The input file is expected to contain a model saved in an tsoobgx-internal binary format
#' using either \code{\link{bgx.save}} or \code{\link{cb.save.model}} in R, or using some 
#' appropriate methods from other tsoobgx interfaces. E.g., a model trained in Python and 
#' saved from there in tsoobgx format, could be loaded from R.
#' 
#' Note: a model saved as an R-object, has to be loaded using corresponding R-methods,
#' not \code{bgx.load}.
#' 
#' @return 
#' An object of \code{bgx.Booster} class.
#' 
#' @seealso 
#' \code{\link{bgx.save}}, \code{\link{bgx.Booster.complete}}. 
#' 
#' @examples
#' data(agaricus.train, package='tsoobgx')
#' data(agaricus.test, package='tsoobgx')
#' train <- agaricus.train
#' test <- agaricus.test
#' bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2, 
#'                eta = 1, nthread = 2, nrounds = 2,objective = "binary:logistic")
#' bgx.save(bst, 'bgx.model')
#' bst <- bgx.load('bgx.model')
#' if (file.exists('bgx.model')) file.remove('bgx.model')
#' pred <- predict(bst, test$data)
#' @export
bgx.load <- function(modelfile) {
  if (is.null(modelfile))
    stop("bgx.load: modelfile cannot be NULL")

  handle <- bgx.Booster.handle(modelfile = modelfile)
  # re-use modelfile if it is raw so we do not need to serialize
  if (typeof(modelfile) == "raw") {
    bst <- bgx.handleToBooster(handle, modelfile)
  } else {
    bst <- bgx.handleToBooster(handle, NULL)
  }
  bst <- bgx.Booster.complete(bst, saveraw = TRUE)
  return(bst)
}
