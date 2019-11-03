#' Save tsoobgx model to binary file
#' 
#' Save tsoobgx model to a file in binary format.
#' 
#' @param model model object of \code{bgx.Booster} class.
#' @param fname name of the file to write.
#' 
#' @details 
#' This methods allows to save a model in an tsoobgx-internal binary format which is universal 
#' among the various tsoobgx interfaces. In R, the saved model file could be read-in later
#' using either the \code{\link{bgx.load}} function or the \code{bgx_model} parameter 
#' of \code{\link{bgx.train}}.
#' 
#' Note: a model can also be saved as an R-object (e.g., by using \code{\link[base]{readRDS}} 
#' or \code{\link[base]{save}}). However, it would then only be compatible with R, and 
#' corresponding R-methods would need to be used to load it.
#' 
#' @seealso 
#' \code{\link{bgx.load}}, \code{\link{bgx.Booster.complete}}. 
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
bgx.save <- function(model, fname) {
  if (typeof(fname) != "character")
    stop("fname must be character")
  if (!inherits(model, "bgx.Booster")) {
    stop("model must be bgx.Booster.",
         if (inherits(model, "bgx.DMatrix")) " Use bgx.DMatrix.save to save an bgx.DMatrix object." else "")
  }
  model <- bgx.Booster.complete(model, saveraw = FALSE)
  .Call(tsooBGXerSaveModel_R, model$handle, fname[1])
  return(TRUE)
}
