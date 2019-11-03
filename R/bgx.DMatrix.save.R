#' Save bgx.DMatrix object to binary file
#' 
#' Save bgx.DMatrix object to binary file
#' 
#' @param dmatrix the \code{bgx.DMatrix} object
#' @param fname the name of the file to write.
#' 
#' @examples
#' data(agaricus.train, package='tsoobgx')
#' train <- agaricus.train
#' dtrain <- bgx.DMatrix(train$data, label=train$label)
#' bgx.DMatrix.save(dtrain, 'bgx.DMatrix.data')
#' dtrain <- bgx.DMatrix('bgx.DMatrix.data')
#' if (file.exists('bgx.DMatrix.data')) file.remove('bgx.DMatrix.data')
#' @export
bgx.DMatrix.save <- function(dmatrix, fname) {
  if (typeof(fname) != "character")
    stop("fname must be character")
  if (!inherits(dmatrix, "bgx.DMatrix"))
    stop("dmatrix must be bgx.DMatrix")
  
  .Call(XGDMatrixSaveBinary_R, dmatrix, fname[1], 0L)
  return(TRUE)
}
