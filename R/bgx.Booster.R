# Construct an internal tsoobgx Booster and return a handle to it.
# internal utility function
bgx.Booster.handle <- function(params = list(), cachelist = list(), modelfile = NULL) {
  if (typeof(cachelist) != "list" ||
      !all(vapply(cachelist, inherits, logical(1), what = 'bgx.DMatrix'))) {
    stop("cachelist must be a list of bgx.DMatrix objects")
  }

  handle <- .Call(tsooBGXerCreate_R, cachelist)
  if (!is.null(modelfile)) {
    if (typeof(modelfile) == "character") {
      .Call(tsooBGXerLoadModel_R, handle, modelfile[1])
    } else if (typeof(modelfile) == "raw") {
      .Call(tsooBGXerLoadModelFromRaw_R, handle, modelfile)
    } else if (inherits(modelfile, "bgx.Booster")) {
      bst <- bgx.Booster.complete(modelfile, saveraw = TRUE)
      .Call(tsooBGXerLoadModelFromRaw_R, handle, bst$raw)
    } else {
      stop("modelfile must be either character filename, or raw booster dump, or bgx.Booster object")
    }
  }
  class(handle) <- "bgx.Booster.handle"
  if (length(params) > 0) {
    bgx.parameters(handle) <- params
  }
  return(handle)
}

# Convert bgx.Booster.handle to bgx.Booster
# internal utility function
bgx.handleToBooster <- function(handle, raw = NULL) {
  bst <- list(handle = handle, raw = raw)
  class(bst) <- "bgx.Booster"
  return(bst)
}

# Check whether bgx.Booster.handle is null
# internal utility function
is.null.handle <- function(handle) {
  if (is.null(handle)) return(TRUE)

  if (!identical(class(handle), "bgx.Booster.handle"))
    stop("argument type must be bgx.Booster.handle")

  if (.Call(XGCheckNullPtr_R, handle))
    return(TRUE)

  return(FALSE)
}

# Return a verified to be valid handle out of either bgx.Booster.handle or bgx.Booster
# internal utility function
bgx.get.handle <- function(object) {
  handle <- switch(class(object)[1],
    bgx.Booster = object$handle,
    bgx.Booster.handle = object,
    stop("argument must be of either bgx.Booster or bgx.Booster.handle class")
  )
  if (is.null.handle(handle)) {
    stop("invalid bgx.Booster.handle")
  }
  handle
}

#' Restore missing parts of an incomplete bgx.Booster object.
#'
#' It attempts to complete an \code{bgx.Booster} object by restoring either its missing
#' raw model memory dump (when it has no \code{raw} data but its \code{bgx.Booster.handle} is valid)
#' or its missing internal handle (when its \code{bgx.Booster.handle} is not valid
#' but it has a raw Booster memory dump).
#'
#' @param object object of class \code{bgx.Booster}
#' @param saveraw a flag indicating whether to append \code{raw} Booster memory dump data
#'                when it doesn't already exist.
#'
#' @details
#'
#' While this method is primarily for internal use, it might be useful in some practical situations.
#'
#' E.g., when an \code{bgx.Booster} model is saved as an R object and then is loaded as an R object,
#' its handle (pointer) to an internal tsoobgx model would be invalid. The majority of tsoobgx methods
#' should still work for such a model object since those methods would be using
#' \code{bgx.Booster.complete} internally. However, one might find it to be more efficient to call the
#' \code{bgx.Booster.complete} function explicitly once after loading a model as an R-object.
#' That would prevent further repeated implicit reconstruction of an internal booster model.
#'
#' @return
#' An object of \code{bgx.Booster} class.
#'
#' @examples
#'
#' data(agaricus.train, package='tsoobgx')
#' bst <- tsoobgx(data = agaricus.train$data, label = agaricus.train$label, max_depth = 2,
#'                eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
#' saveRDS(bst, "bgx.model.rds")
#'
#' bst1 <- readRDS("bgx.model.rds")
#' if (file.exists("bgx.model.rds")) file.remove("bgx.model.rds")
#' # the handle is invalid:
#' print(bst1$handle)
#'
#' bst1 <- bgx.Booster.complete(bst1)
#' # now the handle points to a valid internal booster model:
#' print(bst1$handle)
#'
#' @export
bgx.Booster.complete <- function(object, saveraw = TRUE) {
  if (!inherits(object, "bgx.Booster"))
    stop("argument type must be bgx.Booster")

  if (is.null.handle(object$handle)) {
    object$handle <- bgx.Booster.handle(modelfile = object$raw)
  } else {
    if (is.null(object$raw) && saveraw)
      object$raw <- bgx.save.raw(object$handle)
  }
  return(object)
}

#' Predict method for eXtreme Gradient Boosting model
#'
#' Predicted values based on either tsoobgx model or model handle object.
#'
#' @param object Object of class \code{bgx.Booster} or \code{bgx.Booster.handle}
#' @param newdata takes \code{matrix}, \code{dgCMatrix}, local data file or \code{bgx.DMatrix}.
#' @param missing Missing is only used when input is dense matrix. Pick a float value that represents
#'        missing values in data (e.g., sometimes 0 or some other extreme value is used).
#' @param outputmargin whether the prediction should be returned in the for of original untransformed
#'        sum of predictions from boosting iterations' results. E.g., setting \code{outputmargin=TRUE} for
#'        logistic regression would result in predictions for log-odds instead of probabilities.
#' @param ntreelimit limit the number of model's trees or boosting iterations used in prediction (see Details).
#'        It will use all the trees by default (\code{NULL} value).
#' @param predleaf whether predict leaf index.
#' @param predcontrib whether to return feature contributions to individual predictions (see Details).
#' @param approxcontrib whether to use a fast approximation for feature contributions (see Details).
#' @param predinteraction whether to return contributions of feature interactions to individual predictions (see Details).
#' @param reshape whether to reshape the vector of predictions to a matrix form when there are several
#'        prediction outputs per case. This option has no effect when either of predleaf, predcontrib,
#'        or predinteraction flags is TRUE.
#' @param ... Parameters passed to \code{predict.bgx.Booster}
#'
#' @details
#' Note that \code{ntreelimit} is not necessarily equal to the number of boosting iterations
#' and it is not necessarily equal to the number of trees in a model.
#' E.g., in a random forest-like model, \code{ntreelimit} would limit the number of trees.
#' But for multiclass classification, while there are multiple trees per iteration,
#' \code{ntreelimit} limits the number of boosting iterations.
#'
#' Also note that \code{ntreelimit} would currently do nothing for predictions from gblinear,
#' since gblinear doesn't keep its boosting history.
#'
#' One possible practical applications of the \code{predleaf} option is to use the model
#' as a generator of new features which capture non-linearity and interactions,
#' e.g., as implemented in \code{\link{bgx.create.features}}.
#'
#' Setting \code{predcontrib = TRUE} allows to calculate contributions of each feature to
#' individual predictions. For "gblinear" booster, feature contributions are simply linear terms
#' (feature_beta * feature_value). For "gbtree" booster, feature contributions are SHAP
#' values (Lundberg 2017) that sum to the difference between the expected output
#' of the model and the current prediction (where the hessian weights are used to compute the expectations).
#' Setting \code{approxcontrib = TRUE} approximates these values following the idea explained
#' in \url{http://blog.datadive.net/interpreting-random-forests/}.
#'
#' With \code{predinteraction = TRUE}, SHAP values of contributions of interaction of each pair of features
#' are computed. Note that this operation might be rather expensive in terms of compute and memory.
#' Since it quadratically depends on the number of features, it is recommended to perform selection
#' of the most important features first. See below about the format of the returned results.
#'
#' @return
#' For regression or binary classification, it returns a vector of length \code{nrows(newdata)}.
#' For multiclass classification, either a \code{num_class * nrows(newdata)} vector or
#' a \code{(nrows(newdata), num_class)} dimension matrix is returned, depending on
#' the \code{reshape} value.
#'
#' When \code{predleaf = TRUE}, the output is a matrix object with the
#' number of columns corresponding to the number of trees.
#'
#' When \code{predcontrib = TRUE} and it is not a multiclass setting, the output is a matrix object with
#' \code{num_features + 1} columns. The last "+ 1" column in a matrix corresponds to bias.
#' For a multiclass case, a list of \code{num_class} elements is returned, where each element is
#' such a matrix. The contribution values are on the scale of untransformed margin
#' (e.g., for binary classification would mean that the contributions are log-odds deviations from bias).
#'
#' When \code{predinteraction = TRUE} and it is not a multiclass setting, the output is a 3d array with
#' dimensions \code{c(nrow, num_features + 1, num_features + 1)}. The off-diagonal (in the last two dimensions)
#' elements represent different features interaction contributions. The array is symmetric WRT the last
#' two dimensions. The "+ 1" columns corresponds to bias. Summing this array along the last dimension should
#' produce practically the same result as predict with \code{predcontrib = TRUE}.
#' For a multiclass case, a list of \code{num_class} elements is returned, where each element is
#' such an array.
#'
#' @seealso
#' \code{\link{bgx.train}}.
#'
#' @references
#'
#' Scott M. Lundberg, Su-In Lee, "A Unified Approach to Interpreting Model Predictions", NIPS Proceedings 2017, \url{https://arxiv.org/abs/1705.07874}
#'
#' Scott M. Lundberg, Su-In Lee, "Consistent feature attribution for tree ensembles", \url{https://arxiv.org/abs/1706.06060}
#'
#' @examples
#' ## binary classification:
#'
#' data(agaricus.train, package='tsoobgx')
#' data(agaricus.test, package='tsoobgx')
#' train <- agaricus.train
#' test <- agaricus.test
#'
#' bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2,
#'                eta = 0.5, nthread = 2, nrounds = 5, objective = "binary:logistic")
#' # use all trees by default
#' pred <- predict(bst, test$data)
#' # use only the 1st tree
#' pred1 <- predict(bst, test$data, ntreelimit = 1)
#'
#' # Predicting tree leafs:
#' # the result is an nsamples X ntrees matrix
#' pred_leaf <- predict(bst, test$data, predleaf = TRUE)
#' str(pred_leaf)
#'
#' # Predicting feature contributions to predictions:
#' # the result is an nsamples X (nfeatures + 1) matrix
#' pred_contr <- predict(bst, test$data, predcontrib = TRUE)
#' str(pred_contr)
#' # verify that contributions' sums are equal to log-odds of predictions (up to float precision):
#' summary(rowSums(pred_contr) - qlogis(pred))
#' # for the 1st record, let's inspect its features that had non-zero contribution to prediction:
#' contr1 <- pred_contr[1,]
#' contr1 <- contr1[-length(contr1)]    # drop BIAS
#' contr1 <- contr1[contr1 != 0]        # drop non-contributing features
#' contr1 <- contr1[order(abs(contr1))] # order by contribution magnitude
#' old_mar <- par("mar")
#' par(mar = old_mar + c(0,7,0,0))
#' barplot(contr1, horiz = TRUE, las = 2, xlab = "contribution to prediction in log-odds")
#' par(mar = old_mar)
#'
#'
#' ## multiclass classification in iris dataset:
#'
#' lb <- as.numeric(iris$Species) - 1
#' num_class <- 3
#' set.seed(11)
#' bst <- tsoobgx(data = as.matrix(iris[, -5]), label = lb,
#'                max_depth = 4, eta = 0.5, nthread = 2, nrounds = 10, subsample = 0.5,
#'                objective = "multi:softprob", num_class = num_class)
#' # predict for softmax returns num_class probability numbers per case:
#' pred <- predict(bst, as.matrix(iris[, -5]))
#' str(pred)
#' # reshape it to a num_class-columns matrix
#' pred <- matrix(pred, ncol=num_class, byrow=TRUE)
#' # convert the probabilities to softmax labels
#' pred_labels <- max.col(pred) - 1
#' # the following should result in the same error as seen in the last iteration
#' sum(pred_labels != lb)/length(lb)
#'
#' # compare that to the predictions from softmax:
#' set.seed(11)
#' bst <- tsoobgx(data = as.matrix(iris[, -5]), label = lb,
#'                max_depth = 4, eta = 0.5, nthread = 2, nrounds = 10, subsample = 0.5,
#'                objective = "multi:softmax", num_class = num_class)
#' pred <- predict(bst, as.matrix(iris[, -5]))
#' str(pred)
#' all.equal(pred, pred_labels)
#' # prediction from using only 5 iterations should result
#' # in the same error as seen in iteration 5:
#' pred5 <- predict(bst, as.matrix(iris[, -5]), ntreelimit=5)
#' sum(pred5 != lb)/length(lb)
#'
#'
#' ## random forest-like model of 25 trees for binary classification:
#'
#' set.seed(11)
#' bst <- tsoobgx(data = train$data, label = train$label, max_depth = 5,
#'                nthread = 2, nrounds = 1, objective = "binary:logistic",
#'                num_parallel_tree = 25, subsample = 0.6, colsample_bytree = 0.1)
#' # Inspect the prediction error vs number of trees:
#' lb <- test$label
#' dtest <- bgx.DMatrix(test$data, label=lb)
#' err <- sapply(1:25, function(n) {
#'   pred <- predict(bst, dtest, ntreelimit=n)
#'   sum((pred > 0.5) != lb)/length(lb)
#' })
#' plot(err, type='l', ylim=c(0,0.1), xlab='#trees')
#'
#' @rdname predict.bgx.Booster
#' @export
predict.bgx.Booster <- function(object, newdata, missing = NA, outputmargin = FALSE, ntreelimit = NULL,
                                predleaf = FALSE, predcontrib = FALSE, approxcontrib = FALSE, predinteraction = FALSE,
                                reshape = FALSE, ...) {

  object <- bgx.Booster.complete(object, saveraw = FALSE)
  if (!inherits(newdata, "bgx.DMatrix"))
    newdata <- bgx.DMatrix(newdata, missing = missing)
  if (!is.null(object[["feature_names"]]) &&
      !is.null(colnames(newdata)) &&
      !identical(object[["feature_names"]], colnames(newdata)))
    stop("Feature names stored in `object` and `newdata` are different!")
  if (is.null(ntreelimit))
    ntreelimit <- NVL(object$best_ntreelimit, 0)
  if (NVL(object$params[['booster']], '') == 'gblinear')
    ntreelimit <- 0
  if (ntreelimit < 0)
    stop("ntreelimit cannot be negative")

  option <- 0L + 1L * as.logical(outputmargin) + 2L * as.logical(predleaf) + 4L * as.logical(predcontrib) +
    8L * as.logical(approxcontrib) + 16L * as.logical(predinteraction)

  ret <- .Call(tsooBGXerPredict_R, object$handle, newdata, option[1], as.integer(ntreelimit))

  n_ret <- length(ret)
  n_row <- nrow(newdata)
  npred_per_case <- n_ret / n_row

  if (n_ret %% n_row != 0)
    stop("prediction length ", n_ret, " is not multiple of nrows(newdata) ", n_row)

  if (predleaf) {
    ret <- if (n_ret == n_row) {
      matrix(ret, ncol = 1)
    } else {
      matrix(ret, nrow = n_row, byrow = TRUE)
    }
  } else if (predcontrib) {
    n_col1 <- ncol(newdata) + 1
    n_group <- npred_per_case / n_col1
    cnames <- if (!is.null(colnames(newdata))) c(colnames(newdata), "BIAS") else NULL
    ret <- if (n_ret == n_row) {
      matrix(ret, ncol = 1, dimnames = list(NULL, cnames))
    } else if (n_group == 1) {
      matrix(ret, nrow = n_row, byrow = TRUE, dimnames = list(NULL, cnames))
    } else {
      arr <- array(ret, c(n_col1, n_group, n_row),
                   dimnames = list(cnames, NULL, NULL)) %>% aperm(c(2,3,1)) # [group, row, col]
      lapply(seq_len(n_group), function(g) arr[g,,])
    }
  } else if (predinteraction) {
    n_col1 <- ncol(newdata) + 1
    n_group <- npred_per_case / n_col1^2
    cnames <- if (!is.null(colnames(newdata))) c(colnames(newdata), "BIAS") else NULL
    ret <- if (n_ret == n_row) {
      matrix(ret, ncol = 1, dimnames = list(NULL, cnames))
    } else if (n_group == 1) {
      array(ret, c(n_col1, n_col1, n_row), dimnames = list(cnames, cnames, NULL)) %>% aperm(c(3,1,2))
    } else {
      arr <- array(ret, c(n_col1, n_col1, n_group, n_row),
                   dimnames = list(cnames, cnames, NULL, NULL)) %>% aperm(c(3,4,1,2)) # [group, row, col1, col2]
      lapply(seq_len(n_group), function(g) arr[g,,,])
    }
  } else if (reshape && npred_per_case > 1) {
    ret <- matrix(ret, nrow = n_row, byrow = TRUE)
  }
  return(ret)
}

#' @rdname predict.bgx.Booster
#' @export
predict.bgx.Booster.handle <- function(object, ...) {

  bst <- bgx.handleToBooster(object)

  ret <- predict(bst, ...)
  return(ret)
}


#' Accessors for serializable attributes of a model.
#'
#' These methods allow to manipulate the key-value attribute strings of an tsoobgx model.
#'
#' @param object Object of class \code{bgx.Booster} or \code{bgx.Booster.handle}.
#' @param name a non-empty character string specifying which attribute is to be accessed.
#' @param value a value of an attribute for \code{bgx.attr<-}; for \code{bgx.attributes<-}
#'        it's a list (or an object coercible to a list) with the names of attributes to set
#'        and the elements corresponding to attribute values.
#'        Non-character values are converted to character.
#'        When attribute value is not a scalar, only the first index is used.
#'        Use \code{NULL} to remove an attribute.
#'
#' @details
#' The primary purpose of tsoobgx model attributes is to store some meta-data about the model.
#' Note that they are a separate concept from the object attributes in R.
#' Specifically, they refer to key-value strings that can be attached to an tsoobgx model,
#' stored together with the model's binary representation, and accessed later
#' (from R or any other interface).
#' In contrast, any R-attribute assigned to an R-object of \code{bgx.Booster} class
#' would not be saved by \code{bgx.save} because an tsoobgx model is an external memory object
#' and its serialization is handled externally.
#' Also, setting an attribute that has the same name as one of tsoobgx's parameters wouldn't
#' change the value of that parameter for a model.
#' Use \code{\link{bgx.parameters<-}} to set or change model parameters.
#'
#' The attribute setters would usually work more efficiently for \code{bgx.Booster.handle}
#' than for \code{bgx.Booster}, since only just a handle (pointer) would need to be copied.
#' That would only matter if attributes need to be set many times.
#' Note, however, that when feeding a handle of an \code{bgx.Booster} object to the attribute setters,
#' the raw model cache of an \code{bgx.Booster} object would not be automatically updated,
#' and it would be user's responsibility to call \code{bgx.save.raw} to update it.
#'
#' The \code{bgx.attributes<-} setter either updates the existing or adds one or several attributes,
#' but it doesn't delete the other existing attributes.
#'
#' @return
#' \code{bgx.attr} returns either a string value of an attribute
#' or \code{NULL} if an attribute wasn't stored in a model.
#'
#' \code{bgx.attributes} returns a list of all attribute stored in a model
#' or \code{NULL} if a model has no stored attributes.
#'
#' @examples
#' data(agaricus.train, package='tsoobgx')
#' train <- agaricus.train
#'
#' bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2,
#'                eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
#'
#' bgx.attr(bst, "my_attribute") <- "my attribute value"
#' print(bgx.attr(bst, "my_attribute"))
#' bgx.attributes(bst) <- list(a = 123, b = "abc")
#'
#' bgx.save(bst, 'bgx.model')
#' bst1 <- bgx.load('bgx.model')
#' if (file.exists('bgx.model')) file.remove('bgx.model')
#' print(bgx.attr(bst1, "my_attribute"))
#' print(bgx.attributes(bst1))
#'
#' # deletion:
#' bgx.attr(bst1, "my_attribute") <- NULL
#' print(bgx.attributes(bst1))
#' bgx.attributes(bst1) <- list(a = NULL, b = NULL)
#' print(bgx.attributes(bst1))
#'
#' @rdname bgx.attr
#' @export
bgx.attr <- function(object, name) {
  if (is.null(name) || nchar(as.character(name[1])) == 0) stop("invalid attribute name")
  handle <- bgx.get.handle(object)
  .Call(tsooBGXerGetAttr_R, handle, as.character(name[1]))
}

#' @rdname bgx.attr
#' @export
`bgx.attr<-` <- function(object, name, value) {
  if (is.null(name) || nchar(as.character(name[1])) == 0) stop("invalid attribute name")
  handle <- bgx.get.handle(object)
  if (!is.null(value)) {
    # Coerce the elements to be scalar strings.
    # Q: should we warn user about non-scalar elements?
    if (is.numeric(value[1])) {
      value <- format(value[1], digits = 17)
    } else {
      value <- as.character(value[1])
    }
  }
  .Call(tsooBGXerSetAttr_R, handle, as.character(name[1]), value)
  if (is(object, 'bgx.Booster') && !is.null(object$raw)) {
    object$raw <- bgx.save.raw(object$handle)
  }
  object
}

#' @rdname bgx.attr
#' @export
bgx.attributes <- function(object) {
  handle <- bgx.get.handle(object)
  attr_names <- .Call(tsooBGXerGetAttrNames_R, handle)
  if (is.null(attr_names)) return(NULL)
  res <- lapply(attr_names, function(x) {
    .Call(tsooBGXerGetAttr_R, handle, x)
  })
  names(res) <- attr_names
  res
}

#' @rdname bgx.attr
#' @export
`bgx.attributes<-` <- function(object, value) {
  a <- as.list(value)
  if (is.null(names(a)) || any(nchar(names(a)) == 0)) {
    stop("attribute names cannot be empty strings")
  }
  # Coerce the elements to be scalar strings.
  # Q: should we warn a user about non-scalar elements?
  a <- lapply(a, function(x) {
    if (is.null(x)) return(NULL)
    if (is.numeric(x[1])) {
      format(x[1], digits = 17)
    } else {
      as.character(x[1])
    }
  })
  handle <- bgx.get.handle(object)
  for (i in seq_along(a)) {
    .Call(tsooBGXerSetAttr_R, handle, names(a[i]), a[[i]])
  }
  if (is(object, 'bgx.Booster') && !is.null(object$raw)) {
    object$raw <- bgx.save.raw(object$handle)
  }
  object
}

#' Accessors for model parameters.
#'
#' Only the setter for tsoobgx parameters is currently implemented.
#'
#' @param object Object of class \code{bgx.Booster} or \code{bgx.Booster.handle}.
#' @param value a list (or an object coercible to a list) with the names of parameters to set
#'        and the elements corresponding to parameter values.
#'
#' @details
#' Note that the setter would usually work more efficiently for \code{bgx.Booster.handle}
#' than for \code{bgx.Booster}, since only just a handle would need to be copied.
#'
#' @examples
#' data(agaricus.train, package='tsoobgx')
#' train <- agaricus.train
#'
#' bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2,
#'                eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
#'
#' bgx.parameters(bst) <- list(eta = 0.1)
#'
#' @rdname bgx.parameters
#' @export
`bgx.parameters<-` <- function(object, value) {
  if (length(value) == 0) return(object)
  p <- as.list(value)
  if (is.null(names(p)) || any(nchar(names(p)) == 0)) {
    stop("parameter names cannot be empty strings")
  }
  names(p) <- gsub("\\.", "_", names(p))
  p <- lapply(p, function(x) as.character(x)[1])
  handle <- bgx.get.handle(object)
  for (i in seq_along(p)) {
    .Call(tsooBGXerSetParam_R, handle, names(p[i]), p[[i]])
  }
  if (is(object, 'bgx.Booster') && !is.null(object$raw)) {
    object$raw <- bgx.save.raw(object$handle)
  }
  object
}

# Extract the number of trees in a model.
# TODO: either add a getter to C-interface, or simply set an 'ntree' attribute after each iteration.
# internal utility function
bgx.ntree <- function(bst) {
  length(grep('^booster', bgx.dump(bst)))
}


#' Print bgx.Booster
#'
#' Print information about bgx.Booster.
#'
#' @param x an bgx.Booster object
#' @param verbose whether to print detailed data (e.g., attribute values)
#' @param ... not currently used
#'
#' @examples
#' data(agaricus.train, package='tsoobgx')
#' train <- agaricus.train
#' bst <- tsoobgx(data = train$data, label = train$label, max_depth = 2,
#'                eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
#' attr(bst, 'myattr') <- 'memo'
#'
#' print(bst)
#' print(bst, verbose=TRUE)
#'
#' @method print bgx.Booster
#' @export
print.bgx.Booster <- function(x, verbose = FALSE, ...) {
  cat('##### bgx.Booster\n')

  valid_handle <- !is.null.handle(x$handle)
  if (!valid_handle)
    cat("Handle is invalid! Suggest using bgx.Booster.complete\n")

  cat('raw: ')
  if (!is.null(x$raw)) {
    cat(format(object.size(x$raw), units = "auto"), '\n')
  } else {
    cat('NULL\n')
  }
  if (!is.null(x$call)) {
    cat('call:\n  ')
    print(x$call)
  }

  if (!is.null(x$params)) {
    cat('params (as set within bgx.train):\n')
    cat( '  ',
         paste(names(x$params),
               paste0('"', unlist(x$params), '"'),
               sep = ' = ', collapse = ', '), '\n', sep = '')
  }
  # TODO: need an interface to access all the tsoobgxs parameters

  attrs <- character(0)
  if (valid_handle)
    attrs <- bgx.attributes(x)
  if (length(attrs) > 0) {
    cat('bgx.attributes:\n')
    if (verbose) {
      cat( paste(paste0('  ',names(attrs)),
                 paste0('"', unlist(attrs), '"'),
                 sep = ' = ', collapse = '\n'), '\n', sep = '')
    } else {
      cat('  ', paste(names(attrs), collapse = ', '), '\n', sep = '')
    }
  }

  if (!is.null(x$callbacks) && length(x$callbacks) > 0) {
    cat('callbacks:\n')
    lapply(callback.calls(x$callbacks), function(x) {
      cat('  ')
      print(x)
    })
  }

  if (!is.null(x$feature_names))
    cat('# of features:', length(x$feature_names), '\n')

  cat('niter: ', x$niter, '\n', sep = '')
  # TODO: uncomment when faster bgx.ntree is implemented
  #cat('ntree: ', bgx.ntree(x), '\n', sep='')

  for (n in setdiff(names(x), c('handle', 'raw', 'call', 'params', 'callbacks',
                                'evaluation_log','niter','feature_names'))) {
    if (is.atomic(x[[n]])) {
      cat(n, ':', x[[n]], '\n', sep = ' ')
    } else {
      cat(n, ':\n\t', sep = ' ')
      print(x[[n]])
    }
  }

  if (!is.null(x$evaluation_log)) {
    cat('evaluation_log:\n')
    print(x$evaluation_log, row.names = FALSE, topn = 2)
  }

  invisible(x)
}
