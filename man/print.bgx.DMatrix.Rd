% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bgx.DMatrix.R
\name{print.bgx.DMatrix}
\alias{print.bgx.DMatrix}
\title{Print bgx.DMatrix}
\usage{
\method{print}{bgx.DMatrix}(x, verbose = FALSE, ...)
}
\arguments{
\item{x}{an bgx.DMatrix object}

\item{verbose}{whether to print colnames (when present)}

\item{...}{not currently used}
}
\description{
Print information about bgx.DMatrix.
Currently it displays dimensions and presence of info-fields and colnames.
}
\examples{
data(agaricus.train, package='tsoobgx')
train <- agaricus.train
dtrain <- bgx.DMatrix(train$data, label=train$label)

dtrain
print(dtrain, verbose=TRUE)

}
