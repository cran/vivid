% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vivi.R
\name{vivi}
\alias{vivi}
\title{vivi}
\usage{
vivi(
  data,
  fit,
  response,
  gridSize = 50,
  importanceType = NULL,
  nmax = 500,
  reorder = TRUE,
  class = 1,
  predictFun = NULL,
  normalized = FALSE
)
}
\arguments{
\item{data}{Data frame used for fit.}

\item{fit}{A supervised machine learning model, which understands condvis2::CVpredict}

\item{response}{The name of the response for the fit.}

\item{gridSize}{The size of the grid for evaluating the predictions.}

\item{importanceType}{One of either "\%IncMSE" or "IncNodePurity" for use with randomForest. Or set to equal "agnostic" to override
embedded importance measures and return agnostic importance values.}

\item{nmax}{Maximum number of data rows to consider. Default is 500. Use all rows if NULL.}

\item{reorder}{If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances.}

\item{class}{Category for classification, a factor level, or a number indicating which factor level.}

\item{predictFun}{Function of (fit, data) to extract numeric predictions from fit. Uses condvis2::CVpredict by default, which works for many fit classes.}

\item{normalized}{Should Friedman's H-statistic be normalized or not. Default is FALSE.}
}
\value{
A matrix of interaction values, with importance on the diagonal.
}
\description{
Creates a matrix displaying variable importance on the diagonal
and variable interaction on the off-diagonal.
}
\examples{

aq <- na.omit(airquality)
f <- lm(Ozone ~ ., data = aq)
m <- vivi(fit = f, data = aq, response = "Ozone") # as expected all interactions are zero
viviHeatmap(m)

\donttest{
library(ranger)
rf <- ranger(Species ~ ., data = iris, importance = "impurity", probability = TRUE)
vivi(fit = rf, data = iris, response = "Species")
}
}
