% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression.r
\name{mtr_max_error}
\alias{mtr_max_error}
\title{Max Error}
\usage{
mtr_max_error(actual, predicted)
}
\arguments{
\item{actual}{\code{[numeric]} The ground truth numeric vector.}

\item{predicted}{\code{[numeric]} The predicted numeric vector, where each
element in the vector is a prediction of the corresponding elements in
\code{actual}.}
}
\value{
A numeric scalar output
}
\description{
\code{mtr_max_error} computes the maximum residual error, a metric that
captures the worst error between the predicted value and the true value.
}
\examples{

act <- c(3, 2, 7, 1)
pred <- c(9, 2, 7, 1)
mtr_max_error(act, pred)


}
\author{
An Chu
}
