% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression.r
\name{mtr_root_mean_squared_error}
\alias{mtr_root_mean_squared_error}
\title{Root Mean Squared Error Function}
\usage{
mtr_root_mean_squared_error(actual, predicted)
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
test
}
\examples{

act <- c(3, -0.5, 2, 7)
pred <- c(2.5, 0.0, 2, 8)
mtr_mean_squared_error(act, pred)


}
\author{
An Chu
}
