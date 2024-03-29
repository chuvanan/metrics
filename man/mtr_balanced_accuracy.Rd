% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conf-mat.r
\name{mtr_balanced_accuracy}
\alias{mtr_balanced_accuracy}
\title{Balanced Accuracy}
\usage{
mtr_balanced_accuracy(actual, predicted, cutoff = 0.5)
}
\arguments{
\item{actual}{\code{[numeric]} Ground truth binary numeric vector containing
1 for the positive class and 0 for the negative class.}

\item{predicted}{\code{[numeric]} A vector of estimated probabilities.}

\item{cutoff}{\code{[numeric]} A cutoff value for \code{predicted} vector
which classify a sample into a given class. Default value is 0.5}
}
\value{
A numeric scalar output
}
\description{
Balanced Accuracy
}
\seealso{
\code{\link{mtr_accuracy}}
}
\author{
An Chu
}
