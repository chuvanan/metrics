% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conf-mat.r
\name{for}
\alias{for}
\alias{mtr_for}
\alias{mtr_false_omission_rate}
\title{False Omission Rate}
\usage{
mtr_for(actual, predicted, cutoff = 0.5)

mtr_false_omission_rate(actual, predicted, cutoff = 0.5)
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
False Omission Rate
}
\author{
An Chu
}
