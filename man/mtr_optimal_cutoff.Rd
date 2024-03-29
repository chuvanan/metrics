% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimal-cutoff.r
\name{mtr_optimal_cutoff}
\alias{mtr_optimal_cutoff}
\title{Classification Optimal Cutoff}
\usage{
mtr_optimal_cutoff(actual, predicted, objective = c("positive_class",
  "negative_class", "both", "missclassified_error"))
}
\arguments{
\item{actual}{\code{[numeric]} Ground truth binary numeric vector containing
1 for the positive class and 0 for the negative class.}

\item{predicted}{\code{[numeric]} A vector of estimated probabilities.}

\item{objective}{\code{[character]}}
}
\value{
A scalar numeric output
}
\description{
Optimal cutoff
}
\author{
An Chu
}
