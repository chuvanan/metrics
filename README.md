<!-- [![Travis build status](https://travis-ci.org/chuvanan/metrics.svg?branch=master)](https://travis-ci.org/chuvanan/metrics) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/chuvanan/metrics/branch/master/graph/badge.svg)](https://codecov.io/gh/chuvanan/metrics?branch=master) -->
<!-- [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) -->
metrics
-------

### Why another package for evaluating machine learning models?

Because I believe there’s still a niche for an R package that have all
the following traits in one place:

-   *Simple*

-   *Consistent interface*

-   *Well-documented*

-   *Well-tested*

-   *Accurate and fast*

Why do I think so? While doing my evaluation work on a machine learning
project I found that there’s no single R package is on a par with
scikit-learn’s `metrics` module in term of coverage, ease of use,
throughout testing and documentaion richness. I’m not saying that these
packages are terrible, they’re designed for a very specific use
case(s)/problem(s) with varying quality.

-   The two major framework for doing machine learning in R are `caret`
    and `mlr(3)`. The next generation of `caret` is `tinymodels` of
    which `parnship` is the main package for performance metrics.

-   `pROC`, `precrec`

-   `InformationValue`

-   `Metrics`, `ModelMetrics`

### Overview of `metrics`

#### Installation

Install the stable version of `metrics` from CRAN:

    install.packages("metrics")

Or install the development version from Github with:

    devtools::install_github("chuvanan/metrics")

#### Getting started

All `metrics` functions share the same interface:
`mtr_fun(actual, predicted)` which is applicable to both classification
and regression settings.

-   `mtr_` is the short form of **m**e**tr**ics. As in `stringr`
    package, `metrics` uses the prefix to provide consistent naming that
    makes it easy to type with autocompletion in RStuido or Emacs’s ESS.

-   `_fun` is the name of performance metrics. The package declares
    verbosely which measure is going to be used. For a full list of
    evaluation metrics, please see TODO.

-   `metrics` package prefers convention over configuration. Argument
    `actual`, in context of classification tasks, stricly accepts binary
    values `0` and `1` where the former is negative class and the later
    is positive one.

Here’s a quick example:

    library(metrics)

    ## simulate sample data set
    set.seed(123)
    preds <- runif(1000)
    truth <- round(preds)
    preds[sample(1000, 300)] <- runif(300) # noise

    ## overall accuracy
    mtr_accuracy(truth, preds)              # default threshold is 0.5

    ## [1] 0.838

    ## precision
    mtr_precision(truth, preds)

    ## [1] 0.82643

    ## recall
    mtr_recall(truth, preds)

    ## [1] 0.8498986

    ## AUROC
    mtr_auc_roc(truth, preds)

    ## [1] 0.8260939
