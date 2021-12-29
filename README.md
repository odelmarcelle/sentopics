<!-- badges: start -->
[![R-CMD-check](https://github.com/odelmarcelle/sentopics/workflows/R-CMD-check/badge.svg)](https://github.com/odelmarcelle/sentopics/actions)
[![Codecov test coverage](https://codecov.io/gh/odelmarcelle/sentopics/branch/master/graph/badge.svg?token=V6M82L4ZCX)](https://codecov.io/gh/odelmarcelle/sentopics?branch=master)
<!-- badges: end -->


## How to Install

``` r
devtools::install_github("odelmarcelle/sentopics") 
```

## How to use

Currently, only a simple workflow is documented.

#### Initialize a model from a quanteda tokens object

The data should be imported to the model under a quanteda tokens object.
Model specifications (number of topics, hyperparameters) are fixed at initialization.

``` r
library(quanteda)
library(sentopics)

toks <- tokens(corpus(data_news))

model <- LDA(toks, K = 10, alpha = 0.1)
```

#### Estimate the model

Model is estimated through the `grow()` function, which iterates the Gibbs sampler by a number of steps.

``` r
model <- grow(model, 100)
```

#### Extracting results

Most probables words are extracted through the `topWords()` function. Otherwise, a `melt()` method is
defined and returns a data.table in the long format containig all estimated distributions.

``` r
head(topWords(model))

head(melt(model))
```

#### Incorporating sentiment seed words

For sentiment-based models, a quanteda dictionary can be used to insert sentiment orientation of words.
The dictionary should contain valid positive and negative categories.

``` r
rJST(toks, lexicon = quanteda::data_dictionary_LSD2015)
```


## Improvements/Suggestions/Bugs

- add parameters about convergence tolerance (see e.g. `stm::stm()`)

- probably safer to replace the variable `T` by something else to avoid conflict with `TRUE`

- probably safer to not export a new `melt()` function

