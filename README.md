
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sentopics

<!-- badges: start -->

[![R-CMD-check](https://github.com/odelmarcelle/sentopics/workflows/R-CMD-check/badge.svg)](https://github.com/odelmarcelle/sentopics/actions)
[![Codecov test
coverage](https://codecov.io/gh/odelmarcelle/sentopics/branch/master/graph/badge.svg?token=V6M82L4ZCX)](https://codecov.io/gh/odelmarcelle/sentopics?branch=master)
<!-- badges: end -->

## How to Install

`sentopics` currently requires the development version of data.table
(1.14.3 or greater).

``` r
if (!require(data.table, quietly = TRUE)) install.packages("data.table")
data.table::update.dev.pkg()
devtools::install_github("odelmarcelle/sentopics") 
```

## Basic usage

Using a sample of speeches from the European Central Bank, a LDA model
is easily created from a list of tokenized texts. See
<https://quanteda.io> for details on `tokens` input objects and
pre-processing functions.

``` r
library(sentopics)
print(ECB_speeches, 2)
# Tokens consisting of 613 documents and 2 docvars.
# text1 :
#  [1] "dialogue"  "world"     ""          ""          "christine" "lagarde"  
#  [7] "president" "ecb"       ""          ""          ""          ""         
# [ ... and 846 more ]
# 
# text2 :
#  [1] "change"     "continuity" "law"        "keynote"    "christine" 
#  [6] "lagarde"    "president"  "ecb"        "ecb"        "legal"     
# [11] "conference" "frankfurt" 
# [ ... and 865 more ]
# 
# [ reached max_ndoc ... 611 more documents ]
set.seed(12)
lda <- LDA(ECB_speeches, K = 4)
lda <- grow(lda, 100)
lda
# A LDA model with 4 topics. Currently grown by 100 Gibbs sampling iterations.
```

There are various way to extract results from the model: it is either
possible to directly access the estimated mixtures from the `lda` object
or to use some helper functions.

``` r
head(lda$theta) # The document-topic distributions
#        topic
# doc_id       topic1     topic2      topic3      topic4
#   text1 0.237903226 0.27822581 0.096774194 0.387096774
#   text2 0.007072136 0.48656294 0.036775106 0.469589816
#   text3 0.027166882 0.28072445 0.002587322 0.689521345
#   text4 0.813259669 0.01878453 0.164640884 0.003314917
#   text5 0.028790787 0.36852207 0.393474088 0.209213052
#   text6 0.872058824 0.02794118 0.097058824 0.002941176
head(melt(lda, include_docvars = TRUE)) # The document-topic in a 'long' format & with meta-data
#     topic        prob      .date    .id .sentiment
#    <fctr>       <num>     <char> <char>      <num>
# 1: topic1 0.237903226 2021-11-29  text1 -0.2516129
# 2: topic1 0.007072136 2021-11-26  text2 -0.5030303
# 3: topic1 0.027166882 2021-11-25  text3 -0.5135135
# 4: topic1 0.813259669 2021-11-24  text4 -0.4813187
# 5: topic1 0.028790787 2021-11-22  text5 -0.2320000
# 6: topic1 0.872058824 2021-11-19  text6 -0.4139535
topWords(lda, output = "matrix") # The most probable words per topic
#       topic1            topic2      topic3            topic4        
#  [1,] "inflation"       "bank"      "monetary_policy" "risk"        
#  [2,] "growth"          "european"  "rate"            "payment"     
#  [3,] "euro_area"       "risk"      "policy"          "bank"        
#  [4,] "economy"         "country"   "interest_rate"   "ecb"         
#  [5,] "price"           "euro_area" "asset"           "market"      
#  [6,] "economic"        "crisis"    "ecb"             "european"    
#  [7,] "monetary_policy" "banking"   "purchase"        "central_bank"
#  [8,] "recovery"        "national"  "bank"            "climate"     
#  [9,] "global"          "market"    "central_bank"    "service"     
# [10,] "firm"            "capital"   "market"          "financial"
```

Two visualization are also implemented: `plot_topWords()` display the
most probable words and `plot()` summarize the topic proportions and
their top words.

``` r
plot(lda)
```

<img src="man/figures/README-plot-lda-1.png" style="display: block; margin: auto;" />

After properly incorporating date and sentiment medata data (if they are
not already present in the `tokens` input), time series functions allows
to study the evolution of topic proportions and related sentiment.

``` r
sentopics_date(lda) <- ECB_speeches$.date
sentopics_sentiment(lda) <- ECB_speeches$.sentiment
head(proportion_topics(lda, period = "month"))
#                topic1    topic2     topic3    topic4
# 2016-01-01 0.13096229 0.3264452 0.10138001 0.4412125
# 2016-02-01 0.23195503 0.3628055 0.21731416 0.1879253
# 2016-03-01 0.09263318 0.4573727 0.09186358 0.3581305
# 2016-04-01 0.23132782 0.3038253 0.29362972 0.1712172
# 2016-05-01 0.30935122 0.3196261 0.24010683 0.1309159
# 2016-06-01 0.18192363 0.3264466 0.16706162 0.3245681
plot_sentiment_breakdown(lda, period = "day", rolling_window = 90)
```

<img src="man/figures/README-series-1.png" width="100%" />
