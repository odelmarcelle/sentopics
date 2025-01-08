# sentopics 0.7.5

* Fixed a test following change in suggested dependency.

# sentopics 0.7.4

* Fixed a test

# sentopics 0.7.3

* Renamed `grow()` to `fit()` as a more intuitive name. `grow()` remains in the package for compatibility with older version.
* Aligned the weight argument and the algorithm of `topWords(..., method="FREX")` to the original paper.
* Fixed the un-exported function `get_ECB_conferences()` to accommodates changes from the ECB website.
* Fixed the model conversion from newer `seededlda` versions (1.2.0 and subsequent)

# sentopics 0.7.2

* Added `as.LDA()` method for outputs from the **keyATM** package.
* Fixed conflict with `data.table::melt()` when **data.table** was loaded prior to **sentopics**

# sentopics 0.7.1

* Small change of a test sometimes failing on other platforms.

# sentopics 0.7.0

* Improved dependency to older version of R
* Added conversions from other packages (**lda**, **topicmodels** and **stm**)
* Changed the parallel back-end from **doFuture** to **future.apply** (hence removing the `nCores` argument from `grow()`)
* Added a function `LDAvis()` that prepare a dynamic visualization of LDA models using the **LDAvis** package
* Reduced vignettes size
* Bug fix: Fixed an error in `as.tokens.dfm()`

# sentopics 0.6.2

* Bug fix: Corrected an issue with the compilation on newest Linux platforms

# sentopics 0.6.1

* Various documentation updates

# sentopics 0.6.0

* First CRAN release
