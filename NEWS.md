# sentopics 0.7.2

* Added `as.LDA()` method for outputs from the **keyATM** package.

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
