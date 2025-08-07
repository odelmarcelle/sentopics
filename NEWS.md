# sentopics 1.0.0

### Breaking changes

* Renamed base class `sentopicmodel` to `sentopicsmodel` for consistency
  with the package name.

* Renamed various functions and classes to snake_case style.
   - renamed function `mergeTopics()` to `merge_topics()`
   - renamed function `topWords()` to `top_words()`
   - renamed function `plot_topWords()` to `plot_top_words()`
   - renamed function `chainsScores()` to `chains_scores()`
   - renamed function `chainsDistances()` to `chains_distances()`
   - renamed S3 class `topWords` to `top_words`
   - renamed S3 class `multiChains` to `multi_chains`

* Corrected naming of Reverse Joint Sentiment/Topic model (rJST).
  To align with this change, internal attributes of `sentopicsmodel` objects
  have been renamed from `reversed` to `reverse`.

* Modified names of columns in the `PicaultRenault_data` dataset to
  syntactically valid names.

### Other changes

* Reformatted codebase using Air.
* Removed a comprehensive assessment from the `PicaultRenault_data` dataset
  not present in the original paper.

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
