# sentopics 1.0.1

All changes below are internal. Estimates, log-likelihoods and random number
consumption are unchanged: fitted models are bit-identical to those produced by
version 1.0.0 for the same seed.

### Bug fixes

* Fixed memory leaks in the Dirichlet hyperparameter optimization routines.
  `updateAlpha()`, `updateGamma()` and `polya_fit_simple()` allocated their
  working buffers with `new[]` and never released them, leaking on every call.
  This affected models fitted with `alphaCycle` or `gammaCycle` greater than
  zero; the default (`0`, no optimization) was unaffected.

* Fixed an uninitialized member read: `model::init()` assigned the initial
  `beta` to a shadowing local variable, leaving the `initBeta` field of the
  C++ model exposed to R holding indeterminate memory.

* The Dirichlet fixed-point iteration now leaves a hyperparameter at its
  previous value instead of propagating a non-finite or non-positive update,
  and stops early on a degenerate denominator.

* Updated the ECB source URL in the documentation of `ECB_press_conferences`
  and `ECB_press_conferences_tokens` following a change to the ECB website.

### Performance

* Sped up model fitting with the default `computeLikelihood = TRUE` by about a
  fifth (about a quarter at larger `K`). The terms of `p(w | topic, sentiment)`
  that depend only on `beta` are constant while sampling and are now computed
  once per run rather than once per iteration, and the sparsity of the
  topic-word counts is used to skip the majority of the remaining `lgamma()`
  evaluations.

* Sped up the document components of the likelihood by moving the quantities
  that do not vary across documents out of the document loop.

* Sped up Dirichlet hyperparameter optimization by about a quarter. The `sum_h`
  term of the fixed-point iteration does not depend on the dimension being
  updated and is now computed once per iteration rather than once per
  dimension, and zero counts reuse an already available `digamma()` value.

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

* Removed `.sentiment` docvars in `ECB_press_conferences` dataset.

### Other changes

* Reformatted codebase using Air.
* Removed a comprehensive assessment from the `PicaultRenault_data` dataset
  not present in the original paper.

# sentopics 0.7.5

* Fixed an example following change in suggested dependency.

# sentopics 0.7.5

* Fixed a test following change in suggested dependency.

# sentopics 0.7.4

* Fixed a test

# sentopics 0.7.3

* Renamed `grow()` to `fit()` as a more intuitive name. `grow()` remains in the package for compatibility with older version.
* Aligned the weight argument and the algorithm of `topWords(..., method="FREX")` to the original paper.
* Fixed the unexported function `get_ECB_conferences()` to accommodates changes from the ECB website.
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
