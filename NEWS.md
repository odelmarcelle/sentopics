# sentopics 1.0.2 (unreleased)

* Document the topic-oriented preprocessing of `ECB_press_conferences_tokens`,
  the absence of lexicon negators, and the token requirements for valence
  shifters to affect sentiment. The shipped dataset is unchanged.

* Explain why `merge_topics()` recomputes rJST sentiment from pooled counts
  and priors, and how to aggregate original sentiment contributions instead.
  Add regression coverage for the pooled posterior calculation.

* Reject `rJST(fitted_lda, lexicon = ...)` when lexicon preprocessing changes
  the fitted tokens or vocabulary, with guidance to initialize a new model.

* Reject unsupported sentiment-layer conversions in `as.LDA()` with a clear
  error and document its structural conversion contract.

* Report missing raw sentiment stored through `docvars()` with an actionable
  error, and clarify that `na.rm` does not permit incomplete document sentiment.

* Validate finite positive active priors and integer iteration, layer and cycle
  counts before sampling, preserving the inactive LDA prior and lexicon zeros.

* Document the intentional in-place caching of raw and scaled sentiment,
  including how time-series functions reuse the stored scaling parameters.

* Isolate each chain's raw and scaled sentiment cache while keeping tokens,
  ordinary document variables and fixed priors in a shared core. Accessing
  chains with `[[`, `$`, or `as.list(copy = FALSE)` assembles temporary views;
  `as.list()` produces independent models. Sentiment caches survive extraction
  and zero-iteration fitting and are invalidated by further sampling.

* Invalidate model-derived raw and scaled sentiment after continued fitting
  or reset, while retaining externally assigned sentiment and the input model.

* Corrected tied FREX ranks in `top_words()` to use empirical CDFs,
  and fixed the documented FREX formula.

* Compute `top_words(method = "term-score")` in log space to avoid
  underflow with many topic and sentiment combinations.

* Use metric-specific default coherence windows in `chains_scores()`
  when `window` is not supplied.

* Clarified that `sentiment_topics(period = "identity")` returns topic-weighted
  sentiment contributions for each document, before aggregation.

* Corrected integer count reconstruction when converting **stm**, **topicmodels**
  (variational LDA), and **keyATM** models. The default `count_method = "heuristic"`
  retains the original fast strategy with corrected residual selection and
  unit-count removal. Set `count_method = "optimized"` to minimize squared error
  in reconstructed source probabilities using a slower, deterministic optimizer.
  Both methods preserve required document, word and topic totals.

* Remove the five documented stop words (`a`, `the`, `an`, `of`, `to`)
  before n-gram matching in `compute_PicaultRenault_scores()`. The original
  full stop list remains unavailable. The new `stopwords` argument accepts
  a custom list; `character(0)` disables removal.

# sentopics 1.0.1

* Fixed double topic weighting in rJST sentiment extraction with
  `method = "proportional"`.

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

* Fixed a bias in JST and rJST models caused by cached probabilities.

* Fixed a labeling error when extracting sentiment time series for rJST models.

### Other changes

* Reformatted codebase using Air.
* Removed a comprehensive assessment from the `PicaultRenault_data` dataset
  not present in the original paper.
* Fixed model relabelling with stored scaled sentiment, optional arguments in
  `sentiment_series()`, `melt()` dispatch with current **data.table** versions,
  and JST coherence dimension labels.
* Restored R-backed token and assignment indices after sampling errors, and
  record unevaluated log-likelihoods as `NA` when `computeLikelihood = FALSE`.
* LDA inference performance improvements.
* Miscellaneous test improvements.

# sentopics 0.7.7

* Support quanteda 4.5.0.

# sentopics 0.7.6

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
