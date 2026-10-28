test_that("chains retain only sentiment columns and share their static core", {
  model <- rJST(
    ECB_press_conferences_tokens[1:40],
    K = 2,
    lexicon = LoughranMcDonald
  )
  chains <- fit(model, 2, nChains = 2, seed = 8, displayProgress = FALSE)
  raw <- unclass(chains)
  expect_true(all(vapply(raw, function(x) is.null(x$tokens), logical(1))))
  expect_true(all(vapply(
    raw,
    function(x) {
      length(attr(attr(x, "sentiment_cache"), "columns")) == 0L
    },
    logical(1)
  )))
  base_vars <- attr(attr(chains, "base")$tokens, "docvars")
  view <- chains[[1]]
  for (name in names(base_vars)) {
    expect_identical(
      data.table::address(attr(view$tokens, "docvars")[[name]]),
      data.table::address(base_vars[[name]])
    )
  }
  size_before <- as.numeric(object.size(chains))
  invisible(suppressMessages(sentiment_series(view, period = "day")))
  cache <- attr(unclass(chains)[[1]], "sentiment_cache")
  cols <- attr(cache, "columns")
  expect_setequal(
    names(cols),
    c(
      ".sentiment",
      ".sentiment_scaled",
      ".s_topic1",
      ".s_topic2",
      ".s_topic1_scaled",
      ".s_topic2_scaled"
    )
  )
  expect_false(any(names(base_vars) %in% names(cols)))
  expect_identical(attr(attr(chains, "base")$tokens, "docvars"), base_vars)
  expect_gt(as.numeric(object.size(chains)), size_before)
  expect_equal(
    sentopics_sentiment(chains[[1]], quiet = TRUE),
    sentopics_sentiment(view, quiet = TRUE)
  )
  expect_null(chains[[2]]$tokens$.sentiment)

  copied <- data.table::copy(chains)
  invisible(suppressMessages(sentiment_series(copied[[2]], period = "day")))
  expect_null(chains[[2]]$tokens$.sentiment)
  expect_false(is.null(copied[[2]]$tokens$.sentiment_scaled))
  restored <- unserialize(serialize(chains, NULL))
  expect_equal(
    sentopics_sentiment(restored[[1]], quiet = TRUE),
    sentopics_sentiment(chains[[1]], quiet = TRUE)
  )
  refitted <- fit(chains, 0, displayProgress = FALSE)
  expect_equal(
    sentopics_sentiment(refitted[[1]], quiet = TRUE),
    sentopics_sentiment(chains[[1]], quiet = TRUE)
  )
  independent <- as.list(chains)
  expect_null(attr(independent[[1]]$tokens, "sentiment_cache"))
  referenced <- as.list(chains, copy = FALSE)
  invisible(suppressMessages(sentiment_series(referenced[[2]], period = "day")))
  expect_equal(
    sentopics_sentiment(chains[[2]], quiet = TRUE),
    sentopics_sentiment(referenced[[2]], quiet = TRUE)
  )
})

test_that("external sentiment and extracted replacement retain value semantics", {
  model <- fit(
    LDA(ECB_press_conferences_tokens[1:40], K = 2),
    1,
    displayProgress = FALSE
  )
  external <- seq(-1, 1, length.out = length(model$tokens))
  sentopics_sentiment(model) <- external
  chains <- fit(model, 1, nChains = 2, displayProgress = FALSE)
  view <- chains[[1]]
  suppressMessages(sentopics_sentiment(view) <- rep(99, length(external)))
  expect_identical(chains[[1]]$tokens$.sentiment, external)
  expect_identical(view$tokens$.sentiment, rep(99, length(external)))
  invisible(sentiment_series(chains[[1]], period = "day"))
  expect_null(chains[[2]]$tokens$.sentiment_scaled)
  refitted <- fit(chains, 1, displayProgress = FALSE)
  expect_identical(refitted[[1]]$tokens$.sentiment, external)
  expect_identical(refitted[[2]]$tokens$.sentiment, external)
})

test_that("legacy chains with stored tokens retain separate caches", {
  model <- rJST(
    ECB_press_conferences_tokens[1:40],
    K = 2,
    lexicon = LoughranMcDonald
  )
  chains <- fit(model, 2, nChains = 2, displayProgress = FALSE)
  legacy <- unclass(chains)
  for (i in seq_along(legacy)) {
    legacy[[i]]$tokens <- data.table::copy(chains[[i]]$tokens)
    attr(legacy[[i]]$tokens, "sentiment_cache") <- NULL
    attr(legacy[[i]], "sentiment_cache") <- NULL
  }
  class(legacy) <- "multi_chains"
  invisible(suppressMessages(sentiment_series(legacy[[1]], period = "day")))
  expect_false(is.null(legacy[[1]]$tokens$.sentiment_scaled))
  expect_null(legacy[[2]]$tokens$.sentiment)
  migrated <- fit(legacy, 1, displayProgress = FALSE)
  expect_true(all(vapply(
    unclass(migrated),
    function(x) is.null(x$tokens),
    logical(1)
  )))
  expect_null(migrated[[1]]$tokens$.sentiment)
  expect_false(is.null(legacy[[1]]$tokens$.sentiment_scaled))
})
