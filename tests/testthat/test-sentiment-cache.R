test_that("continued fits and resets invalidate only model-derived sentiment", {
  toks <- ECB_press_conferences_tokens[1:30]
  for (constructor in list(JST, rJST)) {
    set.seed(41)
    model <- fit(
      constructor(toks, K = 2, lexicon = LoughranMcDonald),
      2,
      displayProgress = FALSE
    )
    cached <- suppressMessages(sentopics_sentiment(model))
    model$tokens$.sentiment_scaled <- rep(99, length(model$tokens))
    original <- data.table::copy(quanteda::docvars(model$tokens))

    continued <- fit(model, 10, displayProgress = FALSE)
    expect_null(continued$tokens$.sentiment)
    expect_null(continued$tokens$.sentiment_scaled)
    expect_false(any(startsWith(
      names(quanteda::docvars(continued$tokens)),
      ".s_"
    )))
    actual <- suppressMessages(sentopics_sentiment(continued))
    expected <- suppressMessages(sentopics_sentiment(
      continued,
      override = TRUE
    ))
    expect_equal(actual, expected)
    expect_identical(quanteda::docvars(model$tokens), original)

    unchanged <- fit(model, 0, displayProgress = FALSE)
    expect_identical(unchanged$tokens$.sentiment, cached$.sentiment)

    restarted <- reset(model)
    expect_null(restarted$tokens$.sentiment)
    restarted <- fit(restarted, 3, displayProgress = FALSE)
    actual <- suppressMessages(sentopics_sentiment(restarted))
    expected <- suppressMessages(sentopics_sentiment(
      restarted,
      override = TRUE
    ))
    expect_equal(actual, expected)

    external <- seq(-1, 1, length.out = length(model$tokens))
    sentopics_sentiment(model) <- external
    continued <- fit(model, 3, displayProgress = FALSE)
    expect_identical(continued$tokens$.sentiment, external)
    expect_identical(reset(model)$tokens$.sentiment, external)
  }
})
