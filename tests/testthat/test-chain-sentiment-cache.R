test_that("chains own independent raw and scaled sentiment caches", {
  for (constructor in list(JST, rJST)) {
    model <- constructor(
      ECB_press_conferences_tokens[1:80],
      K = 2,
      lexicon = LoughranMcDonald
    )
    chains <- fit(model, 10, nChains = 2, seed = 2, displayProgress = FALSE)
    first <- suppressMessages(sentopics_sentiment(chains[[1]], override = TRUE))
    second <- suppressMessages(sentopics_sentiment(
      chains[[2]],
      override = TRUE
    ))
    expect_false(isTRUE(all.equal(first$.sentiment, second$.sentiment)))
    expect_equal(sentopics_sentiment(chains[[1]], quiet = TRUE), first)
    expect_equal(sentopics_sentiment(chains$chain2, quiet = TRUE), second)
    expect_null(attr(chains, "base")$tokens$.sentiment)

    invisible(sentiment_series(chains[[1]], period = "day"))
    expect_false(is.null(chains[[1]]$tokens$.sentiment_scaled))
    expect_null(chains[[2]]$tokens$.sentiment_scaled)

    detached <- as.list(chains)
    detached[[1]]$tokens$.sentiment <- rep(99, length(model$tokens))
    expect_equal(
      sentopics_sentiment(chains[[1]], quiet = TRUE)$.sentiment,
      first$.sentiment
    )

    continued <- fit(chains, 5, displayProgress = FALSE)
    for (i in 1:2) {
      expect_null(continued[[i]]$tokens$.sentiment)
      actual <- suppressMessages(sentopics_sentiment(continued[[i]]))
      expected <- suppressMessages(sentopics_sentiment(
        continued[[i]],
        override = TRUE
      ))
      expect_equal(actual, expected)
    }
    expect_equal(sentopics_sentiment(chains[[2]], quiet = TRUE), second)
  }
})
