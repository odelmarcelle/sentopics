test_that("rJST sentiment is conditional on topic and weighted once", {
  set.seed(123)
  model <- fit(
    rJST(
      ECB_press_conferences_tokens[c(3, 1, 4, 2)],
      K = 2,
      lexicon = LoughranMcDonald
    ),
    1,
    displayProgress = FALSE
  )
  sentopics_labels(model) <- list(topic = c("zeta", "alpha"))
  sentopics_date(model) <- as.Date(c(
    "2020-02-01",
    "2020-01-01",
    "2020-02-01",
    "2020-01-01"
  ))
  topic_cols <- paste0(".s_", colnames(model$theta))
  positive <- t(model$pi["positive", , ])
  negative <- t(model$pi["negative", , ])

  for (method in c("proportional", "proportionalPol")) {
    conditional <- positive - negative
    if (method == "proportionalPol") {
      conditional <- conditional / (positive + negative)
    }
    expected <- rowSums(model$theta * conditional)
    sentiment <- suppressMessages(sentopics_sentiment(
      model,
      method = method,
      override = TRUE
    ))

    expect_identical(sentiment$.id, names(model$tokens))
    expect_equal(
      unname(as.matrix(sentiment[, topic_cols, with = FALSE])),
      unname(conditional)
    )
    expect_equal(sentiment$.sentiment, unname(expected))

    breakdown <- sentiment_breakdown(
      model,
      period = "month",
      scale = FALSE,
      as.xts = FALSE
    )
    topical <- sentiment_topics(
      model,
      period = "month",
      scale = FALSE,
      as.xts = FALSE
    )
    series <- sentiment_series(
      model,
      period = "month",
      scale = FALSE,
      as.xts = FALSE
    )
    for (i in seq_len(nrow(breakdown))) {
      docs <- model$tokens$.date == breakdown$date[i]
      contributions <- colMeans((model$theta * conditional)[docs, ])
      expect_equal(breakdown$sentiment[i], mean(expected[docs]))
      expect_equal(series$sentiment[i], mean(expected[docs]))
      expect_equal(
        unname(unlist(breakdown[i, colnames(model$theta), with = FALSE])),
        unname(contributions)
      )
      expect_equal(
        unname(unlist(topical[i, colnames(model$theta), with = FALSE])),
        unname(contributions / colMeans(model$theta[docs, ]))
      )
    }
  }

  # Simulate sentiment cached by the old implementation, including scaled data.
  conditional <- positive - negative
  for (k in seq_along(topic_cols)) {
    quanteda::docvars(model$tokens, topic_cols[k]) <-
      model$theta[, k] * conditional[, k]
  }
  old_sentiment <- unname(rowSums(model$theta^2 * conditional))
  model$tokens$.sentiment <- old_sentiment
  model$tokens$.sentiment_scaled <- rep(99, length(old_sentiment))
  model$tokens$.s_zeta_scaled <- rep(99, length(old_sentiment))
  expect_equal(
    sentopics_sentiment(model, quiet = TRUE)$.sentiment,
    old_sentiment
  )
  refreshed <- suppressMessages(sentopics_sentiment(model, override = TRUE))
  expect_equal(
    refreshed$.sentiment,
    unname(rowSums(model$theta * conditional))
  )
  expect_equal(
    unname(as.matrix(refreshed[, topic_cols, with = FALSE])),
    unname(conditional)
  )
  expect_false(any(grepl("_scaled$", names(quanteda::docvars(model$tokens)))))
  expect_equal(sentopics_sentiment(model, quiet = TRUE), refreshed)
})

test_that("equal rJST topic sentiment is not attenuated by topic proportions", {
  model <- fit(
    rJST(ECB_press_conferences_tokens[1:2], K = 2, lexicon = LoughranMcDonald),
    1,
    displayProgress = FALSE
  )
  # Controlled posterior: both topics have conditional sentiment 0.4.
  model$theta[,] <- 0.5
  model$pi["negative", , ] <- 0.1
  model$pi["neutral", , ] <- 0.4
  model$pi["positive", , ] <- 0.5

  sentiment <- suppressMessages(sentopics_sentiment(model))
  expect_equal(sentiment$.s_topic1, c(0.4, 0.4))
  expect_equal(sentiment$.s_topic2, c(0.4, 0.4))
  expect_equal(sentiment$.sentiment, c(0.4, 0.4))
})
