test_that("missing sentiment injected through metadata has an informative error", {
  model <- fit(
    LDA(ECB_press_conferences_tokens[1:5], K = 2),
    1,
    displayProgress = FALSE
  )
  sentopics_sentiment(model) <- seq(-1, 1, length.out = 5)
  values <- model$tokens$.sentiment
  values[1] <- NA_real_
  expect_error(sentopics_sentiment(model) <- values, "NA sentiment")
  quanteda::docvars(model$tokens, ".sentiment") <- values
  expect_error(
    sentopics_sentiment(model),
    "Stored sentiment contains missing values"
  )
  expect_error(
    sentiment_series(model, na.rm = TRUE),
    "Stored sentiment contains missing values"
  )
  expect_error(
    sentiment_breakdown(model),
    "Stored sentiment contains missing values"
  )
  expect_error(
    sentiment_topics(model),
    "Stored sentiment contains missing values"
  )
})

test_that("overriding can repair a missing model-derived sentiment cache", {
  model <- fit(
    JST(ECB_press_conferences_tokens[1:5], K = 2, lexicon = LoughranMcDonald),
    1,
    displayProgress = FALSE
  )
  suppressMessages(sentopics_sentiment(model))
  model$tokens$.sentiment[1] <- NA_real_
  expect_error(sentopics_sentiment(model), "missing values")
  repaired <- suppressMessages(sentopics_sentiment(model, override = TRUE))
  expect_false(anyNA(repaired$.sentiment))
})
