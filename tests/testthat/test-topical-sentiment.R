test_that("LDA identity topical sentiment is weighted by topic", {
  model <- fit(LDA(ECB_press_conferences_tokens[c(3, 1, 2)], K = 2), 1,
    displayProgress = FALSE)
  sentopics_labels(model) <- list(topic = c("zeta", "alpha"))
  sentopics_date(model) <- as.Date(c("2020-03-01", "2020-01-01", "2020-02-01"))
  values <- c(.6, -.4, .2)
  sentopics_sentiment(model) <- values

  identity <- sentiment_topics(model, period = "identity", as.xts = FALSE)
  topics <- colnames(model$theta)
  expect_identical(names(identity), c(".id", "date", topics))
  expect_identical(identity$.id, names(model$tokens))
  expect_equal(identity$date, model$tokens$.date)
  expect_equal(unname(as.matrix(identity[, topics, with = FALSE])),
    unname(model$theta * values))
  expect_equal(rowSums(as.matrix(identity[, topics, with = FALSE])), values)

  monthly <- sentiment_topics(model, period = "month", scale = FALSE,
    as.xts = FALSE)
  expect_equal(as.matrix(monthly[match(identity$date, monthly$date), topics,
    with = FALSE]), as.matrix(identity[, topics, with = FALSE]) / model$theta)
})

test_that("rJST identity topical sentiment is weighted once by topic", {
  model <- fit(rJST(ECB_press_conferences_tokens[c(3, 1, 2)], K = 2,
    lexicon = LoughranMcDonald), 1, displayProgress = FALSE)
  sentopics_labels(model) <- list(topic = c("zeta", "alpha"))
  sentopics_date(model) <- as.Date(c("2020-03-01", "2020-01-01", "2020-02-01"))
  conditional <- t(model$pi["positive", , ] - model$pi["negative", , ])
  expected <- model$theta * conditional
  sentiment <- suppressMessages(sentopics_sentiment(model, override = TRUE))

  identity <- sentiment_topics(model, period = "identity", as.xts = FALSE)
  topics <- colnames(model$theta)
  expect_identical(names(identity), c(".id", "date", topics))
  expect_identical(identity$.id, names(model$tokens))
  expect_equal(unname(as.matrix(identity[, topics, with = FALSE])),
    unname(expected))
  expect_equal(rowSums(as.matrix(identity[, topics, with = FALSE])),
    sentiment$.sentiment)

  monthly <- sentiment_topics(model, period = "month", scale = FALSE,
    as.xts = FALSE)
  expect_equal(as.matrix(monthly[match(identity$date, monthly$date), topics,
    with = FALSE]), as.matrix(identity[, topics, with = FALSE]) / model$theta)
})
