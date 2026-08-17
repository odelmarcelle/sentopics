toks <- ECB_press_conferences_tokens
scores <- compute_PicaultRenault_scores(ECB_press_conferences)
jst <- fit(JST(toks, lexicon = LoughranMcDonald), 1, displayProgress = FALSE)
EC <- scores[names(jst$tokens), "EC"]

jst$tokens$.sentiment <- EC
sentopics_date(jst) <- NULL
sentopics_sentiment(jst) <- NULL

test_that("removal works", {
  expect_null(jst$tokens$.date)
  expect_null(jst$tokens$.sentiment)
})

test_that("sentiment replacement clears stale sentiment docvars", {
  for (value in list(NULL, seq_along(toks[1:10]))) {
    model <- rJST(toks[1:10], lexicon = LoughranMcDonald)
    model$tokens$.sentiment <- seq_along(model$tokens)
    model$tokens$.sentiment_scaled <- seq_along(model$tokens) + 10
    model$tokens$.s_topic1 <- seq_along(model$tokens) + 20
    model$tokens$.s_topic1_scaled <- seq_along(model$tokens) + 30

    sentopics_sentiment(model) <- value
    docvars <- quanteda::docvars(model$tokens)

    expect_false(any(grepl("_scaled$", names(docvars))))
    expect_false(any(startsWith(names(docvars), ".s_")))
  }
})


test_that("sentiment works", {
  expect_message(
    sent <- sentopics_sentiment(jst, method = "proportionalPol"),
    "Sentiment computed and assigned"
  )

  expect_true(all(is.finite(sent$.sentiment)))

  expect_message(
    sent <- sentopics_sentiment(jst, override = TRUE),
    "Sentiment computed and assigned"
  )

  sentopics_sentiment(jst) <- EC

  expect_identical(
    sentopics_sentiment(jst),
    data.table(
      .id = names(toks),
      .sentiment = EC
    )
  )

  jst <- JST(toks)
  expect_error(sentopics_sentiment(jst))
  sentopics_sentiment(jst) <- NULL
  expect_error(sentopics_sentiment(jst))

  lda <- LDA(toks)
  sentopics_sentiment(lda) <- NULL
  expect_error(sentopics_sentiment(lda))

  rjst <- fit(
    rJST(toks, lexicon = LoughranMcDonald),
    1,
    displayProgress = FALSE
  )
  expect_message(
    sent <- sentopics_sentiment(rjst),
    "Sentiment computed and assigned"
  )
  expect_true(all(is.finite(sent$.sentiment)))
})

test_that("date works", {
  expect_error(sentopics_date(jst), "No dates stored")

  sentopics_date(jst) <- ECB_press_conferences_tokens$.date
  expect_message(sentopics_date(jst) <- ECB_press_conferences_tokens$.date)

  expect_identical(
    sentopics_date(jst),
    data.table(
      .id = names(toks),
      .date = as.Date(ECB_press_conferences_tokens$.date)
    )
  )
})

test_that("partial labels retain their semantic dimensions", {
  model <- rJST(toks[1:10], K = 2, S = 3, lexicon = LoughranMcDonald)
  sentiment_labels <- c("negative", "neutral", "positive")

  sentopics_labels(model) <- list(sentiment = sentiment_labels)
  labels <- sentopics_labels(model, flat = FALSE)

  expect_identical(labels$topic, paste0("topic", 1:2))
  expect_identical(labels$sentiment, sentiment_labels)
})

test_that("relabeling preserves raw and scaled sentiment docvars", {
  model <- rJST(toks[1:10], K = 2, lexicon = LoughranMcDonald)
  model$tokens$.s_topic1 <- seq_along(model$tokens)
  model$tokens$.s_topic2 <- seq_along(model$tokens) + 10
  model$tokens$.s_topic1_scaled <- seq_along(model$tokens) + 20
  model$tokens$.s_topic2_scaled <- seq_along(model$tokens) + 30

  sentopics_labels(model) <- list(topic = c("first", "second"))
  docvars <- quanteda::docvars(model$tokens)
  sentiment_columns <- names(docvars)[startsWith(names(docvars), ".s_")]

  expect_identical(
    sentiment_columns,
    c(".s_first", ".s_second", ".s_first_scaled", ".s_second_scaled")
  )
  expect_identical(docvars$.s_first_scaled, seq_along(model$tokens) + 20)
  expect_identical(docvars$.s_second_scaled, seq_along(model$tokens) + 30)
})

test_that("internal rJST sentiment follows theta topic order", {
  model <- fit(
    rJST(toks[1:10], K = 3, lexicon = LoughranMcDonald),
    1,
    displayProgress = FALSE
  )
  sentopics_labels(model) <- list(topic = c("zeta", "alpha", "mu"))

  sentiment <- sentopics_sentiment(model, include_docvars = TRUE)
  topic_cols <- paste0(".s_", colnames(model$theta))
  expected <- rowSums(
    as.matrix(sentiment[, topic_cols, with = FALSE]) * model$theta
  )

  expect_equal(sentiment$.sentiment, expected)
})

test_that("internal rJST sentiment follows numeric topic order for K >= 10", {
  model <- fit(
    rJST(toks[1:20], K = 12, lexicon = LoughranMcDonald),
    1,
    displayProgress = FALSE
  )

  sentiment <- sentopics_sentiment(model, include_docvars = TRUE)
  topic_cols <- paste0(".s_", colnames(model$theta))
  expected <- rowSums(
    as.matrix(sentiment[, topic_cols, with = FALSE]) * model$theta
  )

  expect_equal(sentiment$.sentiment, expected)
})

sentopics_date(jst) <- ECB_press_conferences_tokens$.date

test_that("sentiment_series works", {
  skip_if_not_installed("xts")
  jst <- fit(JST(toks, lexicon = LoughranMcDonald), 1, displayProgress = FALSE)
  s2 <- sentiment_series(jst)
  sentopics_sentiment(jst) <- EC
  s1_1 <- sentiment_series(jst)
  expect_false(isTRUE(all.equal(s1_1, s2, check.attributes = FALSE)))

  rjst <- fit(
    rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald),
    1,
    displayProgress = FALSE
  )
  sentopics_labels(jst) <- list(topic = paste0("superTopic", 1:jst$K))
  s2 <- sentiment_series(rjst)
  sentopics_sentiment(rjst) <- EC
  s1_2 <- sentiment_series(rjst)
  expect_false(isTRUE(all.equal(s1_2, s2, check.attributes = FALSE)))

  lda <- fit(LDA(ECB_press_conferences_tokens), 1, displayProgress = FALSE)
  sentopics_sentiment(lda) <- EC
  s1_3 <- sentiment_series(lda)

  expect_identical(s1_1, s1_2)
  expect_identical(s1_1, s1_3)

  expect_s3_class(s1_1, "xts")
  expect_s3_class(sentiment_series(lda, as.xts = FALSE), "data.frame")
  expect_silent(
    sentiment_series(
      lda,
      as.xts = FALSE,
      na.rm = FALSE,
      trim = 0.1
    )
  )

  sentiment_series(lda, period = "day", rolling_window = 30, as.xts = FALSE)

  lda <- LDA(ECB_press_conferences_tokens[1:50])
  expect_error(sentiment_series(lda))
  expect_error(sentiment_series(lda, period = "day"))
})


test_that("series functions works for LDA", {
  skip_if_not_installed("xts")
  lda <- LDA(ECB_press_conferences_tokens)
  sentopics_sentiment(lda) <- EC
  expect_silent(res <- sentiment_series(lda))
  expect_equal(mean(res), 0)
  expect_equal(sd(res), 1)
  lda <- fit(lda, 1, displayProgress = FALSE)

  expect_silent(sentiment_topics(lda))
  scores <- compute_PicaultRenault_scores(ECB_press_conferences)
  sentopics_sentiment(lda) <- scores[names(lda$tokens), "EC"]
  expect_silent(breakdown <- sentiment_breakdown(lda))
  expect_silent(proportion_topics(lda))

  expect_equal(as.matrix(res$sentiment), as.matrix(breakdown$sentiment))

  # check plots
  plot_sentiment_topics(lda, period = "day", rolling_window = 30)
  plot_sentiment_topics(lda, plot_ridgelines = FALSE)
  plot_sentiment_breakdown(lda, period = "day", rolling_window = 30)
  plot_proportion_topics(lda, period = "day", rolling_window = 30)
  plot_proportion_topics(lda, plot_ridgelines = FALSE)
})

test_that("identity period returns document-level breakdowns", {
  model <- fit(LDA(toks[1:10]), 1, displayProgress = FALSE)
  sentopics_sentiment(model) <- seq_along(model$tokens)
  sentopics_date(model) <- as.Date("2020-01-01") + seq_along(model$tokens) - 1

  result <- sentiment_breakdown(
    model,
    period = "identity",
    as.xts = FALSE
  )
  topic_cols <- sentopics_labels(model, flat = FALSE)[["topic"]]

  expect_equal(nrow(result), length(model$tokens))
  expect_equal(result$sentiment, seq_along(model$tokens))
  expect_equal(
    rowSums(as.matrix(result[, topic_cols, with = FALSE])),
    result$sentiment
  )
})

test_that("series functions works for rJST", {
  rjst <- rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
  rjst <- fit(rjst, 10, displayProgress = FALSE)
  sentopics_sentiment(rjst) <- EC

  expect_warning(sentiment_breakdown(rjst))
  expect_warning(sentiment_topics(rjst))

  expect_silent(complete <- proportion_topics(rjst))
  expect_silent(not_complete <- proportion_topics(rjst, complete = FALSE))

  agg <- matrix(0, nrow(complete), rjst$K)
  for (i in seq_len(rjst$K)) {
    agg[, i] <- rowSums(complete[, seq_len(rjst$S) + (i - 1) * rjst$S])
  }
  expect_equal(agg, unclass(not_complete), check.attributes = FALSE)

  sentopics_sentiment(rjst, override = TRUE)
  sentnames <- names(sentopics_sentiment(rjst))
  sentnames <- sub("^\\.s_", "", sentnames[grepl("^\\.s_", sentnames)])
  expect_equal(
    sentopics_labels(rjst, flat = FALSE)$topic,
    sentnames
  )

  ## check if changing labels propagate to stored sentiment
  sentopics_labels(rjst) <- list(
    topic = paste0("superTopic", 1:jst$K),
    sentiment = c("negative", "neutral", "positive")
  )
  sentnames <- names(sentopics_sentiment(rjst))
  sentnames <- sub("^\\.s_", "", sentnames[grepl("^\\.s_", sentnames)])
  expect_equal(
    sentopics_labels(rjst, flat = FALSE)$topic,
    sentnames
  )

  expect_silent(sentiment_breakdown(rjst))
  plot_sentiment_breakdown(rjst, scale = FALSE)

  expect_silent(sentiment_topics(rjst))
  plot_sentiment_topics(rjst, period = "month", scale = FALSE)
  plot_sentiment_topics(
    rjst,
    period = "month",
    scale = FALSE,
    plot_ridgelines = FALSE
  )
})


test_that("series functions works for rJST - internal sentiment", {
  rjst <- rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
  rjst <- fit(rjst, 10, displayProgress = FALSE)

  expect_message(sentiment_breakdown(rjst), "Sentiment computed and assigned")
  expect_silent(sentiment_topics(rjst))

  expect_silent(complete <- proportion_topics(rjst))
  expect_silent(not_complete <- proportion_topics(rjst, complete = FALSE))

  agg <- matrix(0, nrow(complete), rjst$K)
  for (i in seq_len(rjst$K)) {
    agg[, i] <- rowSums(complete[, seq_len(rjst$S) + (i - 1) * rjst$S])
  }
  expect_equal(agg, unclass(not_complete), check.attributes = FALSE)

  sentopics_sentiment(rjst, override = TRUE)
  sentnames <- names(sentopics_sentiment(rjst))
  sentnames <- sub("^\\.s_", "", sentnames[grepl("^\\.s_", sentnames)])
  expect_equal(
    sentopics_labels(rjst, flat = FALSE)$topic,
    sentnames
  )

  ## check if changing labels propagate to stored sentiment
  sentopics_labels(rjst) <- list(
    topic = paste0("superTopic", 1:jst$K),
    sentiment = c("negative", "neutral", "positive")
  )
  sentnames <- names(sentopics_sentiment(rjst))
  sentnames <- sub("^\\.s_", "", sentnames[grepl("^\\.s_", sentnames)])
  expect_equal(
    sentopics_labels(rjst, flat = FALSE)$topic,
    sentnames
  )

  expect_silent(sentiment_breakdown(rjst))
  plot_sentiment_breakdown(rjst, scale = FALSE)

  expect_silent(sentiment_topics(rjst))
  plot_sentiment_topics(rjst, period = "month", scale = FALSE)
  plot_sentiment_topics(
    rjst,
    period = "month",
    scale = FALSE,
    plot_ridgelines = FALSE
  )
})


test_that("series functions works for JST", {
  skip_if_not_installed("xts")
  jst <- JST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
  jst <- fit(jst, 10, displayProgress = FALSE)

  expect_error(sentiment_breakdown(jst))
  expect_error(sentiment_topics(jst))

  expect_silent(complete <- proportion_topics(jst))
  expect_silent(not_complete <- proportion_topics(jst, complete = FALSE))

  agg <- matrix(0, nrow(complete), jst$S)
  for (i in seq_len(jst$S)) {
    agg[, i] <- rowSums(complete[, seq_len(jst$K) + (i - 1) * jst$K])
  }
  expect_equal(agg, unclass(not_complete), check.attributes = FALSE)

  plot_proportion_topics(jst)
  plot_proportion_topics(jst, complete = FALSE)
  plot_proportion_topics(jst, plot_ridgelines = FALSE)
  plot_proportion_topics(jst, complete = FALSE, plot_ridgelines = FALSE)
})
