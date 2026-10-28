test_that("rJST merging recomputes sentiment from pooled counts and priors", {
  set.seed(813)
  model <- fit(
    rJST(
      ECB_press_conferences_tokens[1:5],
      K = 4,
      lexicon = LoughranMcDonald,
      gamma = c(.01, 10, .01)
    ),
    10,
    displayProgress = FALSE
  )
  before <- suppressMessages(sentopics_sentiment(model))$.sentiment
  original <- data.table::copy(model)
  groups <- list(A = 1:2, B = 3:4)
  merged <- suppressMessages(merge_topics(model, groups))
  expect_identical(model, original)
  expect_equal(
    unname(merged$theta),
    unname(cbind(
      A = rowSums(model$theta[, 1:2]),
      B = rowSums(model$theta[, 3:4])
    ))
  )
  counts <- rebuild_zd(as.sentopicsmodel(model))
  for (b in seq_along(groups)) {
    rows <- unlist(lapply(groups[[b]], function(k) {
      (k - 1) * model$S + seq_len(model$S)
    }))
    prior <- rowSums(matrix(model$gamma[rows, 1], nrow = model$S))
    pooled <- vapply(
      seq_along(model$tokens),
      function(d) {
        rowSums(matrix(counts[rows, d], nrow = model$S))
      },
      numeric(model$S)
    )
    expected <- sweep(pooled + prior, 2, colSums(pooled) + sum(prior), "/")
    expect_equal(unname(merged$pi[, b, ]), unname(expected))
    expect_equal(merged$gamma[(b - 1) * model$S + seq_len(model$S), 1], prior)
  }
  after <- suppressMessages(sentopics_sentiment(merged))$.sentiment
  expect_gt(max(abs(after - before)), 1e-8)
  expect_equal(fit(merged, 0, displayProgress = FALSE)$pi, merged$pi)
})
