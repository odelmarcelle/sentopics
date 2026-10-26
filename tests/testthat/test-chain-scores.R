test_that("chain scores use each coherence metric's default window", {
  if (requireNamespace("future", quietly = TRUE)) {
    old_plan <- future::plan(future::sequential)
    on.exit(future::plan(old_plan), add = TRUE)
  }
  tokens <- quanteda::as.tokens(list(
    first = c("alpha", rep("beta", 20), "gamma", rep("delta", 110)),
    second = c("alpha", "gamma", rep("delta", 110), "beta"),
    third = rep(c("beta", "delta"), 70)
  ))
  model <- fit(LDA(tokens, K = 2), 2, nChains = 2, seed = 123,
    displayProgress = FALSE)
  chains <- as.list(model)

  for (window in list(NULL, 5, "boolean")) {
    scores <- if (is.null(window)) chains_scores(model, nWords = 4) else
      chains_scores(model, nWords = 4, window = window)
    for (method in c("C_NPMI", "C_V")) {
      expected <- vapply(chains, function(chain) {
        mean(coherence(chain, nWords = 4, method = method, window = window))
      }, numeric(1))
      expect_equal(unname(unlist(scores[, method])), unname(expected))
    }
  }
  expect_false(isTRUE(all.equal(
    coherence(chains[[1]], nWords = 4, method = "C_NPMI", window = 10),
    coherence(chains[[1]], nWords = 4, method = "C_NPMI", window = 110)
  )))
})
