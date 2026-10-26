test_that("FREX evaluates empirical CDFs at tied probabilities", {
  phi <- cbind(c(.1, .2, .7), c(.2, .4, .4))
  model <- list(phi = array(
    phi,
    dim = c(3, 1, 2),
    dimnames = list(c("alpha", "beta", "gamma"), NULL, NULL)
  ))
  exclusivity <- phi / rowSums(phi)

  for (w in c(0, .3, 1)) {
    scores <- top_words_dt(model, nWords = 3, method = "FREX", w = w)
    for (k in 1:2) {
      expected <- (w / stats::ecdf(exclusivity[, k])(exclusivity[, k]) +
        (1 - w) / stats::ecdf(phi[, k])(phi[, k]))^-1
      actual <- scores[scores$L1 == k, ]
      expect_equal(actual$value[match(rownames(model$phi), actual$word)], expected)
    }
  }
})
