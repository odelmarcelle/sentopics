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

test_that("term scores remain finite across many topic and sentiment cells", {
  words <- c("alpha", "beta", "gamma", "delta")
  phi <- matrix(c(1e-4, 2e-4, 0, 1 - 3e-4), nrow = 4, ncol = 81)
  phi[1, ] <- seq(1e-4, 3e-4, length.out = 81)
  phi[4, ] <- 1 - colSums(phi[1:3, ])
  model <- list(phi = array(
    phi,
    dim = c(4, 3, 27),
    dimnames = list(words, NULL, NULL)
  ))

  scores <- top_words_dt(model, nWords = 4, method = "term-score")
  smoothed <- phi + .Machine$double.eps
  expected <- smoothed * (log(smoothed) - rowMeans(log(smoothed)))
  idx <- cbind(match(scores$word, words), scores$L2 + 3 * (scores$L1 - 1))

  expect_true(all(is.finite(scores$value)))
  expect_equal(scores$value, expected[idx])
})

test_that("log-space term scores agree with the geometric-mean formula", {
  phi <- matrix(c(.1, .3, .6, .2, .3, .5), nrow = 3)
  words <- c("alpha", "beta", "gamma")
  model <- list(phi = array(
    phi,
    dim = c(3, 1, 2),
    dimnames = list(words, NULL, NULL)
  ))
  scores <- top_words_dt(model, nWords = 3, method = "term-score")
  smoothed <- phi + .Machine$double.eps
  expected <- smoothed * log(smoothed / apply(smoothed, 1, prod)^(1 / 2))
  expect_equal(
    scores$value,
    expected[cbind(match(scores$word, words), scores$L1)]
  )
})
