# Deliberately recompute all conditional probabilities after every removal.
# There is no probability cache in this reference, so it cannot retain a stale
# first-layer factor for another sentiment/topic in the same block.
reference_uncached_sampling <- function(model, iterations) {
  x <- as.sentopicsmodel(model)
  words <- cleanPadding(x$tokens)
  assignments <- data.table::copy(x$za)
  Z <- x$L1 * x$L2
  zd <- matrix(0, Z, length(words))
  zw <- matrix(0, Z, nrow(x$vocabulary))
  for (d in seq_along(words)) {
    for (i in seq_along(words[[d]])) {
      z <- assignments[[d]][i]
      w <- words[[d]][i]
      zd[z, d] <- zd[z, d] + 1
      zw[z, w] <- zw[z, w] + 1
    }
  }
  blocks <- split(seq_len(Z), rep(seq_len(x$L1), each = x$L2))
  sum_beta <- rowSums(x$beta)
  for (iteration in seq_len(iterations)) {
    for (d in seq_along(words)) {
      for (i in seq_along(words[[d]])) {
        old <- assignments[[d]][i]
        w <- words[[d]][i]
        zd[old, d] <- zd[old, d] - 1
        zw[old, w] <- zw[old, w] - 1
        totals <- rowSums(zw)
        weight <- numeric(Z)
        for (t in seq_along(blocks)) {
          rows <- blocks[[t]]
          n <- sum(zd[rows, d])
          if (x$L2 == 1L) {
            factor <- (n + x$L1prior[t, 1]) / (totals[rows] + sum_beta[rows])
          } else {
            first <- (n + x$L1prior[t, 1]) / (n + sum(x$L2prior[rows, 1]))
            factor <- first *
              (zd[rows, d] + x$L2prior[rows, 1]) /
              (totals[rows] + sum_beta[rows])
          }
          weight[rows] <- factor * (zw[rows, w] + x$beta[rows, w])
        }
        # Sequential accumulation matches the native draw's arithmetic order.
        cumulative <- numeric(Z)
        cumulative[1] <- weight[1]
        for (z in 2:Z) {
          cumulative[z] <- cumulative[z - 1] + weight[z]
        }
        sampled <- which(cumulative > stats::runif(1) * cumulative[Z])[1]
        assignments[[d]][i] <- as.integer(sampled)
        zd[sampled, d] <- zd[sampled, d] + 1
        zw[sampled, w] <- zw[sampled, w] + 1
      }
    }
  }
  assignments
}

test_that("cached sampling matches a full conditional recomputation", {
  toks <- quanteda::tokens(c(
    a = "good growth bad outlook good steady growth",
    b = "bad outlook steady bad growth good outlook",
    c = "steady growth good outlook bad steady growth"
  ))
  lex <- quanteda::dictionary(list(negative = "bad", positive = "good"))
  set.seed(12)
  models <- list(
    LDA(toks, K = 3, alpha = c(.2, 1.1, 2.3)),
    JST(
      toks,
      lexicon = lex,
      K = 3,
      S = 2,
      gamma = c(.3, 1.7),
      alpha = c(.2, .8, 2.1)
    ),
    rJST(
      toks,
      lexicon = lex,
      K = 3,
      S = 2,
      alpha = c(.2, .8, 2.1),
      gamma = c(.3, 1.7)
    )
  )
  for (model in models) {
    for (seed in c(17, 81, 209)) {
      set.seed(seed)
      expected <- reference_uncached_sampling(model, 5)
      expected_rng <- .Random.seed
      set.seed(seed)
      actual <- fit(
        model,
        5,
        displayProgress = FALSE,
        computeLikelihood = FALSE
      )
      expect_identical(actual$za, expected)
      expect_identical(.Random.seed, expected_rng)
      # fit(..., 0) reconstructs posterior probabilities from the reference
      # assignments; use the fitted object only for its valid output structure.
      reconstructed <- data.table::copy(actual)
      reconstructed$za <- expected
      reconstructed <- fit(reconstructed, 0, displayProgress = FALSE)
      expect_identical(actual$theta, reconstructed$theta)
      if (!inherits(model, "LDA")) {
        expect_identical(actual$pi, reconstructed$pi)
      }
    }
  }
})
