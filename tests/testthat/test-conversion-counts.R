test_that("the default uses the corrected original heuristic with reproducible refill", {
  expected <- matrix(2 / 3, nrow = 3, ncol = 3)
  set.seed(321)
  before <- .Random.seed
  default <- round_posterior_counts(expected, rep(2, 3), rep(2, 3))
  expect_false(identical(.Random.seed, before))
  set.seed(321)
  explicit <- round_posterior_counts(
    expected,
    rep(2, 3),
    rep(2, 3),
    method = "heuristic"
  )
  expect_identical(default, explicit)
  expect_equal(rowSums(default), rep(2, 3))
  expect_equal(colSums(default), rep(2, 3))
})

test_that("the heuristic corrects residual selection and permits unit donors", {
  expect_identical(
    round_posterior_counts(matrix(c(.4, .4, .2), ncol = 1), 1),
    matrix(c(1L, 0L, 0L), ncol = 1)
  )
  counts <- round_posterior_counts(matrix(.6, nrow = 10, ncol = 1), 5)
  expect_equal(colSums(counts), 5)
  expect_true(all(counts >= 0L))
  expect_identical(
    round_posterior_counts(rbind(c(1, 3), c(1, 1)), c(2, 4), c(0, 6)),
    rbind(c(0L, 0L), c(2L, 4L))
  )
})

test_that("both methods preserve count margins on legitimate inputs", {
  set.seed(742)
  for (case in 1:30) {
    nr <- sample(1:6, 1)
    nc <- sample(1:8, 1)
    original <- matrix(sample(0:10, nr * nc, replace = TRUE), nr)
    expected <- matrix(rexp(nr * nc) - .1, nr)
    for (method in c("heuristic", "optimized")) {
      counts <- round_posterior_counts(
        expected,
        colSums(original),
        rowSums(original),
        method = method
      )
      expect_equal(rowSums(counts), rowSums(original))
      expect_equal(colSums(counts), colSums(original))
      expect_type(counts, "integer")
      expect_true(all(counts >= 0L))
    }
  }
  for (method in c("heuristic", "optimized")) {
    expect_identical(
      round_posterior_counts(
        matrix(0, 2, 3),
        rep(0, 3),
        rep(0, 2),
        method = method
      ),
      matrix(0L, 2, 3)
    )
    expect_identical(
      round_posterior_counts(
        matrix(c(.1, .2, .7), ncol = 1),
        5,
        c(0, 2, 3),
        method = method
      ),
      matrix(c(0L, 2L, 3L), ncol = 1)
    )
  }
})

test_that("one-margin rounding chooses nearest feasible counts", {
  counts <- round_posterior_counts(
    matrix(c(.4, .4, .2), ncol = 1),
    1,
    method = "optimized"
  )
  expect_identical(counts, matrix(c(1L, 0L, 0L), ncol = 1))
  counts <- round_posterior_counts(
    matrix(c(.6, .6, .8), ncol = 1),
    2,
    method = "optimized"
  )
  expect_identical(counts, matrix(c(1L, 0L, 1L), ncol = 1))
})

test_that("column allocation minimizes error without rescaling the source", {
  counts <- round_posterior_counts(
    matrix(c(5, 1), ncol = 1),
    10,
    method = "optimized"
  )
  expect_identical(counts, matrix(c(7L, 3L), ncol = 1))
})

test_that("joint rounding minimizes total error beyond greedy feasibility", {
  expected <- rbind(c(.4, .5, .1), c(.2, .4, .4), c(.4, .1, .5))
  counts <- round_posterior_counts(
    expected,
    rep(1, 3),
    rep(1, 3),
    method = "optimized"
  )
  expect_equal(sum((counts - expected)^2), 1.6)
})

test_that("probability weights prioritize source distributions over raw counts", {
  expected <- matrix(c(.8, .2, 6.2, 2.8), nrow = 2)
  weights <- matrix(c(1, 1, 1 / 81, 1 / 81), nrow = 2)
  counts <- round_posterior_counts(
    expected,
    c(1, 9),
    c(5, 5),
    weights,
    method = "optimized"
  )
  unweighted <- round_posterior_counts(
    expected,
    c(1, 9),
    c(5, 5),
    method = "optimized"
  )
  expect_identical(counts, matrix(c(1L, 0L, 4L, 5L), nrow = 2))
  expect_identical(unweighted, matrix(c(0L, 1L, 5L, 4L), nrow = 2))
  expect_lt(
    sum(weights * (counts - expected)^2),
    sum(weights * (unweighted - expected)^2)
  )
})

test_that("weighted transportation agrees with exhaustive feasible tables", {
  compositions <- function(total, size) {
    if (size == 1L) {
      return(matrix(total, ncol = 1))
    }
    do.call(
      rbind,
      lapply(0:total, function(first) {
        cbind(first, compositions(total - first, size - 1L))
      })
    )
  }
  set.seed(728)
  for (case in 1:80) {
    nr <- sample(2:3, 1)
    nc <- sample(2:3, 1)
    original <- matrix(sample(0:2, nr * nc, replace = TRUE), nr)
    expected <- matrix(runif(nr * nc, -.5, 4), nr)
    weights <- matrix(10^runif(nr * nc, -6, 2), nr)
    row_totals <- rowSums(original)
    col_totals <- colSums(original)
    options <- lapply(col_totals, compositions, size = nr)
    best <- Inf
    enumerate <- function(j, left, value) {
      if (j > nc) {
        if (all(left == 0)) {
          best <<- min(best, value)
        }
        return(invisible(NULL))
      }
      for (k in seq_len(nrow(options[[j]]))) {
        candidate <- options[[j]][k, ]
        if (any(candidate > left)) {
          next
        }
        enumerate(
          j + 1L,
          left - candidate,
          value + sum(weights[, j] * (candidate - expected[, j])^2)
        )
      }
    }
    enumerate(1L, row_totals, 0)
    counts <- round_posterior_counts(
      expected,
      col_totals,
      row_totals,
      weights,
      method = "optimized"
    )
    expect_equal(sum(weights * (counts - expected)^2), best, tolerance = 1e-10)
  }
})

test_that("integer transportation preserves both count margins deterministically", {
  expected <- matrix(c(.7, .3, .3, .7), nrow = 2)
  expect_equal(
    round_posterior_counts(expected, c(1, 1), c(1, 1), method = "optimized"),
    diag(2)
  )

  expected <- matrix(c(0, .1, .2, 0, 4, .6, .8, 2, 0, .3, .1, 0), nrow = 3)
  set.seed(123)
  rng <- .Random.seed
  counts <- round_posterior_counts(
    expected,
    c(0, 2, 1, 2),
    c(4, 1, 0),
    method = "optimized"
  )
  expect_equal(rowSums(counts), c(4, 1, 0))
  expect_equal(colSums(counts), c(0, 2, 1, 2))
  expect_type(counts, "integer")
  expect_true(all(counts >= 0))
  expect_identical(.Random.seed, rng)
  expect_identical(
    counts,
    round_posterior_counts(
      expected,
      c(0, 2, 1, 2),
      c(4, 1, 0),
      method = "optimized"
    )
  )

  for (i in 1:30) {
    original <- matrix(sample(0:10, 35, replace = TRUE), nrow = 5)
    expected <- matrix(rexp(35), nrow = 5)
    counts <- round_posterior_counts(
      expected,
      colSums(original),
      rowSums(original),
      method = "optimized"
    )
    expect_equal(rowSums(counts), rowSums(original))
    expect_equal(colSums(counts), colSums(original))
    expect_true(all(counts >= 0))
  }
})

test_that("feasible fractional tables are rounded without unnecessary error", {
  expected <- matrix(2 / 3, nrow = 3, ncol = 3)
  counts <- round_posterior_counts(
    expected,
    rep(2, 3),
    rep(2, 3),
    method = "optimized"
  )
  expect_equal(rowSums(counts), rep(2, 3))
  expect_equal(colSums(counts), rep(2, 3))
  expect_true(all(counts >= floor(expected)))
  expect_true(all(counts <= ceiling(expected)))
  expect_equal(sum((counts - expected)^2), 2)

  set.seed(246)
  for (i in 1:30) {
    original <- matrix(sample(0:10, 35, replace = TRUE), nrow = 5)
    tables <- stats::r2dtable(3, rowSums(original), colSums(original))
    expected <- (tables[[1]] + tables[[2]] + tables[[3]]) / 3
    counts <- round_posterior_counts(
      expected,
      colSums(original),
      rowSums(original),
      method = "optimized"
    )
    expect_equal(rowSums(counts), rowSums(original))
    expect_equal(colSums(counts), colSums(original))
    expect_true(all(counts >= 0L))
    expect_lte(sum((counts - expected)^2), sum((tables[[1]] - expected)^2))
  }
})

test_that("feasible zero and integer cells are preserved", {
  expected <- matrix(c(.7, .3, 0, .3, .7, 0, 0, 0, 2), nrow = 3)
  counts <- round_posterior_counts(
    expected,
    c(1, 1, 2),
    c(1, 1, 2),
    method = "optimized"
  )
  expect_equal(counts[expected == 0], rep(0, sum(expected == 0)))
  expect_equal(counts[expected == 2], 2)
  expect_equal(rowSums(counts), c(1, 1, 2))
  expect_equal(colSums(counts), c(1, 1, 2))
  expect_equal(
    round_posterior_counts(diag(3), rep(1, 3), rep(1, 3), method = "optimized"),
    diag(3)
  )
})

test_that("zero margins and singleton dimensions retain their shape", {
  expect_identical(
    round_posterior_counts(
      matrix(0, 2, 3),
      rep(0, 3),
      rep(0, 2),
      method = "optimized"
    ),
    matrix(0L, 2, 3)
  )
  expect_identical(
    round_posterior_counts(
      matrix(c(.1, .2, .7), nrow = 1),
      c(0, 2, 3),
      5,
      method = "optimized"
    ),
    matrix(c(0L, 2L, 3L), nrow = 1)
  )
  expect_identical(
    round_posterior_counts(
      matrix(c(.1, .2, .7), ncol = 1),
      5,
      c(0, 2, 3),
      method = "optimized"
    ),
    matrix(c(0L, 2L, 3L), ncol = 1)
  )
})

test_that("STM conversion allocates a single token to a most probable topic", {
  x <- structure(
    list(
      settings = list(dim = list(K = 3, wcounts = list(x = c(1, 1)))),
      theta = matrix(c(.4, .4, .2), nrow = 2, ncol = 3, byrow = TRUE),
      beta = list(logbeta = list(matrix(log(.5), nrow = 3, ncol = 2))),
      vocab = c("alpha", "beta"),
      convergence = list(its = 1, bound = 0)
    ),
    class = "STM"
  )
  model <- as.LDA(
    x,
    docs = list(matrix(c(1L, 1L), nrow = 2), matrix(c(2L, 1L), nrow = 2))
  )
  expect_identical(unname(model$zd), matrix(c(1L, 0L, 0L), nrow = 3, ncol = 2))
  expect_identical(unname(model$zw), unname(model$zd))
  expect_equal(unname(model$theta), x$theta)
})

test_that("VEM conversion allocates a single token to a most probable topic", {
  skip_if_not_installed("topicmodels")
  x <- methods::new(
    "LDA_VEM",
    k = 3L,
    alpha = .1,
    gamma = matrix(c(.4, .4, .2), nrow = 2, ncol = 3, byrow = TRUE),
    beta = matrix(log(.5), nrow = 3, ncol = 2),
    iter = 1L
  )
  docs <- quanteda::dfm(quanteda::as.tokens(list("alpha", "beta")))
  model <- as.LDA(x, docs = docs)
  expect_identical(unname(model$zd), matrix(c(1L, 0L, 0L), nrow = 3, ncol = 2))
  expect_identical(unname(model$zw), unname(model$zd))
  expect_equal(unname(model$theta), x@gamma)
})

test_that("STM and VEM conversions minimize source word probability error", {
  phi <- rbind(c(.8, .2), c(6.2, 2.8) / 9)
  theta <- rbind(c(1.1, .1) / 1.2, c(.1, 9.1) / 9.2)
  expected <- matrix(c(1L, 4L, 0L, 5L), nrow = 2)
  x <- structure(
    list(
      settings = list(dim = list(K = 2, wcounts = list(x = c(5, 5)))),
      theta = theta,
      beta = list(logbeta = list(log(phi))),
      vocab = c("alpha", "beta"),
      convergence = list(its = 1, bound = 0)
    ),
    class = "STM"
  )
  docs <- list(matrix(c(1L, 1L), nrow = 2), matrix(c(1L, 4L, 2L, 5L), nrow = 2))
  fast <- as.LDA(x, docs)
  expect_identical(unname(fast$zw), matrix(c(0L, 5L, 1L, 4L), nrow = 2))
  explicit <- as.LDA(x, docs, count_method = "heuristic")
  expect_identical(fast$zw, explicit$zw)
  model <- as.LDA(x, docs, count_method = "optimized")
  expect_identical(unname(model$zw), expected)
  expect_equal(unname(model$phi), t(phi))
  expect_error(as.LDA(x, docs, count_method = "unknown"), "arg")

  skip_if_not_installed("topicmodels")
  x <- methods::new(
    "LDA_VEM",
    k = 2L,
    alpha = .1,
    gamma = theta,
    beta = log(phi),
    iter = 1L
  )
  docs <- quanteda::dfm(quanteda::as.tokens(list(
    "alpha",
    c(rep("alpha", 4), rep("beta", 5))
  )))
  fast <- as.LDA(x, docs)
  expect_identical(unname(fast$zw), matrix(c(0L, 5L, 1L, 4L), nrow = 2))
  explicit <- as.LDA(x, docs, count_method = "heuristic")
  expect_identical(fast$zw, explicit$zw)
  model <- as.LDA(x, docs, count_method = "optimized")
  expect_identical(unname(model$zw), expected)
  expect_equal(unname(model$phi), t(phi))
  expect_error(as.LDA(x, docs, count_method = "unknown"), "arg")
})

test_that("keyATM conversion minimizes source document probability error", {
  theta <- rbind(c(1.9, .3) / 2.2, c(3.9, 1.3) / 5.2, c(6.9, 3.3) / 10.2)
  colnames(theta) <- c("first", "second")
  x <- structure(
    list(
      keyword_k = 0,
      no_keyword_topics = 2,
      theta = theta,
      values_iter = list(alpha_iter = list(alpha = .1)),
      vocab = c("alpha", "beta"),
      V = 2,
      phi = matrix(.5, nrow = 2, ncol = 2),
      priors = list(beta = .01),
      topic_counts = c(8, 9),
      options = list(iterations = 1)
    ),
    class = "keyATM_output"
  )
  docs <- list(W_raw = list(rep("alpha", 2), rep("beta", 5), rep("beta", 10)))
  set.seed(321)
  fast <- as.LDA(x, docs)
  set.seed(321)
  explicit <- as.LDA(x, docs, count_method = "heuristic")
  expect_identical(fast$zd, explicit$zd)
  model <- as.LDA(x, docs, count_method = "optimized")
  expect_identical(
    unname(model$zd),
    matrix(c(2L, 0L, 3L, 2L, 3L, 7L), nrow = 2)
  )
  expect_false(identical(fast$zd, model$zd))
  expect_equal(unname(model$theta), unname(theta))
  expect_error(as.LDA(x, docs, count_method = "unknown"), "arg")
})

test_that("keyATM conversion preserves topic, document and word counts", {
  x <- structure(
    list(
      keyword_k = 0,
      no_keyword_topics = 3,
      theta = matrix(
        c(.4, .4, .2),
        nrow = 2,
        ncol = 3,
        byrow = TRUE,
        dimnames = list(NULL, c("first", "second", "third"))
      ),
      values_iter = list(alpha_iter = list(alpha = .1)),
      vocab = c("alpha", "beta"),
      V = 2,
      phi = matrix(.5, nrow = 3, ncol = 2),
      priors = list(beta = .01),
      topic_counts = c(2, 0, 0),
      options = list(iterations = 1)
    ),
    class = "keyATM_output"
  )
  model <- as.LDA(x, docs = list(W_raw = list("alpha", "beta")))
  expect_identical(unname(model$zd), matrix(c(1L, 0L, 0L), nrow = 3, ncol = 2))
  expect_identical(unname(model$zw), unname(model$zd))
  expect_equal(unname(model$theta), unname(x$theta))
})
