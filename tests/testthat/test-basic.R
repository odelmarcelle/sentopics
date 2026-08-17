context("Basic tests")
toks <- ECB_press_conferences_tokens[1:20]
sentopicsmodel <- sentopicsmodel(toks, lex = LoughranMcDonald)


test_that("creating a sentopicsmodel works", {
  expect_true(check_integrity(sentopicsmodel, fast = FALSE))
  expect_s3_class(sentopicsmodel, "sentopicsmodel")
})

sentopicsmodel <- fit(sentopicsmodel, 5, displayProgress = FALSE)

test_that("fitting a sentopicsmodel works", {
  expect_true(check_integrity(sentopicsmodel, fast = FALSE))
  expect_s3_class(sentopicsmodel, "sentopicsmodel")
  expect_length(sentopicsmodel$logLikelihood, 5)
  expect_equal(sentopicsmodel$it, 5)
  expect_true(all(c("L1post", "L2post", "phi") %in% names(sentopicsmodel)))
})

test_that("reset works", {
  res <- reset(sentopicsmodel)
  expect_equal(res$it, 0)
  expect_null(res$L1post)
  expect_null(res$L2post)
  expect_false(isTRUE(all.equal(sentopicsmodel$za, res$za)))
  expect_length(res$logLikelihood, 1)
  refitted <- fit(res, 5, displayProgress = FALSE)
})

test_that("print and plot methods work", {
  expect_output(
    print(sentopicsmodel),
    "A sentopicsmodel topic model with 5 topics and 3 sentiments. Currently fitted by 5 Gibbs sampling iterations."
  )
})

test_that("output functions works", {
  expect_silent(tops <- top_words(sentopicsmodel, output = "matrix"))
  expect_type(tops, "character")
  expect_false(anyNA(tops))
  txts <- getTexts(sentopicsmodel, topic = "l1-1", "negative")
  expect_length(txts, 3)
  expect_type(unlist(txts), "character")
})

test_that("top_words subset works", {
  expect_silent(
    tops <- top_words(sentopicsmodel, output = "matrix", subset = topic %in% 1)
  )
  expect_equal(dim(tops), c(10L, 3L))
  expect_silent(
    tops <- top_words(
      sentopicsmodel,
      output = "matrix",
      subset = topic %in% 1 & sentiment == 3L
    )
  )
  expect_equal(dim(tops), c(10L, 1L))
  expect_silent(
    tops <- top_words(
      sentopicsmodel,
      output = "matrix",
      subset = topic %in% 1 & sentiment == 9L
    )
  )
  expect_equal(dim(tops), c(10L, 0L))
})


test_that("top_words methods works", {
  expect_silent(tops <- top_words(sentopicsmodel, method = "probability"))
  expect_true(all(is.finite(tops$value)))
  expect_output(print(tops), "probability")

  expect_silent(tops <- top_words(sentopicsmodel, method = "term-score"))
  expect_true(all(is.finite(tops$value)))
  expect_output(print(tops), "term-score")

  expect_silent(tops <- top_words(sentopicsmodel, method = "FREX"))
  expect_true(all(is.finite(tops$value)))
  expect_output(print(tops), "FREX")
})

test_that("hyperparameter update cycles update the correct priors", {
  set.seed(1)
  initial <- sentopicsmodel(
    toks,
    L1 = 5,
    L2 = 3,
    L1prior = 1,
    L2prior = 5,
    L1cycle = 1,
    L2cycle = 1
  )
  fitted <- fit(initial, 5, displayProgress = FALSE)

  expect_equal(dim(fitted$L1prior), c(5L, 1L))
  expect_equal(dim(fitted$L2prior), c(15L, 1L))
  expect_true(all(is.finite(fitted$L1prior)))
  expect_true(all(fitted$L1prior > 0))
  expect_true(all(is.finite(fitted$L2prior)))
  expect_true(all(fitted$L2prior > 0))
  expect_gt(max(abs(fitted$L1prior - initial$L1prior)), 0)
  expect_gt(max(abs(fitted$L2prior - initial$L2prior)), 0)

  ## Exercise the LDA-specific iteration path as well.
  initial_lda <- sentopicsmodel(
    toks,
    L1 = 5,
    L2 = 1,
    L1prior = 1,
    L2prior = 0,
    L1cycle = 1
  )
  fitted_lda <- fit(initial_lda, 5, displayProgress = FALSE)
  expect_equal(dim(fitted_lda$L1prior), c(5L, 1L))
  expect_gt(max(abs(fitted_lda$L1prior - initial_lda$L1prior)), 0)
})

### TODO: add test for utils functions such as getTheta()
