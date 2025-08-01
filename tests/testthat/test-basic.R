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

test_that("Updates works", {
  alphaU <- sentopicsmodel(toks, L1cycle = 2)
  alphaU <- fit(alphaU, 5, displayProgress = FALSE)
  expect_false(isTRUE(all.equal(rep(1, 5), alphaU$alpha)))

  alphaU <- LDA(toks)
  alphaU$alphaCycle <- 2
  alphaU <- fit(alphaU, 5, displayProgress = FALSE)
  expect_false(isTRUE(all.equal(rep(1, 5), alphaU$alpha)))
})

### TODO: add test for utils functions such as getTheta()
