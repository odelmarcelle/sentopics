
context("Basic tests")
toks <- ECB_press_conferences_tokens[1:20]
sentopicmodel <- sentopicmodel(toks, lex = LoughranMcDonald)


test_that("creating a sentopicmodel works", {

  expect_true(check_integrity(sentopicmodel, fast = FALSE))
  expect_s3_class(sentopicmodel, "sentopicmodel")
})

sentopicmodel <- grow(sentopicmodel, 5, displayProgress = FALSE)

test_that("growing a sentopicmodel works", {
  expect_true(check_integrity(sentopicmodel, fast = FALSE))
  expect_s3_class(sentopicmodel, "sentopicmodel")
  expect_length(sentopicmodel$logLikelihood, 5)
  expect_equal(sentopicmodel$it, 5)
  expect_true(all(c("L1post", "L2post", "phi") %in% names(sentopicmodel)))
})

test_that("print and plot methods work", {
  expect_output(print(sentopicmodel), "A sentopicmodel topic model with 5 topics and 3 sentiments. Currently grown by 5 Gibbs sampling iterations.")
})

test_that("output functions works", {
  expect_silent(tops <- topWords(sentopicmodel, output = "matrix"))
  expect_false(anyNA(tops))
  txts <- getTexts(sentopicmodel, topic = "l1-1", "negative")
  expect_length(txts, 3)
  expect_type(unlist(txts), "character")
})

test_that("Updates works", {
  alphaU <- sentopicmodel(toks, L1cycle = 2)
  alphaU <- grow(alphaU, 5, displayProgress = FALSE)
  expect_false(isTRUE(all.equal(rep(1, 5), alphaU$alpha)))

  alphaU <- LDA(toks)
  alphaU$alphaCycle <- 2
  alphaU <- grow(alphaU, 5, displayProgress = FALSE)
  expect_false(isTRUE(all.equal(rep(1, 5), alphaU$alpha)))
})

# JST <- JST(toks, lexicon = quanteda::dictionary(quanteda.dictionaries::data_dictionary_LoughranMcDonald))

### TODO: add test for utils functions such as getTheta()

