context("Test grow (ensure that grow() still work)")

toks <- ECB_press_conferences_tokens[1:10]

test_that("LDA works", {
  expect_silent(LDA <- grow(LDA(toks), 10, displayProgress = FALSE))
  expect_output(
    print(LDA),
    "An LDA model with 5 topics. Currently fitted by 10 Gibbs sampling iterations."
  )
})


test_that("rJST works", {
  expect_silent(
    rJST <- grow(
      rJST(toks, lexicon = LoughranMcDonald),
      10,
      displayProgress = FALSE
    )
  )
  expect_output(
    print(rJST),
    "A reversed-JST model with 5 topics and 3 sentiments. Currently fitted by 10 Gibbs sampling iterations."
  )
})


test_that("JST works", {
  expect_silent(
    JST <- grow(
      JST(toks, lexicon = LoughranMcDonald),
      10,
      displayProgress = FALSE
    )
  )
  expect_output(
    print(JST),
    "A JST model with 3 sentiments and 5 topics. Currently fitted by 10 Gibbs sampling iterations."
  )
})


test_that("sentopicmodel works", {
  expect_silent(
    sentopicmodel <- grow(
      sentopicmodel(toks, lexicon = LoughranMcDonald),
      10,
      displayProgress = FALSE
    )
  )
  expect_output(
    print(sentopicmodel),
    "A sentopicmodel topic model with 5 topics and 3 sentiments. Currently fitted by 10 Gibbs sampling iterations."
  )
})
