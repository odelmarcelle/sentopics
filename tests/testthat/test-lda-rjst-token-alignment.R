test_that("LDA to rJST requires unchanged tokens and vocabulary", {
  toks <- quanteda::tokens(c(
    d1 = "the outlook is not good at all for growth",
    d2 = "growth is strong and good for the outlook"
  ))
  lda <- fit(LDA(toks, K = 2), 2, displayProgress = FALSE)
  original <- data.table::copy(lda)
  expect_error(
    rJST(lda, lexicon = LoughranMcDonald),
    "lexicon changes.*Initialize rJST"
  )
  expect_identical(lda, original)
  fresh <- rJST(toks, K = 2, lexicon = LoughranMcDonald)
  expect_true("#NEGATOR#__good" %in% fresh$vocabulary$word)
  expect_silent(fit(fresh, 1, displayProgress = FALSE))

  lex <- quanteda::dictionary(list(positive = "good", negative = "bad"))
  converted <- rJST(lda, lexicon = lex)
  expect_equal(converted$theta, lda$theta)
  expect_equal(lapply(converted$za, function(z) (z - 1L) %/% 3L + 1L), lda$za)
  expect_identical(converted$vocabulary$word, lda$vocabulary$word)
  expect_silent(fit(converted, 1, displayProgress = FALSE))
})

test_that("vocabulary merging is rejected even without changing document lengths", {
  lda <- fit(
    LDA(quanteda::tokens(c("Good good outlook", "bad Good growth")), K = 2),
    1,
    displayProgress = FALSE
  )
  expect_error(rJST(lda, lexicon = LoughranMcDonald), "vocabulary")
})
