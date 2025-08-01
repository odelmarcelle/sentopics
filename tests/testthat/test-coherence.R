context("Tests for coherence metrics")
toks <- ECB_press_conferences_tokens[
  quanteda::ntoken(ECB_press_conferences_tokens) >= 110
]
dfm <- quanteda::dfm(toks, tolower = FALSE)
dfm <- quanteda::dfm_trim(dfm, min_termfreq = 2)
dfm <- quanteda::dfm_remove(dfm, quanteda::stopwords("en"))
toks <- quanteda::tokens_keep(toks, colnames(dfm), padding = TRUE)
sentopicsmodel <- sentopicsmodel(toks, lexicon = LoughranMcDonald)
sentopicsmodel <- fit(sentopicsmodel, 100, displayProgress = FALSE)


test_that("basic metrics works", {
  expect_silent(coherence(sentopicsmodel))
  expect_error(coherence(sentopicsmodel, 1))
  expect_error(coherence(sentopicsmodel, "C_V"))
  expect_silent(res <- coherence(sentopicsmodel, method = "C_V"))
  expect_false(anyNA(res))
  expect_silent(res <- coherence(sentopicsmodel, method = "C_NPMI"))
  expect_false(anyNA(res))
  # expect_silent(res <- coherence(sentopicsmodel, method = "topics"))
  # expect_false(anyNA(res))
  # expect_silent(res <- coherence(sentopicsmodel, method = "topicsScaled"))
  # expect_false(anyNA(res))
  # expect_silent(res <- coherence(sentopicsmodel, method = "CLexicon"))
  # expect_false(anyNA(res[c(1,3), ]))
  # expect_silent(res <- coherence(sentopicsmodel, method = "CLexiconScaled"))
  # expect_false(anyNA(res[c(1,3), ]))
  # expect_silent(res <- coherence(sentopicsmodel, method = "CLexiconScaledNPMI"))
  # expect_false(anyNA(res[c(1,3), ]))
  # expect_silent(res <- coherence(sentopicsmodel, method = "hierarchyTopics"))
  # expect_false(anyNA(res))
})

LDA <- LDA(toks)
LDA <- fit(LDA, 100, displayProgress = FALSE)

JST <- JST(toks)
JST <- fit(JST, 100, displayProgress = FALSE)

rJST <- rJST(toks)
rJST <- fit(rJST, 100, displayProgress = FALSE)

test_that("other models works", {
  expect_silent(res <- coherence(LDA))
  expect_false(anyNA(res))
  expect_silent(res <- coherence(JST))
  expect_false(anyNA(res))
  expect_silent(res <- coherence(rJST))
  expect_false(anyNA(res))

  # expect_identical(coherence(rJST), coherence.sentopicsmodel(rJST))
  expect_length(coherence(LDA), 5)
})
