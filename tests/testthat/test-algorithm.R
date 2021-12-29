

vocab <- generateVocab(nTopics = 2, nSentiments = 3, nWords = 5, nCommonWords = 2, betaDirichlet = 10000000)
toks <- generateDocuments(vocab, nDocs = 10, L1prior = .1, L2prior = .1, nWords = 50, nClass = 1)
lex <- generatePartialLexicon(toks)

sentopicmodel1 <- sentopicmodel(toks, lex, initLDA = 2, smooth = 0, L1cycle = 1, L2cycle = 1)
sentopicmodel2 <- sentopicmodel(toks, lex, initLDA = 0, smooth = 2, L1cycle = 1, L2cycle = 1)
sentopicmodel3 <- sentopicmodel(toks, lex, initLDA = 2, smooth = 2, L1cycle = 1, L2cycle = 1)

test_that("algo variations works", {
  expect_silent(a <- grow(sentopicmodel1, 3, displayProgress = FALSE))
  expect_false(any(c(a$L1prior == 5, a$L2prior == 5)))
  expect_silent(a <- grow(sentopicmodel2, 3, displayProgress = FALSE))
  expect_false(any(c(a$L1prior == 5, a$L2prior == 5)))
  expect_silent(a <- grow(sentopicmodel3, 3, displayProgress = FALSE))
  expect_false(any(c(a$L1prior == 5, a$L2prior == 5)))
})

test_that("asymmetric prior works", {
  expect_silent(JST <- JST(toks, lex, K = 5, alpha = 1:5, S = 2, gamma = 1:2))
  expect_error(JST <- JST(toks, lex, K = 4, alpha = 1:5, S = 2, gamma = 1:2))
  expect_error(JST <- JST(toks, lex, K = 5, alpha = 1:5, S = 3, gamma = 1:2))

  expect_silent(rJST <- rJST(toks, lex, K = 5, alpha = 1:5, S = 2, gamma = 1:2))
  expect_error(rJST <- rJST(toks, lex, K = 4, alpha = 1:5, S = 2, gamma = 1:2))
  expect_error(rJST <- rJST(toks, lex, K = 5, alpha = 1:5, S = 3, gamma = 1:2))
})
