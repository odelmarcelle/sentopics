

vocab <- generateVocab(nTopics = 2, nSentiments = 3, nWords = 5, nCommonWords = 2, betaDirichlet = 10000000)
toks <- generateDocuments(vocab, nDocs = 10, L1prior = .1, L2prior = .1, nWords = 50, nClass = 1)
lex <- generatePartialLexicon(toks)


test_that("asymmetric prior works", {
  expect_silent(JST <- JST(toks, lex, K = 5, alpha = 1:5, S = 2, gamma = 1:2))
  expect_error(JST <- JST(toks, lex, K = 4, alpha = 1:5, S = 2, gamma = 1:2))
  expect_error(JST <- JST(toks, lex, K = 5, alpha = 1:5, S = 3, gamma = 1:2))

  expect_silent(rJST <- rJST(toks, lex, K = 5, alpha = 1:5, S = 2, gamma = 1:2))
  expect_error(rJST <- rJST(toks, lex, K = 4, alpha = 1:5, S = 2, gamma = 1:2))
  expect_error(rJST <- rJST(toks, lex, K = 5, alpha = 1:5, S = 3, gamma = 1:2))
})
