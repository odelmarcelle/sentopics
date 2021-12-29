
context("testing rJST")
toks <- ECB_speeches[1:10]

rJST <- rJST(toks, lexicon = LoughranMcDonald)

test_that("rJST works", {
  tabl <- table(rJST$vocabulary$lexicon)
  expect_length(tabl, 3)
  expect_gt(sum(tabl), 0)
  rJST <- grow(rJST, 2, displayProgress = FALSE)
  expect_true(check_integrity(rJST, fast = FALSE))
  expect_s3_class(rJST, "rJST")
  expect_equal(attr(rJST, "reversed"), TRUE)

  rJST2 <- grow(rJST, 2, nChains = 2, displayProgress = FALSE)
  rJST2 <- grow(rJST2, 2, displayProgress = FALSE)
  expect_identical(attr(rJST2, "containedClass"), "rJST")

  expect_true(all(sapply(rJST2, function(x) attr(x, "reversed"))))
  expect_true(all(sapply(rJST2, check_integrity, fast = FALSE)))
})

rJST <- grow(rJST, 2, displayProgress = FALSE)

test_that("functions works", {
  expect_s3_class(topWords(rJST), "data.table")
  expect_true(is.matrix(topWords(rJST, output = "matrix")))
  expect_silent(p <- topWords(rJST, output = "plot"))
  expect_s3_class(p, "ggplot")
  expect_silent(print(p))
  expect_silent(m <- melt(rJST))
  expect_s3_class(m, "data.table")
  # expect_equal(ncol(m), 6)
  expect_silent(p <- plot(rJST))
  expect_s3_class(p, "plotly")
  expect_silent(print(p))
})

test_that("test convergence", {
  vocab <- generateVocab(nTopics = 3, nSentiments = 2, nWords = 3, nCommonWords = 1)
  toks <- generateDocuments(vocab, nDocs = 100, L1prior = .1, L2prior = 5, nWords = 100)
  lex <- generatePartialLexicon(toks, Sindex = 1:2)

  ## With lexicon
  retry <- 0
  convergence <- FALSE
  while (convergence == FALSE && retry < 5) {
    rJST <- rJST(toks, lex, K = 3, S = 2, alpha = .1)
    rJST <- grow(rJST, 100, nChains = 2, displayProgress = FALSE)
    count <- 0
    while (convergence == FALSE && count < 30) {
      rJST <- grow(rJST, 100, nChains = 2, displayProgress = FALSE)
      count <- count + 1
      if (all(distToGenerative(rJST, vocab) < .15)) convergence <- TRUE
    }
    retry <- retry + 1
  }
  sapply(distToGenerative(rJST, vocab), expect_lte, .15)

  ## No lexicon
  retry <- 0
  convergence <- FALSE
  while (convergence == FALSE && retry < 5) {
    rJST <- rJST(toks, K = 3, S = 2, alpha = .1)
    rJST <- grow(rJST, 100, nChains = 2, displayProgress = FALSE)
    expect_message(distToGenerative(rJST, vocab), "No lexicon detected, allowing")
    count <- 0
    while (convergence == FALSE && count < 30) {
      rJST <- grow(rJST, 100, nChains = 2, displayProgress = FALSE)
      count <- count + 1
      if (all(suppressMessages(distToGenerative(rJST, vocab)) < .15))
        convergence <- TRUE
    }
    retry <- retry + 1
  }
  sapply(suppressMessages(distToGenerative(rJST, vocab)), expect_lte, .15)
})
