context("testing JST")
toks <- ECB_press_conferences_tokens[1:10]

JST <- JST(toks, lexicon = LoughranMcDonald)

test_that("JST works", {
  expect_output(
    print(JST),
    "A JST model with 3 sentiments and 5 topics. Currently fitted by 0 Gibbs sampling iterations."
  )
  tabl <- table(JST$vocabulary$lexicon)
  expect_length(tabl, 3)
  expect_gt(sum(tabl), 0)
  JST <- fit(JST, 2, displayProgress = FALSE)
  expect_true(check_integrity(JST, fast = FALSE))
  expect_s3_class(JST, "JST")
  expect_equal(attr(JST, "reverse"), FALSE)

  JST2 <- fit(JST, 2, nChains = 2, displayProgress = FALSE)
  JST2 <- fit(JST2, 2, displayProgress = FALSE)
  expect_identical(attr(JST2, "containedClass"), "JST")

  expect_true(all(!sapply(JST2, function(x) attr(x, "reverse"))))
  expect_true(all(sapply(JST2, check_integrity, fast = FALSE)))
})

JST <- fit(JST, 2, displayProgress = FALSE)

test_that("functions works", {
  expect_s3_class(top_words(JST), "data.table")
  expect_true(is.matrix(top_words(JST, output = "matrix")))
  expect_silent(p <- top_words(JST, output = "plot"))
  expect_s3_class(p, "ggplot")
  expect_silent(print(p))
  expect_silent(m <- melt(JST))
  expect_s3_class(m, "data.table")
  # expect_equal(ncol(m), 6)
  skip_if_not_installed("plotly")
  expect_silent(p <- plot(JST))
  expect_s3_class(p, "plotly")
  expect_silent(print(p))
})

test_that("test convergence", {
  vocab <- generateVocab(
    nTopics = 3,
    nSentiments = 2,
    nWords = 3,
    nCommonWords = 1,
    hierarchy = "JST"
  )
  toks <- generateDocuments(
    vocab,
    nDocs = 100,
    L1prior = .1,
    L2prior = 5,
    nWords = 100,
    nClass = 1
  )
  lex <- generatePartialLexicon(toks, Sindex = 1:2)

  ## With lexicon
  retry <- 0
  convergence <- FALSE
  while (convergence == FALSE && retry < 5) {
    JST <- JST(toks, lex, K = 3, S = 2, gamma = .1, alpha = 5)
    JST <- fit(JST, 100, nChains = 2, displayProgress = FALSE)
    count <- 0
    while (convergence == FALSE && count < 30) {
      JST <- fit(JST, 100, nChains = 2, displayProgress = FALSE)
      count <- count + 1
      if (all(distToGenerative(JST, vocab) < .15)) convergence <- TRUE
    }
    retry <- retry + 1
  }
  sapply(distToGenerative(JST, vocab), expect_lte, .15)

  ## No lexicon
  retry <- 0
  convergence <- FALSE
  while (convergence == FALSE && retry < 10) {
    JST <- JST(toks, K = 3, S = 2, gamma = .1, alpha = 5)
    JST <- fit(JST, 100, nChains = 2, displayProgress = FALSE)
    expect_message(
      distToGenerative(JST, vocab),
      "No lexicon detected, allowing"
    )
    count <- 0
    while (convergence == FALSE && count < 30) {
      JST <- fit(JST, 100, nChains = 2, displayProgress = FALSE)
      count <- count + 1
      if (all(suppressMessages(distToGenerative(JST, vocab)) < .15)) {
        convergence <- TRUE
      }
    }
    retry <- retry + 1
  }
  sapply(suppressMessages(distToGenerative(JST, vocab)), expect_lte, .15)
})

#
#
# top_words(JST$chain1, output = "plot")
# top_words(JST$chain2, output = "plot")
#
# e <- JST$chain2
# e <- fit(e, 10000)
# top_words(e, output = "plot")
# plot(e$logLikelihood)
# rebuild_zw(as.sentopicsmodel(JST$chain2))
# rebuild_zw(as.sentopicsmodel(e))
