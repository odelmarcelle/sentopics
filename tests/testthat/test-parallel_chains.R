context("Tests for multiple and parallel chains")
library("future")
plan(sequential)

vocab <- generateVocab(
  nTopics = 5,
  nSentiments = 3,
  nWords = 5,
  nCommonWords = 2
)
toks <- generateDocuments(
  vocab,
  nDocs = 10,
  L1prior = 5,
  L2prior = 5,
  nWords = 10,
  nClass = 2
)
generated_sentopicsmodel <- sentopicsmodel(toks)
generated_sentopicsmodel <- fit(
  generated_sentopicsmodel,
  20,
  displayProgress = FALSE,
  nChains = 2
)


test_that("multiple chains works", {
  expect_true(all(sapply(generated_sentopicsmodel, check_integrity)))
  expect_s3_class(generated_sentopicsmodel, "multi_chains")
  expect_message(
    generated_sentopicsmodel <- fit(
      generated_sentopicsmodel,
      20,
      displayProgress = FALSE
    ),
    NA
  )
  expect_true(all(sapply(generated_sentopicsmodel, check_integrity)))
})

test_that("accessors works", {
  expect_s3_class(generated_sentopicsmodel$tokens, "tokens")
  expect_s3_class(generated_sentopicsmodel$vocabulary, "data.table")
  expect_true(check_integrity(generated_sentopicsmodel$chain1))
  expect_true(check_integrity(generated_sentopicsmodel[[1]]))

  expect_s3_class(tmp <- generated_sentopicsmodel[1], "multi_chains")
  expect_true(check_integrity(tmp[[1]]))
  expect_s3_class(head(generated_sentopicsmodel), "multi_chains")
  expect_s3_class(tail(generated_sentopicsmodel), "multi_chains")
})

if (Sys.getenv("R_COVR") != "true") {
  plan(multisession, workers = 2)
  generated_sentopicsmodel <- sentopicsmodel(toks)
  generated_sentopicsmodel <- fit(
    generated_sentopicsmodel,
    20,
    displayProgress = TRUE,
    nChains = 10
  )

  test_that("parallel works", {
    expect_true(all(sapply(generated_sentopicsmodel, check_integrity)))
    expect_s3_class(generated_sentopicsmodel, "multi_chains")
    expect_message(
      generated_sentopicsmodel <- fit(
        generated_sentopicsmodel,
        20,
        displayProgress = FALSE
      ),
      NA
    )
    expect_true(all(sapply(generated_sentopicsmodel, check_integrity)))
  })

  test_that("seed works", {
    plan(sequential)
    generated_sentopicsmodel <- sentopicsmodel(toks)
    set.seed(123)
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 1
    )
    set.seed(123)
    sentopicsmodel_2 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 1
    )
    expect_identical(sentopicsmodel_1, sentopicsmodel_2)
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 1,
      seed = 123
    )
    sentopicsmodel_2 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 1,
      seed = 123
    )
    expect_identical(sentopicsmodel_1, sentopicsmodel_2)
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 1,
      seed = 1234
    )
    expect_false(identical(sentopicsmodel_1, sentopicsmodel_2))
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 1,
      seed = NULL
    )
    sentopicsmodel_2 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 1,
      seed = NULL
    )
    expect_false(identical(sentopicsmodel_1, sentopicsmodel_2))

    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = 123
    )
    sentopicsmodel_2 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = 123
    )
    expect_identical(sentopicsmodel_1, sentopicsmodel_2)
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = 1234
    )
    expect_false(identical(sentopicsmodel_1, sentopicsmodel_2))
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = NULL
    )
    sentopicsmodel_2 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = NULL
    )
    expect_false(identical(sentopicsmodel_1, sentopicsmodel_2))

    plan(multisession, workers = 2)
    set.seed(123)
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2
    )
    set.seed(123)
    sentopicsmodel_2 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2
    )
    expect_identical(sentopicsmodel_1, sentopicsmodel_2)
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = NULL
    )
    sentopicsmodel_2 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = NULL
    )
    expect_false(identical(sentopicsmodel_1, sentopicsmodel_2))
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = 1234
    )
    sentopicsmodel_2 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = 123
    )
    expect_false(identical(sentopicsmodel_1, sentopicsmodel_2))
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = 123
    )
    expect_identical(sentopicsmodel_1, sentopicsmodel_2)

    sentopicsmodel_11 <- fit(
      sentopicsmodel_1,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = 1234
    )
    sentopicsmodel_22 <- fit(
      sentopicsmodel_2,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = 1234
    )
    expect_identical(sentopicsmodel_11, sentopicsmodel_22)
    sentopicsmodel_22 <- fit(
      sentopicsmodel_2,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = 123
    )
    expect_false(identical(sentopicsmodel_11, sentopicsmodel_22))
    sentopicsmodel_11 <- fit(
      sentopicsmodel_1,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = NULL
    )
    sentopicsmodel_22 <- fit(
      sentopicsmodel_2,
      2,
      displayProgress = FALSE,
      nChains = 2,
      seed = NULL
    )
    expect_false(identical(sentopicsmodel_11, sentopicsmodel_22))

    # cran is annoyed by more than 2 cores => skip it
    skip_on_cran()
    # for CMD check
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      num_workers <- 2L
    } else {
      # use all cores in devtools::test()
      num_workers <- min(parallel::detectCores(), 5L)
    }

    plan(multisession, workers = num_workers)
    ### this works !
    sentopicsmodel_1 <- fit(
      generated_sentopicsmodel,
      5,
      displayProgress = FALSE,
      nChains = 3,
      seed = 1234
    )
    plan(multisession, workers = 2)
    sentopicsmodel_2 <- fit(
      generated_sentopicsmodel,
      5,
      displayProgress = FALSE,
      nChains = 3,
      seed = 1234
    )
    plan(multisession, workers = 1)
    sentopicsmodel_3 <- fit(
      generated_sentopicsmodel,
      5,
      displayProgress = FALSE,
      nChains = 3,
      seed = 1234
    )
    expect_identical(sentopicsmodel_1, sentopicsmodel_2)
    expect_identical(sentopicsmodel_1, sentopicsmodel_3)

    plan(multisession, workers = 2)
    sentopicsmodel_3 <- fit(
      generated_sentopicsmodel,
      5,
      displayProgress = FALSE,
      nChains = 3,
      seed = 123
    )
    expect_false(identical(sentopicsmodel_1, sentopicsmodel_3))
  })
}

toks <- generateDocuments(
  vocab,
  nDocs = 10,
  L1prior = 5,
  L2prior = 5,
  nWords = 110,
  nClass = 2
)
lex <- generatePartialLexicon(toks)
generated_sentopicsmodel <- sentopicsmodel(toks, lex)
generated_sentopicsmodel <- fit(
  generated_sentopicsmodel,
  20,
  displayProgress = FALSE,
  nChains = 3
)

test_that("scores & distances works", {
  plan(multisession, workers = if (Sys.getenv("R_COVR") != "true") 2 else 1)
  expect_silent(
    scores <- chains_scores(
      generated_sentopicsmodel,
      nWords = 2,
      window = "boolean"
    )
  )
  expect_false(anyNA(scores))
  expect_silent(distances <- chains_distances(generated_sentopicsmodel))
  expect_true(is.matrix(distances))
  expect_false(anyNA(distances))
  expect_silent(
    distances <- chains_distances(generated_sentopicsmodel, method = "cosine")
  )
  expect_false(anyNA(distances))
  expect_silent(
    distances <- chains_distances(
      generated_sentopicsmodel,
      method = "hellinger"
    )
  )
  expect_false(anyNA(distances))
  expect_silent(
    distances <- chains_distances(generated_sentopicsmodel, method = "minMax")
  )
  expect_false(anyNA(distances))
  expect_silent(
    distances2 <- chains_distances(
      generated_sentopicsmodel,
      method = "invariantEuclidean"
    )
  )
  expect_false(anyNA(distances))
  expect_true(all(distances <= distances2))
  expect_silent(
    distances <- chains_distances(
      generated_sentopicsmodel,
      method = "naiveEuclidean"
    )
  )
  expect_false(anyNA(distances))
})


test_that("equality between euclidean distances", {
  toks <- ECB_press_conferences_tokens[1:10]
  LDA <- LDA(toks)
  LDAs <- fit(LDA, 10, nChains = 5)
  expect_equal(
    chains_distances(LDAs),
    chains_distances(LDAs, method = "invariantEuclidean")
  )
  rJST <- rJST(toks, S = 1)
  rJSTs <- fit(rJST, 10, nChains = 5)
  expect_equal(
    chains_distances(rJSTs),
    chains_distances(rJSTs, method = "invariantEuclidean")
  )
})


test_that("plot methods work", {
  expect_silent(plot(generated_sentopicsmodel))
})

## fix detritus
plan(sequential)
