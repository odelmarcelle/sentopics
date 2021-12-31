
context("Test utils")

test_that("virtualDocuments works", {
  toks <- ECB_speeches[1:10]
  expect_warning(virtualDocuments(vD <- toks, window = 1000), "Some documents")
  expect_equal(names(toks), unique(gsub("\\..*", "", names(vD))))
  expect_silent(vD <- virtualDocuments(toks, window = 100))
  expect_equal(names(toks), unique(gsub("\\..*", "", names(vD))))
  expect_true(all(lengths(vD) == 100))
  expect_silent(vD <- virtualDocuments(toks, window = "boolean"))
  expect_identical(c(unclass(toks)), c(unclass(toks)))
})

test_that("as.tokens.dfm works", {
  toks <- ECB_speeches[1:10]
  dfm <- quanteda::dfm(toks, tolower = FALSE)
  expect_silent(LDA <- LDA(dfm))
  expect_silent(JST <- JST(dfm))
  expect_silent(rJST <- rJST(dfm))
  expect_silent(sentopicmodel <- sentopicmodel(dfm))
  expect_identical(as.tokens(dfm, tokens = toks), toks)
})


test_that("melt works", {
  toks <- ECB_speeches[1:10]
  model <- sentopicmodel(toks)
  expect_error(melt.sentopicmodel(model), "Nothing to melt")
  model <- grow(model, 10, displayProgress = FALSE)
  melt.sentopicmodel(model)
})

test_that("sunburst works", {
  toks <- ECB_speeches[1:10]
  model <- sentopicmodel(toks)
  model <- grow(model, 10, displayProgress = FALSE)
  expect_silent(p <- plot(model))
  expect_s3_class(p, "plotly")
  expect_silent(print(p))
})
2


test_that("R likelihood works", {
  toks <- ECB_speeches[1:10]
  model <- sentopicmodel(toks)
  model <- grow(model, 10, displayProgress = FALSE)
  logLik <- c(tail(model$logLikelihood, 1L), sapply(attr(model$logLikelihood, "components"), tail, 1L))
  RlogLik <- multLikelihood(model)
  expect_equivalent(logLik, RlogLik[, 1])

  JST <- grow(JST(toks), displayProgress = FALSE)
  logLik <- c(tail(JST$logLikelihood, 1L), sapply(attr(JST$logLikelihood, "components"), tail, 1L))
  RlogLik <- multLikelihood(JST)
  expect_equivalent(logLik, RlogLik[, 1])
})


test_that("recompile & reset works", {
  toks <- ECB_speeches[1:2]
  model <- JST(toks, lex = LoughranMcDonald)
  model$vocabulary[!is.na(lexicon)]
  model$vocabulary[word == "crisis"]
  model <- grow(model, 10, displayProgress = FALSE)
  expect_true(all(model$phi["crisis", ,-1] == 0))
  expect_true(all(model$phi["crisis", ,1] > 0))

  model$vocabulary[word == "crisis", lexicon := NA ]
  idx <- model$vocabulary[word == "crisis", index]
  expect_warning(model2 <- recompileVocabulary(model),
                 "The model will be reset")
  expect_silent(model2 <- recompileVocabulary(model2))
  expect_true(all(model2$beta[, idx] > 0))
  model2 <- grow(model2, 10, displayProgress = FALSE)
  expect_equal(model2$it, 10)
  expect_true(all(model2$phi["crisis", ,] > 0))
})


