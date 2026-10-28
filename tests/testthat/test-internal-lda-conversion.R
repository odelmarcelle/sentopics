test_that("as.LDA rejects sentiment layers instead of relabelling them", {
  toks <- ECB_press_conferences_tokens[1:5]
  for (K in c(2, 3)) {
    model <- JST(toks, K = K, S = 3)
    expect_error(as.LDA(model), "does not collapse sentiment layers")
    model <- fit(model, 1, displayProgress = FALSE)
    expect_error(as.LDA(model), "does not collapse sentiment layers")
  }
  expect_error(as.LDA(rJST(toks, K = 2)), "does not collapse")
  chains <- fit(JST(toks, K = 2), 1, nChains = 2, displayProgress = FALSE)
  expect_error(as.LDA(chains), "does not collapse")
  expect_s3_class(chains[[1]], "JST")
})

test_that("as.LDA retains valid LDA and internal single-layer conversions", {
  model <- fit(
    LDA(ECB_press_conferences_tokens[1:5], K = 2),
    1,
    displayProgress = FALSE
  )
  expect_identical(as.LDA(model), model)
  restored <- as.LDA(as.sentopicsmodel(model))
  expect_equal(restored$theta, model$theta)
  expect_equal(restored$phi, model$phi)
  expect_equal(restored$za, model$za)
  chains <- fit(model, 0, nChains = 2, displayProgress = FALSE)
  expect_equal(as.LDA(chains)[[1]]$theta, chains[[1]]$theta)
})
