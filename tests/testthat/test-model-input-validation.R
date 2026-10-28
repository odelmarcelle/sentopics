test_that("invalid active priors fail before native model construction", {
  toks <- ECB_press_conferences_tokens[1:5]
  for (value in list(-1, 0, NA_real_, NaN, Inf, numeric())) {
    expect_error(LDA(toks, alpha = value), "alpha.*prior")
    expect_error(LDA(toks, beta = value), "beta.*prior")
    expect_error(JST(toks, gamma = value), "gamma.*prior")
    expect_error(rJST(toks, gamma = value), "gamma.*prior")
  }
  expect_error(LDA(toks, K = 2.5), "L1.*integer")
  expect_error(rJST(toks, S = NA_real_), "L2.*integer")
  expect_error(rJST(toks, alphaCycle = -1), "L1cycle.*integer")
  model <- LDA(toks)
  expect_true(all(model$alpha > 0))
  expect_silent(fit(model, 0, displayProgress = FALSE))
  lex <- rJST(
    quanteda::tokens(c("good bad growth", "good growth")),
    K = 2,
    lexicon = quanteda::dictionary(list(positive = "good", negative = "bad"))
  )
  expect_true(any(lex$beta == 0))
  expect_true(all(lex$beta >= 0))
})

test_that("fit rejects invalid iteration counts for single and multiple chains", {
  model <- LDA(ECB_press_conferences_tokens[1:5])
  chains <- fit(model, 0, nChains = 2, displayProgress = FALSE)
  for (iterations in list(
    -1,
    NA_real_,
    Inf,
    NaN,
    1.5,
    c(1, 2),
    numeric(),
    TRUE
  )) {
    expect_error(
      fit(model, iterations, displayProgress = FALSE),
      "iterations.*integer"
    )
    expect_error(
      fit(chains, iterations, displayProgress = FALSE),
      "iterations.*integer"
    )
  }
  expect_error(fit(model, 1, nChains = 1.5), "nChains.*integer")
  expect_equal(fit(model, 2, displayProgress = FALSE)$it, 2)
})
