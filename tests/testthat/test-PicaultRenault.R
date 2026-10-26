test_that("stop words do not prevent Picault-Renault n-gram matches", {
  texts <- c(
    "risks price stability",
    paste("risks", c("a", "the", "an", "of", "to"), "price stability")
  )
  scores <- compute_PicaultRenault_scores(quanteda::corpus(texts))
  entry <- PicaultRenault[PicaultRenault$keyword == "risk price stabil", ]
  expected <- c(
    MP = (entry$mp_rest - entry$mp_acco) /
      (entry$mp_rest + entry$mp_neut + entry$mp_acco),
    EC = (entry$ec_posi - entry$ec_nega) /
      (entry$ec_posi + entry$ec_neut + entry$ec_nega)
  )
  expect_equal(
    unname(scores),
    matrix(expected, nrow = length(texts), ncol = 2, byrow = TRUE)
  )
})

test_that("Picault-Renault stop-word removal can be overridden", {
  text <- quanteda::corpus("risks to price stability")
  filtered <- compute_PicaultRenault_scores(text)
  unfiltered <- compute_PicaultRenault_scores(text, stopwords = character())
  expect_false(isTRUE(all.equal(filtered, unfiltered)))
  expect_equal(unname(unfiltered[, "MP"]), 0.621621621621622)
  expect_equal(compute_PicaultRenault_scores(text, stopwords = "TO"), filtered)

  custom <- quanteda::corpus("risks intervening price stability")
  expect_equal(
    compute_PicaultRenault_scores(custom, stopwords = "intervening"),
    filtered
  )
  dfms <- compute_PicaultRenault_scores(text, return_dfm = TRUE)
  expect_equal(
    as.numeric(rowSums(as.matrix(dfms$MP))),
    unname(filtered[, "MP"])
  )
  expect_equal(
    as.numeric(rowSums(as.matrix(dfms$EC))),
    unname(filtered[, "EC"])
  )
})

test_that("default stop words preserve sentiment-bearing negations", {
  text <- quanteda::corpus("not constrained")
  expect_equal(
    unname(compute_PicaultRenault_scores(text)),
    matrix(c(-1, 0), nrow = 1)
  )
})
