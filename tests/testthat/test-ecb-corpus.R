test_that("the packaged ECB corpus contains the documented policy sample", {
  excluded <- c(43L, 44L, 61L, 65L, 67L, 82L, 83L, 84L, 201L, 261L,
    263L, 265L, 267L, 269L, 271L, 273L, 275L, 277L, 279L, 283L, 286L,
    287L, 289L, 290L, 292L, 293L, 295L, 296L, 299L, 301L, 303L, 305L,
    307L, 309L, 311L, 317L, 319L)
  raw <- quanteda::docvars(ECB_press_conferences)
  tokens <- quanteda::docvars(ECB_press_conferences_tokens)
  expect_setequal(unique(raw$doc_id), setdiff(seq_len(327L), excluded))
  expect_setequal(unique(tokens$doc_id), unique(raw$doc_id))
  expect_length(ECB_press_conferences, 4942L)
  expect_true(all(!raw$doc_id %in% excluded))
  expect_true(all(!tokens$doc_id %in% excluded))
  expect_true(all(raw$doc_id[raw$.date == as.Date("2002-01-03")] == 45L))
  expect_true(all(c(114L, 256L) %in% raw$doc_id))
  expect_equal(length(unique(raw$.date)), 290L)
  expect_equal(max(raw$.date), as.Date("2026-10-29"))
  expect_equal(sum(raw$.date == as.Date("2026-10-29")), 21L)
})

test_that("packaged ECB tokens retain the original paragraph identity", {
  raw <- quanteda::docvars(ECB_press_conferences)
  tokens <- quanteda::docvars(ECB_press_conferences_tokens)
  ids <- names(ECB_press_conferences_tokens)
  position <- match(ids, names(ECB_press_conferences))
  expect_false(anyNA(position))
  expected <- raw[position, ]
  rownames(expected) <- rownames(tokens) <- NULL
  expect_equal(tokens, expected)
  expect_true(all(quanteda::ntoken(ECB_press_conferences_tokens,
    remove_padding = TRUE) > 0L))
})
