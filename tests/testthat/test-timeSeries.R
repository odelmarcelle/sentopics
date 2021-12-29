

toks <- ECB_speeches

test_that("prior population works", {
  jst <- JST(toks)

  expect_message(sent <- sentopics_sentiment(jst), "'.sentiment' docvars found.")

  expect_identical(sentopics_sentiment(jst), data.table(.id = names(toks), .sentiment = ECB_speeches$.sentiment))
  expect_identical(sentopics_date(jst), data.table(.id = names(toks), .date = as.Date(ECB_speeches$.date)))
})

jst <- JST(toks, lexicon = LoughranMcDonald) |> grow(10, displayProgress = FALSE)


sentopics_date(jst) <- NULL
sentopics_sentiment(jst) <- NULL

test_that("removal works", {
 expect_null(jst$tokens$.date)
 expect_null(jst$tokens$.sentiment)
})




test_that("sentiment works", {
  expect_message(sent <- sentopics_sentiment(jst, method = "proportionalPol"), "Sentiment computed and assigned")

  expect_true(all(is.finite(sent$.sentiment)))

  expect_message(sent <- sentopics_sentiment(jst, override = TRUE), "Sentiment computed and assigned")

  sentopics_sentiment(jst) <- ECB_speeches$.sentiment

  expect_identical(sentopics_sentiment(jst), data.table(.id = names(toks), .sentiment = ECB_speeches$.sentiment))

})

test_that("date works", {
  expect_error(sentopics_date(jst), "No dates stored")

  sentopics_date(jst) <- ECB_speeches$.date
  expect_message(sentopics_date(jst) <- ECB_speeches$.date)

  expect_identical(sentopics_date(jst), data.table(.id = names(toks), .date = as.Date(ECB_speeches$.date)))
})

sentopics_sentiment(jst) <- ECB_speeches$.sentiment
sentopics_date(jst) <- ECB_speeches$.date

test_that("sentiment_series works", {

  sentiment_series(jst)
  # sentiment_topics(jst)
  # sentiment_breakdown(jst)
  # proportion_topics(jst)
})


test_that("series functions works", {

  lda <- LDA(ECB_speeches)
  lda <- grow(lda, 10, displayProgress = FALSE)

  sentiment_series(lda)
  sentiment_topics(lda)
  sentiment_breakdown(lda)
  proportion_topics(lda)
})
