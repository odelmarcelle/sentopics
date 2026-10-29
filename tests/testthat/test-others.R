test_that("get_ECB_conferences works", {
  skip_on_cran()
  skip_if_offline("www.ecb.europa.eu")
  res <- get_ECB_press_conferences(years = 1998)
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_length(res, 6)
})

test_that("compute_PicaultRenault_scores works", {
  docs <- quanteda::corpus_reshape(ECB_press_conferences, "documents")
  res1 <- compute_PicaultRenault_scores(docs)

  res2 <- compute_PicaultRenault_scores(ECB_press_conferences)
  res2 <- apply(res2, 2, function(x) {
    aggregate(x, by = list(quanteda::docid(ECB_press_conferences)), mean)$x
  })

  expect_equal(res1, res2, check.attributes = FALSE)
})

ecb_fixture_download <- function(pages) {
  function(url, destfile, ...) {
    if (grepl('index_include', url, fixed=TRUE)) {
      content <- paste0('<a href="/press/press_conference/2000/html/',names(pages),'">Entry</a>', collapse='')
    } else {
      content <- pages[[basename(url)]]
    }
    writeLines(content, destfile, useBytes=TRUE)
    invisible(0L)
  }
}

test_that('ECB parser accepts arbitrary paragraph attributes and removes attributed Q&A', {
  skip_if(packageVersion('testthat') < '3.2.0')
  page <- paste0('<main ><h1>Indexed page</h1><h2>ECB President</h2>',
    '<p>Plain paragraph.</p>',
    '<p id="one" class="body" data-note="a > b">Double quoted attributes.</p>',
    "<p data-note='c > d' id='two'>Single quoted attributes.</p>",
    '<P ID="three">Uppercase paragraph.</P>',
    '<pre>Not a paragraph.</pre>',
    '<p id="end">We are now ready to take your questions.</p>',
    '<P id="qa"><strong>Question:</strong>Do not retain this question.</P>',
    '<p>Do not retain this answer.</p></main>')
  local_mocked_bindings(download.file=ecb_fixture_download(c(is000101.en.html=page)),.package='utils')
  result <- get_ECB_press_conferences(years=2000)
  expect_equal(result$content, c('Plain paragraph.','Double quoted attributes.',
    'Single quoted attributes.','Uppercase paragraph.','We are now ready to take your questions.'))
})

test_that('ECB parser separates a host welcome without removing either speaker', {
  skip_if(packageVersion('testthat') < '3.2.0')
  page <- paste0('<main ><h1>Press conference</h1><h2>ECB President</h2>',
    '<h2> Welcome address by the host</h2><p>Host welcome.</p>',
    '<p>* * *</p><p>President statement.</p></main>')
  local_mocked_bindings(download.file=ecb_fixture_download(c(is000101.en.html=page)),.package='utils')
  result <- get_ECB_press_conferences(years=2000)
  expect_equal(result$content,c('Host welcome.','President statement.'))
  expect_equal(trimws(result$section_title),c('Welcome address by the host','ECB President'))
  expect_equal(length(unique(result$doc_id)),1L)
})

test_that('ECB parser cleans website text while keeping every indexed event', {
  skip_if(packageVersion('testthat') < '3.2.0')
  page <- paste0('<main ><h1>Regular event</h1><h2>ECB President</h2>',
    '<p>With the transcript of the questions and answers</p>',
    '<p>Spoken statement. Click here for the transcript of questions and answers.</p>',
    '<p>The first paragraph of the last question was updated on 17 December 2021 at 14:40 to correct a transcription error.</p></main>')
  other <- '<main ><h1>Other institutional event</h1><h2>Speaker</h2><p>Slides: pdf 36 kB.</p></main>'
  local_mocked_bindings(download.file=ecb_fixture_download(c(is000101.en.html=page,is000102.en.html=other)),.package='utils')
  result <- get_ECB_press_conferences(years=2000)
  expect_equal(result$content,c('Spoken statement.','Slides: pdf 36 kB.'))
  expect_equal(length(unique(result$doc_id)),2L)
  listed <- get_ECB_press_conferences(years=2000,data.table=FALSE)
  expect_length(listed,2L)
  expect_equal(unname(vapply(listed,'[[','','title')),c('Regular event','Other institutional event'))
})
