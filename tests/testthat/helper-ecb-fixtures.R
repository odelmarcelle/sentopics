# Keep complete conferences so the fixture has multiple paragraphs per date
# and spans enough months for quarterly, yearly and rolling-window series.
ecb_test_tokens <- function() {
  toks <- ECB_press_conferences_tokens
  conferences <- head(unique(toks$doc_id), 24L)
  recompileTypes(toks[toks$doc_id %in% conferences])
}

# External sentiment tests need varied values, independently of lexicon scoring.
ecb_test_sentiment <- function(toks) {
  stats::setNames(seq(-1, 1, length.out = length(toks)), names(toks))
}
