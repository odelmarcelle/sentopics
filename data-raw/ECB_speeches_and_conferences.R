# Press conference --------------------------------------------------------

# The parser preserves the ECB index. The packaged corpus selects monetary
# policy statements and related policy briefings before preprocessing.
# Match exclusions by date and title so the selection does not depend on the
# sequence of item identifiers returned by a future download.

# Tokenization runtime for the 29 October 2026 snapshot:
# Python 3.12.14, spaCy 3.8.7, en_core_web_sm 3.8.0, spacyr 1.3.0,
# quanteda 4.3.1 and quanteda.textstats 0.97.2.
# Python dependencies are pinned in ECB_preprocessing_requirements.txt.
# Set RETICULATE_PYTHON to the corresponding environment before running.

library(quanteda)
ECB_press_conferences <- get_ECB_press_conferences(years = 1998:2026)
exclusions <- read.csv("data-raw/ECB_corpus_exclusions.csv", encoding = "UTF-8")
event_key <- function(x) paste(x$date, x$title, sep = "\r")
stopifnot(all(event_key(exclusions) %in% event_key(ECB_press_conferences)))
ECB_press_conferences <- ECB_press_conferences[
  !event_key(ECB_press_conferences) %in% event_key(exclusions),
]
# ECB_press_conferences2 <- ECB_press_conferences[, list(
#   date = unique(date),
#   title = paste0(unique(title), " -- ", section_title[1]),
#   content = paste0(content, collapse = "\n\n")
# ), by = "doc_id"]

# ECB_press_conferences2 <- corpus(ECB_press_conferences2, docid_field = "doc_id", text_field = "content")
# ECB_press_conferences22 <- corpus_reshape(ECB_press_conferences2, "paragraphs")
# ECB_press_conferences22
ECB_press_conferences <- corpus(
  ECB_press_conferences,
  docid_field = "paragraph_id",
  text_field = "content"
)
#
# head(attr(ECB_press_conferences22, "docvars"))
# head(attr(ECB_press_conferences, "docvars"))
#
# waldo::compare(ECB_press_conferences, ECB_press_conferences22)

attr(ECB_press_conferences, "meta")$object$unit <- "paragraph"
attr(ECB_press_conferences, "docvars")$docid_ <- factor(
  ECB_press_conferences$doc_id,
  levels = unique(ECB_press_conferences$doc_id)
)
data.table::setnames(attr(ECB_press_conferences, "docvars"), "date", ".date")
ECB_press_conferences$.date <- as.Date(ECB_press_conferences$.date)
# ee <- corpus_reshape(ECB_press_conferences, "documents")

usethis::use_data(ECB_press_conferences, overwrite = TRUE)

## Subset paragraphs too short
{
  n <- ntoken(
    ECB_press_conferences,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  )
  # summary(n)
  # aggregate(n, list(ECB_press_conferences$doc_id), sum)$x |> hist()
  ECB_press_conferences_tokens <- ECB_press_conferences[n > 10]
}

{
  spacyr::spacy_initialize(entity = FALSE)
  (parsed <- spacyr::spacy_parse(
    ECB_press_conferences_tokens,
    entity = FALSE,
    nounphrase = FALSE
  )) |>
    system.time() |>
    print()
  toks <- quanteda::as.tokens(
    subset(parsed, pos %in% c("NOUN", "ADJ", "PROPN")),
    use_lemma = TRUE
  )
  ECB_press_conferences_tokens <- ECB_press_conferences_tokens[
    names(ECB_press_conferences_tokens) %in% names(toks)
  ]
  {
    # toks$.date <- as.Date(ECB_press_conferences_tokens$.date)
    # ECB_press_conferences_tokens$date <- NULL
  }
  {
    # toks$.sentiment <- ECB_press_conferences_tokens$.sentiment
    # ECB_press_conferences_tokens$sentiment <- NULL
  }
  docvars(toks) <- docvars(ECB_press_conferences_tokens)
  ECB_press_conferences_tokens <- toks
}

## Remove set of stopwords
ECB_press_conferences_tokens <- tokens_remove(
  ECB_press_conferences_tokens,
  c(
    stopwords("en", source = "marimo"),
    "lady",
    "term",
    "long",
    "medium",
    "short",
    "gentleman",
    "speech",
    "see",
    "ladies",
    "gentlemen",
    "year",
    "years",
    "also",
    "can",
    "may",
    "new",
    "one",
    "time",
    "important",
    "level",
    "however",
    "need",
    "first",
    "even",
    "let",
    "like",
    "two",
    "three",
    "four",
    "five",
    "framework",
    "second",
    "third",
    "now",
    "well",
    "since",
    "recent",
    "process",
    "current",
    "large",
    "many",
    "future",
    "low",
    "still",
    "way",
    "high",
    "across",
    "key",
    "today",
    "last",
    "main",
    "different",
    "much",
    "take",
    "make",
    "higher",
    "lower",
    "made",
    "towards",
    "within",
    "period",
    "view",
    "past",
    "present",
    "already",
    "use",
    "ensure",
    "fact",
    "analysis",
    "number",
    "numbers",
    "objective",
    "part"
  )
)
## Detect bigrams
quanteda.textstats::textstat_collocations(
  ECB_press_conferences_tokens,
  min_count = 500,
  size = 2
)
ECB_press_conferences_tokens <- tokens_compound(
  ECB_press_conferences_tokens,
  sapply(
    quanteda.textstats::textstat_collocations(
      ECB_press_conferences_tokens,
      min_count = 500,
      size = 2
    )$collocation,
    phrase
  )
)

ECB_press_conferences_tokens <- tokens_tolower(ECB_press_conferences_tokens)

length(types(ECB_press_conferences_tokens))

## Frequency-based cleaning
dfm <- dfm(ECB_press_conferences_tokens)
dfm

dfm |>
  dfm_remove(c(stopwords("en", source = "marimo"), "non")) |>
  dfm_trim(min_docfreq = 10) |>
  dfm_trim(max_docfreq = 1, docfreq_type = "prop") |>
  dfm_select(min_nchar = 3) -> dfm2
dfm2

ECB_press_conferences_tokens <- as.tokens(
  dfm2,
  tokens = ECB_press_conferences_tokens,
  padding = TRUE,
  ignore_list = unlist(LoughranMcDonald, use.names = FALSE)
)

usethis::use_data(ECB_press_conferences_tokens, overwrite = TRUE)
