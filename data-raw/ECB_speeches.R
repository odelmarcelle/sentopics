library(quanteda)

ECB_speeches <- get_ECB_speeches(
  filter_english = TRUE,
  clean_footnotes = TRUE,
  compute_sentiment = TRUE,
  tokenize_w_POS = TRUE
)

## Remove set of stopwords
ECB_speeches <- tokens_remove(ECB_speeches, c(stopwords("en"), "speech", "see", "ladies", "gentlemen", "year", "years", "also", "can", "may", "new", "one", "time", "important", "level", "however", "need", "first", "even", "let", "like", "two", "three", "four", "five", "framework", "second", "third", "now", "well", "since", "recent", "process", "current", "large", "many", "future", "low", "still", "way", "high", "across", "key", "today", "last", "main", "different", "much", "take", "make", "higher", "lower", "made", "towards", "within", "period", "view", "past", "present", "already", "use", "ensure", "fact", "analysis", "number", "numbers", "objective", "part"))
## Detect bigrams
ECB_speeches <- tokens_compound(
  ECB_speeches,
  sapply(quanteda.textstats::textstat_collocations(ECB_speeches, min_count = 1500, size = 2)$collocation, phrase)
)

ECB_speeches <- tokens_tolower(ECB_speeches)

## Frequency-based cleaning
dfm <- dfm(ECB_speeches)
dfm

dfm |> dfm_remove(c(stopwords("en"), "non")) |>
  dfm_trim(min_docfreq = .02, docfreq_type = "prop") |>
  dfm_trim(max_docfreq = 1, docfreq_type = "prop") |>
  dfm_select(min_nchar = 3) -> dfm2

dfm(toks2)

intersect(
  c(stopwords("en"), "speech", "see", "ladies", "gentlemen", "year", "years", "also", "can", "may", "new", "one", "time", "important", "level", "however", "need", "first", "even", "let", "like", "two", "three", "four", "five", "framework", "second", "third", "now", "well", "since", "recent", "process", "current", "large", "many", "future", "low", "still", "way", "high", "across", "key", "today", "last", "main", "different", "much", "take", "make", "higher", "lower", "made", "towards", "within", "period", "view", "past", "present", "already", "use", "ensure", "fact", "analysis", "number", "numbers", "objective", "part"),
  unlist(LoughranMcDonald, use.names = FALSE)
)

toks <- as.tokens(dfm2, tokens = ECB_speeches, padding = TRUE, ignore_list = unlist(LoughranMcDonald, use.names = FALSE))

## Subset on dates to limit dataset size
toks <- toks[toks$.date >= "2016-01-01"]
toks <- toks[ntoken(toks) > 200]

ECB_speeches <- toks

usethis::use_data(ECB_speeches, overwrite = TRUE)


# Press conference --------------------------------------------------------

library(quanteda)
ECB_press_conference <- get_ECB_press_conferences()
# ECB_press_conference2 <- ECB_press_conference[, list(
#   date = unique(date),
#   title = paste0(unique(title), " -- ", section_title[1]),
#   content = paste0(content, collapse = "\n\n")
# ), by = "doc_id"]

# ECB_press_conference2 <- corpus(ECB_press_conference2, docid_field = "doc_id", text_field = "content")
# ECB_press_conference22 <- corpus_reshape(ECB_press_conference2, "paragraphs")
# ECB_press_conference22
ECB_press_conference <- corpus(ECB_press_conference, docid_field = "paragraph_id", text_field = "content")
# 
# head(attr(ECB_press_conference22, "docvars"))
# head(attr(ECB_press_conference, "docvars"))
# 
# waldo::compare(ECB_press_conference, ECB_press_conference22)

attr(ECB_press_conference, "meta")$object$unit <- "paragraph"
attr(ECB_press_conference, "docvars")$docid_ <- factor(ECB_press_conference$doc_id, levels = unique(ECB_press_conference$doc_id))
data.table::setnames(attr(ECB_press_conference, "docvars"), "date", ".date")
ECB_press_conference$.date <- as.Date(ECB_press_conference$.date)
# ee <- corpus_reshape(ECB_press_conference, "documents")

{
  s <- sentometrics::compute_sentiment(
    ECB_press_conference,
    lexicons = sentometrics::sento_lexicons(sentometrics::list_lexicons["LM_en"],
                                            sentometrics::list_valence_shifters$en),
    how = "proportional"
  )
  ECB_press_conference$.sentiment <- s$LM_en
  rm(s)
}
usethis::use_data(ECB_press_conference, overwrite = TRUE)

## Subset paragraphs too short
{
  n <- ntoken(ECB_press_conference, remove_punct = TRUE,
              remove_symbols = TRUE, remove_numbers = TRUE)
  # summary(n)
  # aggregate(n, list(ECB_press_conference$doc_id), sum)$x |> hist()
  ECB_press_conference_tokens <- ECB_press_conference[n > 10]
}

{
  spacyr::spacy_initialize(entity = FALSE)
  (parsed <- spacyr::spacy_parse(ECB_press_conference_tokens,
                                 entity = FALSE, nounphrase = FALSE)) |>
    system.time() |>
    print()
  toks <- quanteda::as.tokens(subset(parsed, pos %in% c("NOUN", "ADJ", "PROPN")), use_lemma = TRUE)
  ECB_press_conference_tokens <- ECB_press_conference_tokens[names(ECB_press_conference_tokens) %in% names(toks)]
  {
    # toks$.date <- as.Date(ECB_press_conference_tokens$.date)
    # ECB_press_conference_tokens$date <- NULL
  }
  {
    # toks$.sentiment <- ECB_press_conference_tokens$.sentiment
    # ECB_press_conference_tokens$sentiment <- NULL
  }
  docvars(toks) <- docvars(ECB_press_conference_tokens)
  ECB_press_conference_tokens <- toks
}

## Remove set of stopwords
ECB_press_conference_tokens <- tokens_remove(ECB_press_conference_tokens, c(stopwords("en", source = "marimo"), "lady","term", "long", "medium", "short", "gentleman", "speech", "see", "ladies", "gentlemen", "year", "years", "also", "can", "may", "new", "one", "time", "important", "level", "however", "need", "first", "even", "let", "like", "two", "three", "four", "five", "framework", "second", "third", "now", "well", "since", "recent", "process", "current", "large", "many", "future", "low", "still", "way", "high", "across", "key", "today", "last", "main", "different", "much", "take", "make", "higher", "lower", "made", "towards", "within", "period", "view", "past", "present", "already", "use", "ensure", "fact", "analysis", "number", "numbers", "objective", "part"))
## Detect bigrams
quanteda.textstats::textstat_collocations(ECB_press_conference_tokens, min_count = 500, size = 2)
ECB_press_conference_tokens <- tokens_compound(
  ECB_press_conference_tokens,
  sapply(quanteda.textstats::textstat_collocations(ECB_press_conference_tokens, min_count = 500, size = 2)$collocation, phrase)
)

ECB_press_conference_tokens <- tokens_tolower(ECB_press_conference_tokens)

length(types(ECB_press_conference_tokens))

## Frequency-based cleaning
dfm <- dfm(ECB_press_conference_tokens)
dfm

dfm |> dfm_remove(c(stopwords("en", source = "marimo"), "non")) |>
  dfm_trim(min_docfreq = 10) |>
  dfm_trim(max_docfreq = 1, docfreq_type = "prop") |>
  dfm_select(min_nchar = 3) -> dfm2
dfm2

ECB_press_conference_tokens <- as.tokens(dfm2, tokens = ECB_press_conference_tokens, padding = TRUE, ignore_list = unlist(LoughranMcDonald, use.names = FALSE))

usethis::use_data(ECB_press_conference_tokens, overwrite = TRUE)
