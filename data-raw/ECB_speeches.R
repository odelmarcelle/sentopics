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
