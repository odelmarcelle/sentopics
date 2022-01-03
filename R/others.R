#' Download and pre-process speeches from the European Central Bank
#'
#' @description This helper function automatically retrieve the full data set of
#'   speeches made available by the ECB. In addition, it implements a number of
#'   pre-processing steps that may be turned on or off as needed.
#'
#' @param filter_english if `TRUE`, attempts to select English speeches only
#'   using [textcat::textcat()].
#' @param clean_footnotes if `TRUE`, attempts to clean footnotes from speeches
#'   texts using some regex patterns.
#' @param compute_sentiment if `TRUE`, computes the sentiment of each speech
#'   using [sentometrics::compute_sentiment()] with the the Loughran & McDonald
#'   lexicon.
#' @param tokenize_w_POS if `TRUE`, tokenize and apply Part-Of-Speech tagging
#'   with [spacyr::spacy_parse()]. Nouns, adjectives and proper nouns are then
#'   extracted from the parsed speechs to form a `tokens` object.
#'
#' @return Depending on the arguments, returns either a data.frame or a [quanteda::tokens] object containing speeches of the ECB.
#' @export
get_ECB_speeches <- function(filter_english = TRUE, clean_footnotes = TRUE, compute_sentiment = TRUE, tokenize_w_POS = FALSE) {
  ## CMD check
  pos <- NULL

  mis <- c()
  if (filter_english) mis <- c("textcat")
  if (clean_footnotes) mis <- c("stringr")
  if (compute_sentiment) mis <- c(mis, "sentometrics")
  if (tokenize_w_POS) mis <- c(mis, "spacyr")
  mis <- missingSuggets(mis)
  if (length(mis) > 0) stop("Suggested packages are missing for the function with the current arguments.\n",
                            "Please install first the following packages: ",
                            paste0(mis, collapse = ", "),".\n",
                            "Install command: install.packages(",
                            paste0("'", mis, "'", collapse = ", "),")" )

  utils::download.file(
    "https://www.ecb.europa.eu/press/key/shared/data/all_ECB_speeches.csv",
    file <- tempfile())

  ECB_speeches <- utils::read.table(
    file,
    sep = "|",
    quote = "",
    fill = TRUE,
    header = TRUE,
    encoding = "UTF-8",
    stringsAsFactors = FALSE
  )

  unlink(file)

  if (filter_english) {
    ## Filter on language
    set.seed(123)
    LANG <- textcat::textcat(ECB_speeches$contents)
    LANG[is.na(LANG)] <- "missing_content"
    ECB_speeches <- ECB_speeches[LANG == "english", ]
  }

  if (clean_footnotes) {
    ## Attempts to clear footnotes
    {
      references1 <- stringr::str_match(ECB_speeches$contents, stringr::regex("\\[1\\].+(\\[1\\].+)", ignore_case = TRUE))[, 2, drop = FALSE]
      clean <- stringr::str_replace(ECB_speeches$contents, stringr::regex("(\\[1\\].+)(\\[1\\].+)", ignore_case = TRUE), "\\1")
      clean[is.na(references1)] <- stringr::str_replace(
        ECB_speeches$contents[is.na(references1)],
        stringr::regex("(([A-z][a-z]*, [^[,]]{0,50}?)|([A-z]+ [^[,]]{0,20}?))\\([0-9]{4}\\)[,:\\.].*",
                       ignore_case = TRUE), "")

      if (FALSE) {
        check <- data.table(
          check1 = stringr::str_match(clean, stringr::regex("\\[1\\].+(\\[1\\].+)", ignore_case = TRUE))[, 2],
          check2 = stringr::str_extract(clean, stringr::regex("(([A-z][a-z]*, [^[,]]{0,50}?)|([A-z]+ [^[,]]{0,20}?))\\([0-9]{4}\\)[,:\\.].*", ignore_case = TRUE))
        )
        View(check)
        dt <- data.table(
          sub = nchar(ECB_speeches),
          clean = nchar(clean)
        )
        dt
      }

      clean <- stringr::str_replace_all(clean, stringr::regex("[\\(\\[][0-9]+[\\)\\]]", ignore_case = TRUE), "")
      ECB_speeches$contents <- clean
      rm(clean, references1)
    }
  }

  if (compute_sentiment) {
    s <- sentometrics::compute_sentiment(
      ECB_speeches$contents,
      lexicons = sentometrics::sento_lexicons(sentometrics::list_lexicons["LM_en"],
                                              sentometrics::list_valence_shifters$en),
      how = "proportionalPol"
    )
    ECB_speeches$sentiment <- s$LM_en
    rm(s)
  }


  if (tokenize_w_POS) {
    spacyr::spacy_initialize()
    (parsed <- spacyr::spacy_parse(ECB_speeches$contents)) |> system.time()
    toks <- quanteda::as.tokens(subset(parsed, pos %in% c("NOUN", "ADJ", "PROPN")), use_lemma = TRUE)
    toks$.date <- ECB_speeches$date
    if (compute_sentiment) toks$.sentiment <- ECB_speeches$sentiment
    ECB_speeches <- toks
  }

  ECB_speeches
}
