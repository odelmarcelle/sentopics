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
get_ECB_speeches <- function(filter_english = TRUE, clean_footnotes = TRUE, compute_sentiment = TRUE, tokenize_w_POS = FALSE) { # nocov start
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
    spacyr::spacy_initialize(entity = FALSE)
    (parsed <- spacyr::spacy_parse(ECB_speeches$contents,
                                   entity = FALSE, nounphrase = FALSE)) |>
      system.time() |>
      print()
    toks <- quanteda::as.tokens(subset(parsed, pos %in% c("NOUN", "ADJ", "PROPN")), use_lemma = TRUE)
    toks$.date <- as.Date(ECB_speeches$date)
    if (compute_sentiment) toks$.sentiment <- ECB_speeches$sentiment
    ECB_speeches <- toks
  }

  ECB_speeches
} # nocov end


#' Download press conferences from the European Central Bank
#'
#' @description This helper function automatically retrieve the full data set of
#'   press conferences made available by the ECB. Tt implements a number of
#'   pre-processing steps used to remove the Q&A section from the text.
#'
#' @param years the years for which press conferences should be retrieved
#' @param data.table if TRUE, returns a [data.table]. Otherwise, return a list
#'   in which each element is a press conference.
#'
#' @return Depending on the arguments, returns either a data.frame or a
#'   [quanteda::tokens] object containing speeches of the ECB.
get_ECB_press_conferences <- function(years = 1998:2021, data.table = TRUE) {
  
  # R CMD check:
  paragraph_id <- doc_id <- sections <- NULL
  
  res <- lapply(stats::setNames(nm = years), function(year) {
    
    ## Get index page
    utils::download.file(
      sprintf("https://www.ecb.europa.eu/press/pressconf/%s/html/index_include.en.html", year),
      file <- tempfile(),
      quiet = TRUE)
    html <- readChar(file, file.info(file)$size)
    # html <- readLines(file, encoding = "UTF-8", warn = FALSE) |> paste0(collapse = "")
    unlink(file)
    
    ## Get press conferences
    LIST <- paste0(
      "https://www.ecb.europa.eu",
      regmatches(html, gregexpr(r"(/press.*?\.en\.html)", html, perl = TRUE))[[1]]
    ) |> unique()
    message(year)
    LIST <- lapply(
      stats::setNames(LIST,
               regmatches(LIST, regexpr(r"([0-9]{6})", LIST, perl = TRUE)) |> as.Date(format = "%y%m%d")),
      function(url) {
      utils::download.file(
        url,
        file <- tempfile(),
        quiet = TRUE)
      # html <- readChar(file, file.info(file)$size)
      html <- readLines(file, encoding = "UTF-8", warn = FALSE) |> paste0(collapse = "")
      unlink(file)
      html
    })
    
    ## Get main content of the page
    cleaned <- lapply(LIST, function(html)
      regmatches(html, gregexpr("<main >(?s).*</main>", html, perl = TRUE))[[1]])
    
    ## Correct html tags
    {
      cleaned <- lapply(cleaned, function(html) {
        html <- gsub("&amp;", "&", html, perl = TRUE)
        gsub("&nbsp;", " ", html, perl = TRUE)
      })
      check <- lapply(cleaned, function(html)
        regmatches(html, gregexpr(r"(&[A-Za-z]*?;)", html, perl = TRUE))[[1]])
      if ( any(lengths(check) > 0) ) warning("Unknown html entity detected.")
    }
    
    lapply(LIST, function(html)
      regmatches(html, gregexpr(r"(&[A-Za-z]*?;)", html, perl = TRUE))[[1]])
    
    cleaned <- lapply(cleaned, function(html) {
      
      ## Remove address-box at the end of the page
      html <- regmatches(html, gregexpr(r"(<div class="address-box -top-arrow">(?s).*)", html, perl = TRUE), invert = TRUE)[[1]][1]
      
      ## First attempt at removing questions
      html <- sub(r"(<p>[ ]?(<em>)?[ ]?(<strong>)?[ ]?(<em>)?[ ]?"?[ ]?Question[ ]?(\(translation\))?:.*)", "", html, perl = TRUE)
      # regmatches(html, gregexpr(r"(<p>(<em>)?(<strong>)?(<em>)?"?[ ]?Question[ ]?(\(translation\))?:)", html, perl = TRUE), invert = FALSE)
      
      ## First attempt but based on last line of statement
      html <- sub(r"(<p>[ ]?We are now at your disposal for questions.[ ]?</p>\K.*)", "", html, perl = TRUE)
      html <- sub(r"(<p>[ ]?We are now ready to take your questions.[ ]?</p>\K.*)", "", html, perl = TRUE)
      
      ## Second attempt at removing questions
      html <- sub(r"(<h2[A-Za-z "=:]*?>(<strong>)?[ ]?Transcript of the [Qq]uestions.*?</h2>.*)", "", html, perl = TRUE)
      
      ## Third attempt based on specific cases detected with kwic()
      html <- sub(r"(<p>[ ]?(<strong>)?[ ]?My (first )?question would be.*)", "", html, perl = TRUE)
      
      
      
      title <- gsub("<.*?>", "", regmatches(html, gregexpr("<h1.*?>(?s).*?</h1>", html, perl = TRUE))[[1]])
      rest <- gsub("<h1.*?>(?s).*?</h1>", "", html, perl = TRUE)
      sections <- lapply(rest, function(html)
        strsplit(html, r"((<div class="titlepage">)|(\* \* \*))", perl = TRUE)[[1]])[[1]]
      sections <- lapply(sections, function(html) {
        list(
          section_title = gsub("<.*?>", "", regmatches(html, gregexpr("<h2.*?>(?s).*?</h2>", html, perl = TRUE))[[1]])[1],
          content = gsub("<.*?>", "", regmatches(html, gregexpr("<p>(?s).*?</p>", html, perl = TRUE))[[1]])
        )
      })
      
      res <- list(
        title = title,
        sections = sections[sapply(sections, function(x) length(x$content) > 0)]
      )
      # res$date <- res$sections[[1]]$section_title |> parsedate::parse_date() |> as.Date() |> na.omit()
      # if (length(res$date) < 1) {
      #   print("ahahaha")
      #   print(res$sections[[1]]$section_title)
      #   stop("Missing date")
      # }
      res
    })
    
    cleaned
    
    # ## Remove after second <h2> (this removes Q&A)
    # cleaned <- lapply(cleaned, function(html)
    #   regmatches(html, gregexpr("(?s).*?<h2.*?>(?s).*?</h2>(?s).*?<h2.*?>", html, perl = TRUE))[[1]])
    # 
    # ## Break down in paragraphs and remove HTML tags
    # cleaned <- lapply(cleaned, function(html)
    #   list(
    #     title = gsub("<.*?>", "", regmatches(html, gregexpr("<h1.*?>(?s).*?</h1>", html, perl = TRUE))[[1]]),
    #     subtitle = gsub("<.*?>", "", regmatches(html, gregexpr("<h2.*?>(?s).*?</h2>", html, perl = TRUE))[[1]]),
    #     content = gsub("<.*?>", "", regmatches(html, gregexpr("<p>(?s).*?</p>", html, perl = TRUE))[[1]])
    #   )
    # )
    
  })
  
  res <- unlist(unname(res), recursive = FALSE, use.names = TRUE)
  res <- res[order(names(res))]
  res <- mapply(function(x, date) {x$date <- date; x}, x = res, date = names(res), SIMPLIFY = FALSE)
  names(res) <- seq_along(res)
  if (data.table) {
    res <- rbindlist(res, idcol = "doc_id")
    res <- res[, c({tmp <- rbindlist(sections); tmp[, paragraph_id := paste0(doc_id, "_", .I)]; tmp}), by = c("date", "doc_id", "title") ]
    # res[grepl("Transcript", section_title)] |> View()
  }
  res
}



#' Compute scores using the Picault-Renault lexicon
#'
#' @description Computes Monetary Policy and Economic Condition scores using the
#'   Picault-Renault lexicon for central bank communcation.
#'
#' @param x a [quanteda::corpus] object.
#' @param min_ngram the minimum length of n-grams considered in the computation
#' @param return_dfm if `TRUE`, returns the scaled word-per-document score under
#'   two [dfm], on for the Monetary Policy and one for the Economic Condition
#'   categories. If `FALSE`, returns the sum of all word scores per document.
#'
#' @details the computation is done on a per-document basis, such as each
#'   document is scored with a value between -1 and 1. This is relevant to the
#'   computation of the denominator of the score.
#'
#'   It is possible to compute the score for paragraphs and sentences for a
#'   [quanteda::corpus] segmented using [quanteda::corpus_reshape]. Segmenting a
#'   corpus using **quanteda**'s helpers retain track to which document each
#'   paragraph/sentence belong. However, in that case, it is possible that
#'   paragraphs or sentences are scored outside the (-1,1) interval. In any
#'   case, the of the paragraph/sentences scores averaged over documents will be
#'   contained in the (-1,1) interval.
#'
#' @export
#' @seealso [PicaultRenault]
#' @references Picault, M. & Renault, T. (2017). [Words are not all created
#'   equal: A new measure of ECB
#'   communication](https://www.sciencedirect.com/science/article/abs/pii/S0261560617301808). *Journal of
#'   International Money and Finance*, 79, 136--156.
#' @examples
#' \donttest{# on documents
#' docs <- quanteda::corpus_reshape(ECB_press_conferences, "documents")
#' compute_PicaultRenault_scores(docs)
#'
#' # on paragraphs
#' compute_PicaultRenault_scores(ECB_press_conferences)}
compute_PicaultRenault_scores <- function(x, min_ngram = 2, return_dfm = FALSE) {
  PicaultRenault <- PicaultRenault[PicaultRenault$ngram >= min_ngram, ]
  x <- quanteda::tokens(x, remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, remove_separators = FALSE) |>
    quanteda::tokens_remove(" ") |> 
    quanteda::tokens_wordstem() |> 
    quanteda::tokens_tolower()
  
  x <- quanteda::tokens_compound(
    x,
    quanteda::phrase(PicaultRenault$keyword),
    concatenator = " ")
  
  dfm <- quanteda::dfm(x)
  dfm <- dfm[, intersect(colnames(dfm), PicaultRenault$keyword)]
  
  weight <- PicaultRenault[match(colnames(dfm), PicaultRenault$keyword),
                           -c("keyword", "ngram", "total_class")]
  weight <- lapply(weight, stats::setNames, nm = colnames(dfm))
  
  weighted_dfms <- lapply(weight, function(w)
    quanteda::dfm_weight(dfm, weight = w))

  MP <- quanteda::as.dfm(weighted_dfms$mp_rest - weighted_dfms$mp_acco)
  # MP <- MP/sum(Reduce(`+`, weighted_dfms[c("mp_acco", "mp_neut", "mp_rest")]))
  denom <- as.matrix(Reduce(`+`, weighted_dfms[c("mp_acco", "mp_neut", "mp_rest")]))
  denom <- rowSums(denom) |> stats::aggregate(mean, by = list(doc = quanteda::docid(dfm)))
  ## Prevents 0 denominator
  denom$x[denom$x < sqrt(.Machine$double.eps)] <- sqrt(.Machine$double.eps)
  MP <- as.matrix(MP)
  for (i in seq_len(nrow(MP))) {
    MP[i, ] <- MP[i, ] /  denom[match(quanteda::docid(dfm)[i], denom$doc), ]$x
  }
  MP <- quanteda::as.dfm(MP)
  quanteda::docvars(MP) <- quanteda::docvars(dfm)
  
  EC <- quanteda::as.dfm(weighted_dfms$ec_posi - weighted_dfms$ec_nega)
  denom <- as.matrix(Reduce(`+`, weighted_dfms[c("ec_nega", "ec_neut", "ec_posi")]))
  denom <- rowSums(denom) |> stats::aggregate(mean, by = list(doc = quanteda::docid(dfm)))
  ## Prevents 0 denominator
  denom$x[denom$x < sqrt(.Machine$double.eps)] <- sqrt(.Machine$double.eps)
  EC <- as.matrix(EC)
  for (i in seq_len(nrow(EC))) {
    EC[i, ] <- EC[i, ] /  denom[match(quanteda::docid(dfm)[i], denom$doc), ]$x
  }
  EC <- quanteda::as.dfm(EC)
  quanteda::docvars(EC) <- quanteda::docvars(dfm)
  
  if (return_dfm) return(list(MP = MP, EC = EC))
  else cbind(MP = rowSums(as.matrix(MP)), EC = rowSums(as.matrix(EC)))
}


