
#' @useDynLib sentopics,.registration = TRUE
#' @importFrom Rcpp evalCpp sourceCpp
NULL

#' @title Tools for joining sentiment and topic analysis (sentopics)
#'
#' @description **sentopics** provides function to easily estimate a range of
#'   topic models and process their output. Particularly, it facilitates the
#'   integration of topic analysis with a time dimension through time-series
#'   generating functions. In addition, **sentopics** interacts with sentiment
#'   analysis to compute the sentiment conveyed by topics. Finally, the package
#'   implements a number of visualization helping interpreting the results of
#'   topic models.
#'
#' @section Usage:
#'
#'   Please refer to the vignettes for a comprehensive introduction to the
#'   package functions.
#'   
#'   - [Basic usage](../doc/Basic_usage.html): Introduction to topic model estimation with **sentopics**
#'   - [Topical time series](../doc/Basic_usage.html): Integrate topic analysis with sentiment analysis along a time dimension
#'   
#'   For further details, you may browse the package [documentation](../html/00Index.html).
#'
#'
#' @note Please cite the package in publications. Use
#'   \code{citation("sentopics")}.
#'
"_PACKAGE"

#' Corpus of press conferences from the European Central Bank
#'
#' @description A corpus of 260 ECB press conference, split into 4224
#'   paragraphs. The corpus contains a number of *docvars* indicating the date
#'   of the press conference and a measured sentiment based on the
#'   Loughran-McDonald lexicon.
#'
#'
#' @examples
#' docvars(ECB_press_conferences)
#'
#' @format A [quanteda::corpus] object.
#' @seealso [ECB_press_conferences_tokens]
#'
#' @source \url{https://www.ecb.europa.eu/press/key/date/html/index.en.html}.
"ECB_press_conferences"

#' Tokenized press conferences
#'
#' @description The pre-processed and tokenized version of the
#'   [ECB_press_conferences] corpus of press conferences. The processing
#'   involved the following steps:
#'   
#'   - Subset paragraphs shorter than 10 words
#'   - Removal of stop words
#'   - Part-of-speech tagging, following which only nouns, proper nouns and
#'   adjective were retained.
#'   - Detection and merging of frequent compound words
#'   - Frequency-based cleaning of rare and very common words
#'
#' @examples
#' LDA(ECB_press_conferences_tokens)
#'
#' @format A [quanteda::tokens] object.
#' @seealso [ECB_press_conferences]
#'
#' @source \url{https://www.ecb.europa.eu/press/key/date/html/index.en.html}.
"ECB_press_conferences_tokens"

#' Loughran-McDonald lexicon
#'
#' @description The Loughran-McDonald lexicon for financial texts adapted for
#'   usage in **sentopics**. The lexicon is enhanced with two list of
#'   valence-shifting words.
#'
#' @seealso [JST()], [rJST()]
#'
#' @examples
#' JST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#'
#' @format A [quanteda::dictionary] containing two polarity categories (negative
#'   and positive) and two valence-shifting categories (negator and amplifier).
#'
#' @source \url{https://sraf.nd.edu/loughranmcdonald-master-dictionary/} for the
#'   lexicon and [lexicon::hash_valence_shifters] for the valence shifters.
#'
#' @references Loughran, T. & McDonald, B. (2011). [When Is a Liability Not a
#'   Liability? Textual Analysis, Dictionaries, and
#'   10-Ks](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1331573). *The Journal of
#'   Finance*, 66(1), 35--65.
"LoughranMcDonald"

#' Picault-Renault lexicon
#'
#' @description The Picault-Renault lexicon, specialized in the analysis of
#'   central bank communication. The lexicon identifies a large number of n-grams
#'   and gives their probability to belong to six categories:
#'   
#'   - Monetary Policy - accommodative
#'   - Monetary Policy - neutral
#'   - Monetary Policy - restrictive
#'   - Economic Condition - negative
#'   - Economic Condition - neutral
#'   - Economic Condition - positive
#'
#' @examples
#' head(PicaultRenault)
#'
#' @format A [data.table] object.
#' 
#' @references Picault, M. & Renault, T. (2017). [Words are not all created
#'   equal: A new measure of ECB
#'   communication](https://www.sciencedirect.com/science/article/abs/pii/S0261560617301808). *Journal of
#'    International Money and Finance*, 79, 136--156.
#'
#' @seealso [compute_PicaultRenault_scores()]
#'
#' @source \url{http://www.cbcomindex.com/lexicon.php}
"PicaultRenault"

#' Regression dataset based on Picault & Renault (2017)
#'
#' @description A regression dataset built to partially replicate the result of
#'   Picault & Renault. This dataset contains, for each press conference
#'   published after 2000:
#'   
#'   - The Main Refinancing Rate (MRR) of the ECB set following the press
#'   conference
#'   - The change in the MRR following the press conference
#'   - The change in the MRR observed at the previous press conference
#'   - The Bloomberg consensus on the announced MRR
#'   - The Surprise brought by the announcement, computed as the Bloomberg
#'   consensus minus the MRR following the conference
#'   - The EURO STOXX 50 return on the day of the press conference
#'   - The EURO STOXX 50 return on the day preceding the announcement
#'
#' @examples
#' head(PicaultRenault_data)
#'
#' @format An [xts::xts] object.
#' 
#' @references Picault, M. & Renault, T. (2017). [Words are not all created
#'   equal: A new measure of ECB
#'   communication](https://www.sciencedirect.com/science/article/abs/pii/S0261560617301808). *Journal of
#'    International Money and Finance*, 79, 136--156.
#'
#' @source The data was manually prepared by the author of this package.
"PicaultRenault_data"
