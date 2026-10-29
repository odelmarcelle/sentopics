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
#'   - [Topical time series](../doc/Topical_time_series.html): Integrate topic analysis with sentiment analysis along a time dimension
#'
#'   For further details, you may browse the package [documentation](../html/00Index.html).
#'
#'
#' @note Please cite the package in publications. Use
#'   \code{citation("sentopics")}.
#'
"_PACKAGE"

#' Corpus of ECB monetary policy statements
#'
#' @description A corpus of 290 ECB monetary policy statements and related
#' policy briefings from June 1998 to October 2026, split into 4942 paragraphs.
#' The document variable `.date` records the date of each communication.
#'
#' @details The corpus is drawn from the ECB introductory-statement indexes.
#' Institutional communications unrelated to monetary policy, slide-only
#' entries, podcast links and separate monetary-policy decision releases
#' accompanying a conference statement are excluded. The July 2021 strategy
#' conference and the August 2007 policy briefing are included.
#' Original item and paragraph identifiers are preserved, so a policy
#' statement can be distinguished from another event on the same date.
#' The sample ends with the statement of 29 October 2026.
#' The selection and reasons for exclusion are recorded in the
#' [exclusion list](https://github.com/odelmarcelle/sentopics/blob/master/data-raw/ECB_corpus_exclusions.csv)
#' in the [GitHub repository's data-preparation sources](https://github.com/odelmarcelle/sentopics/tree/master/data-raw).
#' These source files are not installed with the package.
#' [get_ECB_press_conferences()] retrieves all indexed communications before
#' this selection.
#'
#' @examples
#' quanteda::docvars(ECB_press_conferences)
#'
#' @format A [quanteda::corpus] object.
#' @seealso [ECB_press_conferences_tokens], [get_ECB_press_conferences()]
#'
#' @source Annual ECB introductory-statement indexes, 1998 to 2026; for example,
#' \url{https://www.ecb.europa.eu/press/press_conference/monetary-policy-statement/2026/html/index_include.en.html}.
"ECB_press_conferences"

#' Tokenized press conferences
#'
#' @description The pre-processed and tokenized version of the
#'   [ECB_press_conferences] corpus. The processing
#'   involved the following steps:
#'
#'   - Remove paragraphs with ten or fewer lexical tokens
#'   - Removal of stop words
#'   - Part-of-speech tagging, following which only nouns, proper nouns and
#'   adjective were retained.
#'   - Detection and merging of frequent compound words
#'   - Frequency-based cleaning of rare and very common words
#'
#' @details Tokenization and part-of-speech tagging use **spacyr** 1.3.0 with
#'   **spaCy** 3.8.7 and the English model `en_core_web_sm` 3.8.0.
#'
#'   This preprocessing is intended for topic analysis. None of the
#'   negators in [LoughranMcDonald] remains in the shipped tokens, so its
#'   negation handling has no effect on models initialized from this dataset.
#'   For an analysis that needs negation, start from [ECB_press_conferences]
#'   or other original text and retain negators and their neighboring sentiment
#'   words until the model's lexicon preprocessing has processed them. Removing
#'   stop words or selecting parts of speech first can discard this information.
#'
#' @examples
#' LDA(ECB_press_conferences_tokens)
#'
#' @format A [quanteda::tokens] object.
#' @seealso [ECB_press_conferences]
#'
#' @source Annual ECB introductory-statement indexes, 1998 to 2026; for example,
#' \url{https://www.ecb.europa.eu/press/press_conference/monetary-policy-statement/2026/html/index_include.en.html}.
"ECB_press_conferences_tokens"

#' Loughran-McDonald lexicon
#'
#' @description The Loughran-McDonald lexicon for financial texts adapted for
#'   usage in **sentopics**. The lexicon is enhanced with two list of
#'   valence-shifting words.
#'
#' @seealso [JST()], [rJST()]
#'
#' @details Valence shifters operate on the tokens supplied to the model:
#'   recognized sequences of a negator and a polarity word (optionally with an
#'   amplifier between them) are compounded and assigned the opposite polarity.
#'   Shifters already removed during preprocessing cannot affect sentiment.
#'   In particular, [ECB_press_conferences_tokens] contains no lexicon negators;
#'   use the original text with suitable preprocessing when negation matters.
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
#' @references Loughran, T. & McDonald, B. (2011). When Is a Liability Not a
#'   Liability? Textual Analysis, Dictionaries, and
#'   10-Ks. *The Journal of
#'   Finance*, 66(1), 35--65.\doi{10.1111/j.1540-6261.2010.01625.x}
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
#' @format A [data.table::data.table] object.
#'
#' @references Picault, M. & Renault, T. (2017). Words are not all created
#'   equal: A new measure of ECB
#'   communication. *Journal of
#'    International Money and Finance*, 79, 136--156.
#'    \doi{10.1016/j.jimonfin.2017.09.005}
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
#' @references Picault, M. & Renault, T. (2017). Words are not all created
#'   equal: A new measure of ECB
#'   communication. *Journal of
#'    International Money and Finance*, 79, 136--156.
#'    \doi{10.1016/j.jimonfin.2017.09.005}
#'
#' @source The data was manually prepared by the author of this package.
"PicaultRenault_data"
