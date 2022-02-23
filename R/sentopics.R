
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
#'   For further details, you may browse the package [documentation](../html/00Index.html)
#'
#'
#' @note Please cite the package in publications. Use
#'   \code{citation("sentopics")}.
#'
#' @references ...
"_PACKAGE"

#' Example tokenized speeches
#'
#' @docType data
#'
#' @description ...
#'
#' @usage data("ECB_speeches")
#'
#' @examples
#' data("ECB_speeches")
#' head(ECB_speeches)
#'
#' @format A \code{quanteda::tokens} object.
#'
#' @source \url{www.ecb.europa.eu/press/key/html/index.en.html}.
"ECB_speeches"

#' Example lexicon
#'
#' @docType data
#'
#' @description ...
#'
#' @usage data("LoughranMcDonald")
#'
#' @examples
#' data("LoughranMcDonald")
#' summary(LoughranMcDonald)
#'
#' @format A \code{data.table}.
#'
#' @source \url{https://sraf.nd.edu/textual-analysis/resources/}
"LoughranMcDonald"


#' Example lexicon2
#'
#' @docType data
#'
#' @description ...
#'
#' @usage data("LSDfr")
#'
#' @examples
#' data("LSDfr")
#' summary(LSDfr)
#'
#' @format A quanteda \code{dictionary}.
#'
#' @source \url{https://www.poltext.org/fr/donnees-et-analyses/lexicoder}
"LSDfr"
