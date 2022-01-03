
#' @useDynLib sentopics,.registration = TRUE
#' @importFrom Rcpp evalCpp sourceCpp
NULL

#' @title sentopics: tools for estimating and analyzing various classes of sentiment/topic models
#'
#' @description ...
#'
#' @note Please cite the package in publications. Use \code{citation("sentopics")}.
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
