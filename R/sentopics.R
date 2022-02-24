
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

#' Example dataset
#'
#' @docType data
#'
#' @description ...
#'
#' @usage data("ECB_press_conferences")
#'
#' @examples
#' data("ECB_press_conferences")
#' head(ECB_press_conferences)
#'
#' @format A \code{quanteda::corpus} object.
#'
#' @source \url{www.ecb.europa.eu/press/key/html/index.en.html}.
"ECB_press_conferences"

#' Example tokenized speeches
#'
#' @docType data
#'
#' @description ...
#'
#' @usage data("ECB_press_conferences_tokens")
#'
#' @examples
#' data("ECB_press_conferences_tokens")
#' head(ECB_press_conferences_tokens)
#'
#' @format A \code{quanteda::tokens} object.
#'
#' @source \url{www.ecb.europa.eu/press/key/html/index.en.html}.
"ECB_press_conferences_tokens"

#' Loughran-McDonald lexicon
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

#' Picault-Renault lexicon
#'
#' @docType data
#'
#' @description ...
#'
#' @usage data("PicaultRenault")
#'
#' @examples
#' data("PicaultRenault")
#' summary(PicaultRenault)
#'
#' @format A \code{data.table}.
#'
#' @source \url{http://www.cbcomindex.com/lexicon.php}
"PicaultRenault"

#' Regression dataset
#'
#' @docType data
#'
#' @description ...
#'
#' @usage data("PicaultRenault_data")
#'
#' @examples
#' data("PicaultRenault_data")
#' summary(PicaultRenault_data)
#'
#' @format A \code{xts}.
#'
#' @source \url{https://sraf.nd.edu/textual-analysis/resources/}
"PicaultRenault_data"
