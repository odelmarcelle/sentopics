% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{coherence}
\alias{coherence}
\title{Coherence of estimated topics}
\usage{
coherence(
  x,
  nWords = 10,
  method = c("C_NPMI", "C_V"),
  window = NULL,
  NPMIs = NULL
)
}
\arguments{
\item{x}{a model created from the \code{\link[=LDA]{LDA()}}, \code{\link[=JST]{JST()}} or \code{\link[=rJST]{rJST()}} function and
estimated with \code{\link[=fit.sentopicsmodel]{fit()}}}

\item{nWords}{the number of words in each topic used for evaluation.}

\item{method}{the coherence method used.}

\item{window}{optional. The maximum distance between two tokens to be
considered \emph{co-occuring} for the coherence measure. Distance is expressed in
token positions. If \code{NULL}, use the default window for each coherence
metric (10 for C_NPMI and 110 for C_V). Providing an integer or \code{"boolean"}
to this argument will override the default. If \code{"boolean"}, co-occurences
will simply take place if the two words are present in the same document.
This argument has no effect if the \code{NPMIs} argument is also provided.}

\item{NPMIs}{optional NPMI matrix. If provided, skip the computation of NPMI
between words, substantially decreasing computing time.}
}
\value{
A vector or matrix containing the coherence score of each topic.
}
\description{
Computes various coherence based metrics for topic models. It
assesses the quality of estimated topics based on co-occurrences of words.
For best results, consider cleaning the initial tokens object with \code{padding = TRUE}.
}
\details{
Currently, only C_NPMI and C_V are documented. The implementation
follows Röder & al. (2015). For C_NPMI, the sliding window is 10 whereas it
is 110 for C_V.
}
\references{
Röder, M., Both, A., & Hinneburg, A. (2015). \href{https://dl.acm.org/doi/10.1145/2684822.2685324}{Exploring the Space of Topic Coherence Measures}. In \emph{Proceedings
of the Eighth ACM International Conference on Web Search and Data Mining},
399-–408.
}
\author{
Olivier Delmarcelle
}
