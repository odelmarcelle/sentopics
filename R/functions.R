
#' Extract the most representative words from topics
#'
#' @author Olivier Delmarcelle
#'
#' @description Extract the top words in each topic/sentiment from a
#'   `sentopicmodel`.
#'
#' @param x a `sentopicmodel` created from the [LDA()], [JST()] or [rJST()]
#' @param nWords the number of top words to extract
#' @param method specify if a re-ranking function should be applied before
#'   returning the top words. See Details for a description of each method. 
#' @param output determines the output of the function
#' @param subset allows to subset using a logical expression, as in [subset()].
#'   Particularly useful to limit the number of observation on plot outputs. The
#'   logical expression uses topic and sentiment *indices* rather than their
#'   label. It is possible to subset on both topic and sentiment but adding a
#'   `&` operator between two expressions.
#' @param w only used when `method = "FREX"`. Determines the weight assigned to
#'   the exclusivity score at the expense of the frequency score.
#'
#' @return The top words of the topic model. Depending on the output chosen, can
#'   result in either a long-style data.frame, a `ggplot2` object or a matrix.
#'
#' @details `"frequency"` ranks top words according to their frequency
#'   within a topic. This method also reports the overall frequency of
#'   each word. When returning a plot, the overall frequency is
#'   represented with a grey bar.
#'
#'   `"probability"` uses the estimated topic-word mixture \eqn{\phi} to
#'   rank top words.
#'
#'   `"term-score"` implements the re-ranking method from Blei and
#'   Lafferty (2009). This method down-weights terms that have high
#'   probability in all topics using the following score:
#'   \deqn{\text{term-score}_{k,v} = \phi_{k, v}\log\left(\frac{\phi_{k,
#'   v}}{\left(\prod^K_{j=1}\phi_{j,v}\right)^{\frac{1}{K}}}\right),} for
#'   topic \eqn{k}, vocabulary word \eqn{v} and number of topics \eqn{K}.
#'
#'   `"FREX"` implements the re-ranking method from Bischof and Airoldi
#'   (2012). This method used the weight \eqn{w} to balance between
#'   topic-word probability and topic exclusivity using the following
#'   score:
#'   \deqn{\text{FREX}_{k,v}=\left(\frac{w}{\text{ECDF}\left(
#'   \frac{\phi_{k,v}}{\sum_{j=1}^K\phi_{k,v}}\right)}
#'   + \frac{1-w}{\text{ECDF}\left(\phi_{k,v}\right)} \right),} for
#'   topic \eqn{k}, vocabulary word \eqn{v}, number of topics \eqn{K} and
#'   weight \eqn{w}, where \eqn{\text{ECDF}} is the empirical cumulative
#'   distribution function.
#'
#'   
#' @references Blei, DM. and Lafferty, JD. (2009). [Topic
#'   models.](https://doi.org/10.1201/9781420059458-12). In *Text Mining*,
#'   chapter 4, 101--124.
#' 
#'   Bischof JM. and Airoldi, EM. (2012). [Summarizing Topical Content
#'   with Word Frequency and
#'   Exclusivity.](https://dl.acm.org/doi/10.5555/3042573.3042578). In
#'   *Proceedings of the 29th International Coference on International
#'   Conference on Machine Learning*, ICML'12, 9--16.
#'   
#'   
#' @import data.table
#' @export
#' @seealso [melt.sentopicmodel()] for extracting estimated mixtures
#' @examples
#' model <- LDA(ECB_press_conferences_tokens)
#' model <- fit(model, 10)
#' topWords(model)
#' topWords(model, output = "matrix")
#' topWords(model, method = "FREX")
topWords <- function(x,
                     nWords = 10,
                     method = c("frequency", "probability", "term-score", "FREX"),
                     output = c("data.frame", "plot", "matrix"),
                     subset,
                     w = .5) {

  ## CMD check
  word <- value <- overall <- NULL

  if (x$it < 1) stop("No top words yet. Iterate the model with fit() first.")
  class <- class(x)[1]
  method <- match.arg(method)
  output <- match.arg(output)
  x <- reorder_sentopicmodel(x)
  top <- topWords_dt(x, nWords, method, w)
  if (!missing(subset)) {
    if (attr(x, "reversed")) env <- list(topic = quote(L1),
                                         sentiment = quote(L2))
    else env <- list(sentiment = quote(L1),
                     topic = quote(L2))
    subset <- do.call(substitute, list(
      substitute(subset),
      env))
    top <- subset(top, eval(subset))
  }
  
  switch(output,
         "matrix" = {
           res <- matrix(top$word, nrow = nWords)
           tmp <- top$L2 + (x$L2) * (top$L1 - 1)
           colnames(res) <- create_labels(x, class)[unique(tmp)]
           res
         },
         "plot" = {
           mis <- missingSuggets(c("ggplot2", "RColorBrewer"))
           if (length(mis) > 0) stop("Suggested packages are missing for the plot output.\n",
                                     "Please install first the following packages: ",
                                     paste0(mis, collapse = ", "),".\n",
                                     "Install command: install.packages(",
                                     paste0("'", mis, "'", collapse = ", "),")" )
           tmp <- top$L2 + (x$L2) * (top$L1 - 1)
           top$label <- factor(
             tmp,
             levels = seq_len(x$L1 * x$L2),
             labels = create_labels(x, class)
           )
           p <- ggplot2::ggplot(
             top,
             ggplot2::aes(
               reorder_within(
                 reorder_within(word, value, L1),
                 value, L2),
               value, L1, fill = factor(L1))) +
             {if (attr(top, "method") == "frequency")
               ggplot2::geom_col(
                 ggplot2::aes(y = overall),
                 show.legend = FALSE,
                 fill = "grey", alpha = .8
               )} +
             ggplot2::geom_col(show.legend = FALSE) +
             ggplot2::facet_wrap(
               . ~ label,
               strip.position = "top",
               scales = "free"
             ) +
             ggplot2::coord_flip() +
             scale_x_reordered() +
             ggplot2::labs(x = "word", y = attr(top, "method")) +
             ggplot2::theme(
               strip.text.x = ggplot2::element_text(
                 margin = ggplot2::margin(.5,0,3,0)
               ))
           p
         },
         "data.frame" = {
           params <- sentopicmodel_params(x)
           labs <- create_labels(x, class, flat = FALSE)
           for (i in names(labs)) {
             top[[i]] <- factor(top[[i]], levels = seq_along(labs[[i]]), labels = labs[[i]])
           }
           if (class %in% c("rJST", "LDA")) colnames(top) <-
             sub("L1", "topic", sub("L2", "sentiment", colnames(top), fixed = TRUE), fixed = TRUE)
           if (class == "JST") colnames(top) <-
             sub("L1", "sentiment", sub("L2", "topic", colnames(top), fixed = TRUE), fixed = TRUE)
           if (class == "LDA") top$sentiment <- NULL
           top
         }
  )
}

topWords_dt <- function(x,
                        nWords = 10,
                        method = c("frequency", "probability", "term-score", "FREX"),
                        w = .5) {

  ## CMD check
  word <- value <- prob <- tprob <- sprob <- exclusivity <- NULL
  phiStats <- x$phi
  # Removing dimnames in order to have topic and sentiment number only in the next table
  dimnames(phiStats)[2:3] <- list(NULL, NULL)
  phiStats <- data.table::as.data.table(phiStats, sort = FALSE)
  colnames(phiStats) <- c("word", "L2", "L1", "value")

  # Add epsilon to avoid division per zero
  epsilon <- 10^-100

  method <- match.arg(method)
  
  nClusters <- max(phiStats$L1) * max(phiStats$L2)
  switch(method,
         "frequency" = {
           freq <- rebuild_zw(x, array = TRUE)
           freq <- aperm(freq, c(3, 1, 2))
           overall <- rowSums(freq)
           freq <- data.table::as.data.table(freq, sort = FALSE)
           colnames(freq) <- c("word", "L2", "L1", "value")
           freq$overall <- rep(overall, times = x$L1 * x$L2)
           freq$word <- phiStats$word
           phiStats <- freq
         },
         "term-score" = {phiStats <-
           phiStats[ #apply smoothing on value to avoid 0 probability from lexicon words
             , list(word, L1, L2, value = value + .Machine$double.eps)][
               , list(L1, L2, value = value * log(value / prod(value)^(1/nClusters))), by = word]},
         "FREX" = {
           
           if (w < 0 | w > 1) stop("The argument 'w' should be constrained between 0 and 1.")
           
           phiStats[, "exclusivity" := value / sum(value), by = word]
           phiStats <-
             phiStats[, list(word, value = (
               w / (data.table::frank(exclusivity) / .N) +
                 (1 - w) / (data.table::frank(value) / .N)
               )^-1 ), by = c("L1", "L2")]
         },
         "topics" = {
           ## TODO: to update?
           #### disregard sentiments and compute probability mass for each L1
           tmp <- melt(x)[, list(prob = mean(prob), tprob = mean(tprob)), by = list(L1, L2)]
           tmp[, sprob := prob/tprob]
           phiStats <- phiStats[tmp, on = c("L1", "L2")][, value := value*sprob]
           phiStats <- phiStats[, list(value = sum(value)), by = list(word, L1)]
           phiStats[, L2 := 1L]
         }
  )
  topWords <- phiStats[order(-value, word), utils::head(.SD, nWords),
                       by = list(L1, L2)][order(L1, L2)]
  class(topWords) <- c("topWords", class(phiStats))
  attr(topWords, "method") <- method
  topWords
}

#' @rdname topWords
#' @export
#' @examples 
#' plot_topWords(model)
#' plot_topWords(model, subset = topic %in% 1:2)
#' 
#' jst <- JST(ECB_press_conferences_tokens)
#' jst <- fit(jst, 10)
#' plot_topWords(jst)
#' plot_topWords(jst, subset = topic %in% 1:2 & sentiment == 3)
plot_topWords <- function(x,
                          nWords = 10,
                          method = c("frequency", "probability", "term-score", "FREX"),
                          subset,
                          w = .5) {
  eval(substitute(
    topWords(x, nWords, method, output = "plot", e, w),
    list(e = substitute(subset))
  ))
}


#' Coherence of estimated topics
#'
#' @author Olivier Delmarcelle
#'
#' @description Computes various coherence based metrics for topic models. It
#'   assesses the quality of estimated topics based on co-occurrences of words.
#'   For best results, consider cleaning the initial tokens object with `padding
#'   = TRUE`.
#'
#' @param x a model created from the [LDA()], [JST()] or [rJST()] function and
#'   estimated with \code{\link[=fit.sentopicmodel]{fit()}}
#' @param method the coherence method used.
#' @param nWords the number of words in each topic used for evaluation.
#' @param window optional. If `NULL`, use the default window for each coherence
#'   metric (10 for C_NPMI and 110 for C_V). It is possible to override these
#'   default windows by providing an integer or `"boolean"` to this argument,
#'   determining a new window size for all measures. No effect is the `NPMIs`
#'   argument is also provided.
#' @param NPMIs optional NPMI matrix. If provided, skip the computation of NPMI
#'   between words, substantially decreasing computing time.
#'
#' @return A vector or matrix containing the coherence score of each topic.
#' @details Currently, only C_NPMI and C_V are documented. The implementation
#'   follows Röder & al. (2015). For C_NPMI, the sliding window is 10 whereas it
#'   is 110 for C_V.
#'
#' @references Röder, M., Both, A., & Hinneburg, A. (2015). [Exploring the Space
#'   of Topic Coherence
#'   Measures](https://dl.acm.org/doi/10.1145/2684822.2685324). In *Proceedings
#'   of the Eighth ACM International Conference on Web Search and Data Mining*,
#'   399-–408.
#' @export
coherence <- function(x, nWords = 10, method = c("C_NPMI", "C_V"), window = NULL, NPMIs = NULL) {
  UseMethod("coherence")
}
#' @export
coherence.LDA <- function(x, nWords = 10, method = c("C_NPMI", "C_V"), window = NULL, NPMIs = NULL) {
  res <- coherence(as.sentopicmodel(x), nWords, method, window, NPMIs)
  rowSums(res)
}
#' @export
coherence.JST <- function(x, nWords = 10, method = c("C_NPMI", "C_V"), window = NULL, NPMIs = NULL) {
  ### TODO: need to make this more robust
  coherence(as.sentopicmodel(x), nWords, method, window, NPMIs)
}
#' @export
coherence.rJST <- function(x, nWords = 10, method = c("C_NPMI", "C_V"), window = NULL, NPMIs = NULL) {
  coherence(as.sentopicmodel(x), nWords, method, window, NPMIs)
}

#' @export
coherence.sentopicmodel <- function(x, nWords = 10, method = c("C_NPMI", "C_V"), window = NULL, NPMIs = NULL) {
  method <- match.arg(method)

  if (is.null(window)) {
    window_C_NPMI <- 10
    window_C_V <- 110
  } else {
    window_C_NPMI <- window_C_V <- window
  }

  stopifnot(is.numeric(nWords))
  nWords <- as.integer(nWords)
  if (nWords < 2) stop("The number of words must be at least 2.")
  if (inherits(x, "sentopicmodel")) {
    switch(method,
            C_NPMI = C_NPMI(x, nWords, window_C_NPMI, NPMIs = NPMIs),
            C_V = C_V(x, nWords, window_C_V, NPMIs = NPMIs),
            stop("Undefined method"))
  } else {
    stop("Please provide a correct sentopicmodel object")
  }
}


#' Distances between topic models (chains)
#'
#' @author Olivier Delmarcelle
#'
#' @description Computes the distance between different estimates of a topic
#'   model. Since the estimation of a topic model is random, the results may
#'   largely differ as the process is repeated. This function allows to compute
#'   the distance between distinct realizations of the estimation process.
#'   Estimates are referred to as *chains*.
#'
#' @param x a valid `multiChains` object, obtained through the estimation of a
#'   topic model using \code{\link[=fit.sentopicmodel]{fit()}} and the argument
#'   `nChains` greater than `1`.
#' @param method the method used to measure the distance between chains.
#' @param ... further arguments passed to internal distance functions.
#'
#' @details The `method` argument determines how are computed distance.
#' 
#'  - `euclidean` finds the pairs of topics that minimizes and returns the total
#'  Euclidean distance.
#'  - `hellinger` does the same but based on the Hellinger distance.
#'  - `cosine` does the same but based on the Cosine distance.
#'  - `minMax` computes the maximum distance among the best pairs of distances.
#'  Inspired by the *minimum-matching distance* from Tang et al. (2014).
#'  - `naiveEuclidean` computes the Euclidean distance without searching for the
#'  best pairs of topics.
#'  - `invariantEuclidean` computes the best pairs of topics for all allowed
#'  permutations of topic indices. For JST and reversed-JST models, the two-
#'  levels hierarchy of document-sentiment-topic leads some permutations of
#'  indices to represent a drastically different outcome. This setting restricts
#'  the set of permutations to the ones that do not change the interpretation of
#'  the model. Equivalent to `euclidean` for LDA models.
#'
#' @return A matrix of distance between the elements of `x`
#' @examples
#' model <- LDA(ECB_press_conferences_tokens)
#' model <- fit(model, 10, nChains = 5)
#' chainsDistances(model)
#'
#' @seealso [plot.multiChains()] [chainsScores()]
#' @references Tang, J., Meng, Z., Nguyen, X., Mei, Q., and Zhang, M. (2014).
#'   [Understanding the Limiting Factors of Topic Modeling via Posterior
#'   Contraction Analysis](https://proceedings.mlr.press/v32/tang14.html). In
#'   *Proceedings of the 31st International Conference on Machine Learning*, 32,
#'   90--198.
#' @export
chainsDistances <- function(x,
                            method = c("euclidean", "hellinger", "cosine", "minMax", "naiveEuclidean", "invariantEuclidean"),
                            ...) {
  if (!inherits(x, "multiChains")) stop("Please provide a correct multiChains object")

  x <- as.sentopicmodel(x)
  # avoid copying base to each chain
  x <- as.list(x, copy = FALSE)
    
  method <- match.arg(method)
  switch(method,
         "cosine" = cosineDistances(x),
         "hellinger" = hellingerDistances(x),
         "euclidean" = euclideanDistances(x),
         "naiveEuclidean" = naiveEuclideanDistances(x),
         "minMax" = minMaxEuclidean(x),
         "invariantEuclidean" = invariantEuclideanOptim(x, ...),
         stop("Error in distance method")
  )
}

#' Compute scores of topic models (chains)
#'
#' Compute various scores (log likelihood, coherence) for a list of topic
#' models.
#'
#' @param x a valid `multiChains` object, obtained through the estimation of a
#'   topic model using \code{\link[=fit.sentopicmodel]{fit()}} and the argument
#'   `nChains` greater than `1`.
#' @param nWords the number of words used to compute coherence. See
#'   [coherence()].
#' @param window optional. If `NULL`, use the default window for each coherence
#'   metric (10 for C_NPMI and 110 for C_V). It is possible to override these
#'   default windows by providing an integer or `"boolean"` to this argument,
#'   determining a new window size for all measures.
#'
#' @return A `data.table` with some statistics about each chain. For the
#'   coherence metrics, the value shown is the mean coherence across all topics
#'   of a chain
#'
#' @inheritSection fit.sentopicmodel Parallelism
#'
#' @examples
#' model <- LDA(ECB_press_conferences_tokens[1:10])
#' model <- fit(model, 10, nChains = 5)
#' chainsScores(model, window = 5)
#' chainsScores(model, window = "boolean")
#' 
#' # -- Parallel computation --
#' require(future.apply)
#' future::plan("multisession", workers = 2) # Set up 2 workers
#' chainsScores(model, window = "boolean")
#'
#' future::plan("sequential") # Shut down workers
#'
#' @seealso [chainsDistances()] [coherence()]
#'
#' @export
chainsScores <- function(x, window = 110, nWords = 10) {
  if (!inherits(x, "multiChains")) stop("Please provide a correct multiChains object")

  ## CMD check
  name <- NULL

  if (is.null(window)) {
    NPMIsW <- computeNPMI(x$tokens, 110)
    NPMIs10 <- computeNPMI(x$tokens, 10)
  } else {
    NPMIsW <- NPMIs10 <- computeNPMI(x$tokens, window)
  }
  
  # avoid copying base to each chain # could be further optimized
  x <- as.list(x, copy = FALSE)
  
  FUN <- function(x) {
    score <- data.table::data.table(
      # name = name,
      logLikelihood = c(utils::tail(x$logLikelihood, 1)),
      max_logLikelihood = max(x$logLikelihood)
    )
    if (score$logLikelihood == 0) {
      multLikelihood <- get("multLikelihood",
                            envir = getNamespace("sentopics"))
      score$logLikelihood <- multLikelihood(x)[1, 1]
    }
    ## Coherence measures
    score$C_NPMI <- mean(sentopics::coherence(x, nWords, method = "C_NPMI", NPMIs = NPMIs10))
    score$C_V <- mean(sentopics::coherence(x, nWords, method = "C_V", NPMIs = NPMIsW))
    score
  }
  if (requireNamespace("future.apply", quietly = TRUE)) {
    environment(FUN) <- globalenv()
    chainsScores <- future.apply::future_sapply(
      x, FUN, future.seed = FALSE,
      future.globals = list(
        nWords = nWords, NPMIsW = NPMIsW, NPMIs10 = NPMIs10
      ))
  } else {
    chainsScores <- sapply(x, FUN)
  }
  
  t(chainsScores)
}
