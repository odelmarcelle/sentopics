#' Create a Latent Dirichlet Allocation model
#'
#' @description This function initialize a Latent Dirichlet Allocation model.
#'
#' @references Blei, D.M., Ng, A.Y. and Jordan, M.I. (2003). [Latent Dirichlet
#'   Allocation](http://www.cs.columbia.edu/~blei/papers/BleiNgJordan2003.pdf).
#'   *Journal of Machine Learning Research*, 3, 993--1022.
#'
#' @inherit rJST
#' @export
#' @family topic models
#' @seealso Fitting a model: \code{\link[=fit.sentopicsmodel]{fit()}}, extracting
#'   top words: [top_words()]
#' @examples
#' \donttest{# creating a model
#' LDA(ECB_press_conferences_tokens, K = 5, alpha = 0.1, beta = 0.01)
#'
#' # estimating an LDA model
#' lda <- LDA(ECB_press_conferences_tokens)
#' lda <- fit(lda, 100)}
LDA <- function(x, K = 5, alpha = 1, beta = 0.01) {
  as.LDA(sentopicsmodel(
    x,
    lexicon = NULL,
    L1 = K,
    L2 = 1,
    L1prior = alpha,
    L2prior = 0,
    beta = beta
  ))
}

#' Create a Reverse Joint Sentiment/Topic model
#'
#' @description This function initialize a Reverse Joint Sentiment/Topic model.
#'
#' @references Lin, C. and He, Y. (2009). [Joint sentiment/topic model for
#' sentiment analysis](https://dl.acm.org/doi/10.1145/1645953.1646003). In *Proceedings
#' of the 18th ACM conference on Information and knowledge management*,
#' 375--384.
#'
#' Lin, C., He, Y., Everson, R. and Ruger, S. (2012). [Weakly Supervised Joint
#' Sentiment-Topic Detection from Text](https://ieeexplore.ieee.org/document/5710933).
#' *IEEE Transactions on Knowledge and Data Engineering*, 24(6), 1134–-1145.
#'
#' @inherit sentopicsmodel
#'
#' @param K the number of topics
#' @param S the number of sentiments
#' @param alpha the hyperparameter of topic-document distribution
#' @param gamma the hyperparameter of sentiment-document distribution
#' @param alphaCycle integer specifying the cycle size between two updates of
#'   the hyperparameter alpha
#' @param gammaCycle integer specifying the cycle size between two updates of
#'   the hyperparameter alpha
#' @param ... not used
#'
#' @export
#' @seealso Fitting a model: \code{\link[=fit.sentopicsmodel]{fit()}}, extracting
#'   top words: [top_words()]
#' @family topic models
#' @examples
#' \donttest{# simple rJST model
#' rJST(ECB_press_conferences_tokens)
#'
#' # estimating a rJST model including lexicon
#' rjst <- rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#' rjst <- fit(rjst, 100)
#'
#' # from an LDA model:
#' lda <- LDA(ECB_press_conferences_tokens)
#' lda <- fit(lda, 100)
#'
#' # creating a rJST model out of it
#' rjst <- rJST(lda, lexicon = LoughranMcDonald)
#' # topic proportions remain identical
#' identical(lda$theta, rjst$theta)
#' # model should be iterated to estimate sentiment proportions
#' rjst <- fit(rjst, 100)}
rJST <- function(x, ...) {
  UseMethod("rJST")
}
#' @rdname rJST
#' @export
rJST.default <- function(
  x,
  lexicon = NULL,
  K = 5,
  S = 3,
  alpha = 1,
  gamma = 5,
  beta = 0.01,
  alphaCycle = 0,
  gammaCycle = 0,
  ...
) {
  as.rJST(sentopicsmodel(
    x,
    lexicon,
    K,
    S,
    alpha,
    gamma,
    beta,
    alphaCycle,
    gammaCycle
  ))
}
#' @rdname rJST
#' @details The `rJST.LDA` methods enable the transition from a previously
#'   estimated [LDA] model to a sentiment-aware `rJST` model. The function
#'   retains the previously estimated topics and randomly assigns sentiment to
#'   every word of the corpus. The new model will retain the iteration count of
#'   the initial [LDA] model.
#' @export
rJST.LDA <- function(x, lexicon = NULL, S = 3, gamma = 5, ...) {
  if (x$it < 1) {
    stop("Requires an estimated LDA model.")
  }
  if (isTRUE(attr(x, "approx"))) {
    stop("Not possible for approximated models")
  }
  x <- as.sentopicsmodel(x)
  x$L2 <- as.numeric(S)

  if (length(gamma) == 1L) {
    gamma <- matrix(gamma, x$L1 * x$L2, 1)
  }
  if (length(gamma) == x$L2) {
    gamma <- matrix(rep(gamma, times = x$L1), x$L1 * x$L2, 1)
  }

  if (length(gamma) != x$L1 * x$L2) {
    stop("Incorrect prior dimension. Please check input gamma.")
  }

  dim(gamma) <- c(x$L1 * x$L2, 1)
  x$L2prior <- gamma

  vocabulary <- makeVocabulary(x$tokens, lexicon, S)
  x$tokens <- vocabulary$toks
  x$vocabulary <- vocabulary$vocabulary

  reAssignZa <- as.integer(seq(x$L1) * S - 1)
  # x$za <- lapply(x$za, function(x) reAssignZa[x])

  clean <- cleanPadding(x$tokens)
  for (i in seq_along(x$za)) {
    for (j in seq_along(x$za[[i]])) {
      clean[[i]][j]
      x$za[[i]][j]
      set <- as.integer((x$za[[i]][j] - 1) * S + (1:S))
      if (is.na(x$vocabulary$lexicon[clean[[i]][j]])) {
        x$za[[i]][j] <- set[sample.int(S, 1L)]
      } else {
        x$za[[i]][j] <- set[as.integer(x$vocabulary$lexicon[clean[[i]][j]])]
      }
    }
  }

  beta <- matrix(0, x$L1 * x$L2, ncol(x$beta))
  for (i in 1:(x$L1 * x$L2)) {
    beta[i, ] <- x$beta[(i - 1) %/% x$L2 + 1, ]
  }
  for (i in 1:nrow(x$vocabulary)) {
    if (!is.na(x$vocabulary$lexicon[i])) {
      zero <- setdiff(1:x$L2, as.integer(x$vocabulary$lexicon[i]))
      zeros <- c(sapply(
        zero,
        function(j) j + (1:x$L1 - 1) * x$L2,
        USE.NAMES = FALSE
      ))
      beta[zeros, i] <- 0
    }
  }
  x$beta <- beta
  if (x$it > 0) {
    x$phi <- array(
      1 / nrow(x$vocabulary),
      dim = c(nrow(x$vocabulary), x$L2, x$L1)
    )
  }
  if (x$it > 0 & !is.null(x$L2post)) {
    x$L2post <- array(1 / x$L2, dim = c(x$L2, x$L1, length(x$tokens)))
  }

  stopifnot(check_integrity(x))
  x <- fit(x, 0, displayProgress = FALSE)

  as.rJST(x)
}

#' Create a Joint Sentiment/Topic model
#'
#' @description This function initialize a Joint Sentiment/Topic model.
#'
#' @references Lin, C. and He, Y. (2009). [Joint sentiment/topic model for
#' sentiment analysis](https://dl.acm.org/doi/10.1145/1645953.1646003). In *Proceedings
#' of the 18th ACM conference on Information and knowledge management*,
#' 375--384.
#'
#' Lin, C., He, Y., Everson, R. and Ruger, S. (2012). [Weakly Supervised Joint
#' Sentiment-Topic Detection from Text](https://ieeexplore.ieee.org/document/5710933).
#' *IEEE Transactions on Knowledge and Data Engineering*, 24(6), 1134–-1145.
#'
#' @inherit rJST
#' @export
#' @seealso Fitting a model: \code{\link[=fit.sentopicsmodel]{fit()}},
#'   extracting top words: [top_words()]
#' @family topic models
#' @examples
#' \donttest{# creating a JST model
#' JST(ECB_press_conferences_tokens)
#'
#' # estimating a JST model including a lexicon
#' jst <- JST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#' jst <- fit(jst, 100)}
JST <- function(
  x,
  lexicon = NULL,
  S = 3,
  K = 5,
  gamma = 1,
  alpha = 5,
  beta = 0.01,
  gammaCycle = 0,
  alphaCycle = 0
) {
  as.JST(sentopicsmodel(
    x,
    lexicon,
    S,
    K,
    gamma,
    alpha,
    beta,
    gammaCycle,
    alphaCycle,
    reverse = FALSE
  ))
}


#' Create a sentopic model
#'
#' @author Olivier Delmarcelle
#'
#' @description The set of functions [LDA()], [JST()], [rJST()] and
#'   [sentopicsmodel()] are all wrappers to an unified C++ routine and attempt to
#'   replicate their corresponding model. This function is the lower level
#'   wrapper to the C++ routine.
#'
#'
#' @param x tokens object containing the texts. A coercion will be attempted if `x` is not a tokens.
#' @param L1 the number of labels in the first document mixture layer
#' @param L2 the number of labels in the second document mixture layer
#' @param L1prior the first layer hyperparameter of document mixtures
#' @param L2prior the second layer hyperparameter of document mixtures
#' @param beta the hyperparameter of vocabulary distribution
#' @param L1cycle integer specifying the cycle size between two updates of the hyperparameter L1prior
#' @param L2cycle integer specifying the cycle size between two updates of the hyperparameter L2prior
#' @param lexicon a `quanteda` dictionary with positive and negative categories
#' @param reverse indicates on which dimension should `lexicon` apply. When
#'  `reverse=FALSE`, the lexicon is applied on the first layer of the document
#'  mixture (as in a JST model). When `reverse=TRUE`, the lexicon is applied to
#'  the second layer of the document mixture (as in a reverse-JST model).
#'
#' @seealso Fitting a model: \code{\link[=fit.sentopicsmodel]{fit()}},
#'   extracting top words: [top_words()]
#' @family topic models
#'
#' @keywords internal
#'
#' @return An S3 list containing the model parameter and the estimated mixture.
#'   This object corresponds to a Gibbs sampler estimator with zero iterations.
#'   The MCMC can be iterated using the \code{\link[=fit.sentopicsmodel]{fit()}}
#'   function.
#'
#'   - `tokens` is the tokens object used to create the model
#'   - `vocabulary` contains the set of words of the corpus
#'   - `it` tracks the number of Gibbs sampling iterations
#'   - `za` is the list of topic assignment, aligned to the `tokens` object with
#'     padding removed
#'   - `logLikelihood` returns the measured log-likelihood at each iteration,
#'     with a breakdown of the likelihood into hierarchical components as
#'     attribute
#'
#'   The [top_words()] function easily extract the most probables words of each
#'   topic/sentiment.
#'
#' @export
#' @examples
#' LDA(ECB_press_conferences_tokens)
#' rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
sentopicsmodel <- function(
  x,
  lexicon = NULL,
  L1 = 5,
  L2 = 3,
  L1prior = 1,
  L2prior = 5,
  beta = 0.01,
  L1cycle = 0,
  L2cycle = 0,
  reverse = TRUE
) {
  count <- index <- word <- NULL # due to NSE notes in R CMD check

  ## Check arguments
  if (L1 < 1 || L2 < 1) {
    stop(
      "The number of topic and sentiments should be equal or greater than 1."
    )
  }
  if ((L1 * L2) == 1L) {
    stop(
      "Impossible to run a model without having multiple topics or sentiments."
    )
  }

  x <- quanteda::as.tokens(x)

  if (reverse) {
    S <- L2
  } else {
    S <- L1
  }
  if (reverse) {
    Sdim <- "L2"
  } else {
    Sdim <- "L1"
  }

  vocabulary <- makeVocabulary(x, lexicon, S)
  x <- vocabulary$toks
  vocabulary <- vocabulary$vocabulary

  clean <- cleanPadding(x)

  if (length(L1prior) == 1L) {
    L1prior <- matrix(L1prior, L1, 1)
  }
  if (length(L2prior) == 1L) {
    L2prior <- matrix(L2prior, L1 * L2, 1)
  }
  if (length(L2prior) == L2) {
    L2prior <- matrix(rep(L2prior, times = L1), L1 * L2, 1)
  }

  if (length(L1prior) != L1 | length(L2prior) != L1 * L2) {
    stop("Incorrect prior dimension. Please check input alpha or gamma.")
  }

  ## set as matrix incoming vectors
  dim(L1prior) <- c(L1, 1)
  dim(L2prior) <- c(L1 * L2, 1)

  model <- list(
    vocabulary = vocabulary,
    tokens = x,
    za = lapply(lengths(clean), integer),
    L1 = L1,
    L2 = L2,
    L1cycle = L1cycle,
    L2cycle = L2cycle
  )

  cpp_model <- methods::new(cpp_sentopicsmodel, reverse)

  cpp_model$init(
    clean,
    model$za,
    nrow(vocabulary),
    L1,
    L2,
    as.integer(vocabulary$lexicon) - 1,
    L1prior,
    beta,
    L2prior,
    L1cycle,
    L2cycle
  )

  # extract alpha beta and gamma arrays only once
  model$L1prior <- cpp_model$L1prior
  model$beta <- cpp_model$beta
  model$L2prior <- cpp_model$L2prior

  ### TODO: verify that most transfers from C to R and inversely are triggered by pointer

  tmp <- extract_cppModel(cpp_model, core(model))
  model[names(tmp)] <- tmp

  class(model) <- c("sentopicsmodel")
  attr(model, "reverse") <- reverse
  attr(model, "Sdim") <- Sdim
  # if (length(dictVal) > 0) attr(model$vocabulary, "compounds") <- compounds
  reorder_sentopicsmodel(model)
}
