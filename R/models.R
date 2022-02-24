
#' @inherit rJST
#' @export
#' @family topic models
LDA <- function(x, K = 5, alpha = 1, beta = 0.01) {
  as.LDA(sentopicmodel(x, lexicon = NULL, L1 = K, L2 = 1, L1prior = alpha, L2prior = 0, beta = beta))
}

#' @inherit sentopicmodel
#'
#' @param K the number of topics
#' @param S the number of sentiments
#' @param alpha the hyperparameter of topic-document distribution
#' @param gamma the hyperparameter of sentiment-document distribution
#' @param alphaCycle integer specifying the cycle size between two updates of the hyperparameter alpha
#' @param gammaCycle integer specifying the cycle size between two updates of the hyperparameter alpha
#'
#' @export
#' @family topic models
rJST <- function(x, lexicon = NULL, K = 5, S = 3,
                 alpha = 1, gamma = 5, beta = 0.01,
                 alphaCycle = 0, gammaCycle = 0) {
  UseMethod("rJST")
}
#' @export
rJST.default <- function(x, lexicon = NULL, K = 5, S = 3,
                 alpha = 1, gamma = 5, beta = 0.01,
                 alphaCycle = 0, gammaCycle = 0) {
  as.rJST(sentopicmodel(x, lexicon, K, S, alpha, gamma, beta, alphaCycle, gammaCycle))
}
#' @export
rJST.LDA <- function(x, lexicon = NULL, K = 5, S = 3,
                     alpha = 1, gamma = 5, beta = 0.01,
                     alphaCycle = 0, gammaCycle = 0) {
  
  if (x$it < 1) stop("Requires an estimated LDA model.")
  x <- as.sentopicmodel(x)
  x$L2 <- as.numeric(S)
  
  if (length(gamma) == 1L) gamma <- matrix(gamma, x$L1 * x$L2, 1)
  if (length(gamma) == x$L2) gamma <- matrix(rep(gamma, times = x$L1), x$L1 * x$L2, 1)
  
  if (length(gamma) != x$L1 * x$L2)
    stop("Incorrect prior dimension. Please check input gamma.")
  
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
      zeros <- sapply(zero, function(j) j + (1:x$L1 - 1) * x$L2, USE.NAMES = FALSE) |> c()
      beta[zeros, i] <- 0
    }
  }
  x$beta <- beta
  if (x$it > 0) x$phi <- array(1/nrow(x$vocabulary), dim = c(nrow(x$vocabulary), x$L2, x$L1))
  if (x$it > 0 & !is.null(x$L2post)) x$L2post <- array(1/x$L2, dim = c(x$L2, x$L1, length(x$tokens)))
  
  stopifnot(check_integrity(x))
  x <- grow(x, 0, displayProgress = FALSE)
  
  as.rJST(x)
}

#' @inherit rJST
#' @export
#' @family topic models
JST <- function(x, lexicon = NULL, S = 3, K = 5,
                 gamma = 1, alpha = 5, beta = 0.01,
                 gammaCycle = 0, alphaCycle = 0) {

  as.JST(sentopicmodel(x, lexicon, S, K, gamma, alpha, beta, gammaCycle, alphaCycle, reversed = FALSE))
}



#' Create a sentopic model
#'
#' @author Olivier Delmarcelle
#'
#' @description The set of functions [LDA()], [JST()], [rJST()] and [sentopicmodel()] are all wrappers to an unified C++
#'   routine and attempt to replicate their corresponding model.
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
#' @param initLDA integer specifying the number of iterations of the LDA burn-in
#' @param lexicon a `quanteda` dictionary with positive and negative categories
#' @param smooth integer specyfing the number of iterations of the smoothed burn-in
#'
#' @return An initialized `sentopicmodel` object inheriting the relevant model class.
#'   This object corresponds to a Gibbs sampler estimation with zero iterations.
#'   The MCMC can be iterated using the [grow()] function.
#'
#'   The estimated distributions of the model are accessible using the dollar-sign operator.
#'   The [topWords()] function easily extract the most probables words of each topic/sentiment.
#'
#' @seealso [grow()], [topWords()]
#' @family topic models
#'
#' @keywords internal
#'
#' @export
#' @examples
#' data(ECB_press_conferences_tokens)
#' LDA(ECB_press_conferences_tokens)
#' rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
sentopicmodel <- function(x, lexicon = NULL, L1 = 5, L2 = 3,
                 L1prior = 1, L2prior = 5, beta = 0.01,
                 L1cycle = 0, L2cycle = 0,
                 initLDA = 0, smooth = 0, reversed = TRUE) {
  count <- index <- word <- NULL # due to NSE notes in R CMD check

  ## Check arguments
  if (L1 < 1 || L2 < 1) stop("The number of topic and sentiments should be equal or greater than 1.")
  if ((L1 * L2) == 1L) stop("Impossible to run a model without having multiple topics or sentiments.")
  if (initLDA != 0 && smooth > initLDA) warning("The smooth period is greater than the LDA initialization... This is perhaps not a good idea.")

  x <- quanteda::as.tokens(x)

  if (reversed) S <- L2 else S <- L1
  if (reversed) Sdim <- "L2" else Sdim <- "L1"

  vocabulary <- makeVocabulary(x, lexicon, S)
  x <- vocabulary$toks
  vocabulary <- vocabulary$vocabulary

  clean <- cleanPadding(x)

  if (length(L1prior) == 1L) L1prior <- matrix(L1prior, L1, 1)
  if (length(L2prior) == 1L) L2prior <- matrix(L2prior, L1 * L2, 1)
  if (length(L2prior) == L2) L2prior <- matrix(rep(L2prior, times = L1), L1 * L2, 1)

  if (length(L1prior) != L1 | length(L2prior) != L1 * L2)
    stop("Incorrect prior dimension. Please check input alpha or gamma.")

  ## set as matrix incoming vectors
  dim(L1prior) <- c(L1, 1)
  dim(L2prior) <- c(L1 * L2, 1)

  model <- list(vocabulary = vocabulary, tokens = x, za = lapply(lengths(clean), integer), L1 = L1, L2 = L2,
                initLDA = initLDA, smooth = smooth, L1cycle = L1cycle, L2cycle = L2cycle)

  cpp_model <- methods::new(cpp_sentopicmodel, reversed)

  cpp_model$init(
    clean, model$za, nrow(vocabulary), L1, L2, as.integer(vocabulary$lexicon) - 1,
    L1prior, beta, L2prior, L1cycle, L2cycle, initLDA, smooth
  )

  # extract alpha beta and gamma arrays only once
  model$L1prior <- cpp_model$L1prior
  model$beta <- cpp_model$beta
  model$L2prior <- cpp_model$L2prior

  ### TODO: verify that most transfers from C to R and inversely are triggered by pointer

  tmp <- extract_cppModel(cpp_model, core(model))
  model[names(tmp)] <- tmp

  class(model) <- c("sentopicmodel")
  attr(model, "reversed") <- reversed
  attr(model, "Sdim") <- Sdim
  # if (length(dictVal) > 0) attr(model$vocabulary, "compounds") <- compounds
  reorder_sentopicmodel(model)
}

