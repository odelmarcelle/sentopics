
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

  as.rJST(sentopicmodel(x, lexicon, K, S, alpha, gamma, beta, alphaCycle, gammaCycle))
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
#' data(ECB_speeches)
#' LDA(ECB_speeches)
#' rJST(ECB_speeches, lexicon = LoughranMcDonald)
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

