

#' Merge topics into fewer themes
#'
#' @description This operation is especially useful for the analysis of the
#'   model's output, by grouping together topics that share a common theme.
#'
#' @param x a [LDA()] or [rJST()] model.
#' @param merging_list a list where each element is an integer vector containing
#'   the indices of topics to be merged. If named, the list's names become the
#'   label of the aggregated themes.
#'
#' @return A [LDA()] or [rJST()] model with the merged topics.
#'
#' @details Topics are aggregated at the word assignment level. New
#'   document-topic and topic-word probabilities are derived from the aggregated
#'   assignments.
#'
#'   Note that the output of this function does not constitute an estimated
#'   topic model, but merely an aggregation to ease the analysis. It is not
#'   advised to use [grow()] on the merged topic model as it will radically
#'   affect the content and proportions of the new themes.
#'
#' @export
#' @seealso sentopics_labels
#' @examples
#' \donttest{lda <- LDA(ECB_press_conferences_tokens, K = 5)
#' lda <- grow(lda, 100)
#' merging_list <- list(
#'   c(1,5),
#'   2:4
#' )
#' mergeTopics(lda, merging_list)
#' 
#' # also possible with a named list
#' merging_list2 <- list(
#'   mytheme_1 = c(1,5),
#'   mytheme_2 = 2:4
#' )
#' merged <- mergeTopics(lda, merging_list2)
#' sentopics_labels(merged)
#' 
#' # implemented for rJST
#' rjst <- rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#' rjst <- grow(rjst, 100)
#' mergeTopics(rjst, merging_list2)}
mergeTopics <- function(x, merging_list){
  
  if (!inherits(x, c("LDA", "rJST"))) stop("`mergeTopics` is only implemented for LDA and rJST models.")
  if (isTRUE(attr(x, "approx"))) stop("Not possible for approximated models")
  ## TODO: check that merging list is numeric or character
  if (length(merging_list) < 2) stop("The aggregation list should include at least two new topics.")
  if (is.null(names(merging_list)) | any(is.na(names(merging_list))) ) names(merging_list) <- paste0("theme", 1:length(merging_list))
  flag <- all(
    length(unlist(merging_list)) == x$K,
    length(unique(unlist(merging_list))) == x$K
    # all(unlist(merging_list) %in% seq_len(nrow(textmodel_lda$phi))),
    # all(unlist(merging_list) %in% seq_len(ncol(textmodel_lda$theta)))
    # all(unlist(merging_list) %in% dimnames(x$phi)),
    # all(unlist(merging_list) %in% colnames(x$theta))
  )
  if (!flag) {
    dups <- unname(unlist(merging_list)[duplicated(unlist(merging_list))])
    miss <- setdiff(1:x$K, unique(unlist(merging_list)))

    if (length(dups) > 0) dup_mess <- paste0("\n    The following indice(s) are duplicated: ", paste0(dups, collapse = ", ")) else dup_mess <- ""
    if (length(miss) > 0) miss_mess <- paste0("\n    The following indice(s) are missing: ", paste0(miss, collapse = ", ")) else miss_mess <- ""
    error_message <- paste0("The aggregation list is not valid. Make sure to include all existing topics and to avoid duplicates.", dup_mess, miss_mess)
    stop(error_message)
  }
  
  ## not super clean but recompute sentiment for rJST model if it was internal
  if (!is.null(attr(x, "sentiment_not_external"))) sent_flag <- TRUE else
    sent_flag <- FALSE
  if (sent_flag) {
    sentopics_sentiment(x) <- NULL
  }

  newK <- length(merging_list)

  reAssign <- rep(1:newK, times = lengths(merging_list))[order(unlist(merging_list, use.names = FALSE))]
  if (is.null(x$S)) S <- 1 else S <- x$S
  reAssignZa <- as.integer(sapply(reAssign, function(y) (y - 1) * S + 1:S))
  x$za <- lapply(x$za, function(x) reAssignZa[x])

  alpha <- matrix(0, newK, ncol(x$alpha))
  for (i in 1:newK) {
    alpha[i, ] <- colSums(x$alpha[reAssign == i, , drop = FALSE])
  }
  x$alpha <- alpha
  beta <- matrix(0, newK * S, ncol(x$beta))
  for (i in 1:(newK * S)) {
    beta[i, ] <- colSums(x$beta[reAssignZa == i, , drop = FALSE])
  }
  x$beta <- beta
  if (!is.null(x$gamma)) {
    gamma <- matrix(0, newK * S, ncol(x$gamma))
    for (i in 1:(newK * S)) {
      gamma[i, ] <- colSums(x$gamma[reAssignZa == i, , drop = FALSE])
    }
    x$gamma <- gamma
  }

  if (x$it > 0) x$theta <- matrix(1/newK, length(x$tokens), newK)
  if (x$it > 0) x$phi <- array(1/nrow(x$vocabulary), dim = c(nrow(x$vocabulary), S, newK))
  if (x$it > 0 & !is.null(x$pi)) x$pi <- array(1/S, dim = c(S, newK, length(x$tokens)))
  

  x$K <- as.numeric(newK)

  stopifnot(check_integrity(x))
  attr(x, "labels") <- list(L1 = names(merging_list))
  x <- grow(x, 0, displayProgress = FALSE)

  if (sent_flag) {
    sentopics_sentiment(x)
    attr(x, "sentiment_not_external") <- TRUE
  }
    
  x
}
