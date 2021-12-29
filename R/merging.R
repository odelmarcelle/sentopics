

mergeTopics <- function(x, merging_list){
  if (length(merging_list) < 2) stop("The aggregation list should include at least two new topics.")
  if (is.null(names(merging_list)) | any(is.na(names(merging_list))) ) names(merging_list) <- paste0("theme", 1:length(merging_list))
  flag <- all(
    length(unlist(merging_list)) == x$K,
    length(unique(unlist(merging_list))) == x$K
    # all(unlist(merging_list) %in% dimnames(x$phi)),
    # all(unlist(merging_list) %in% colnames(x$theta))
  )
  if (!flag) {
    dups <- unname(unlist(merging_list)[duplicated(unlist(merging_list))])
    miss <- setdiff(1:x$K, unique(unlist(merging_list)))

    if (length(dups) > 0) dup_mess <- paste0(" The following indice(s) are duplicated: ", dups) else dup_mess <- ""
    if (length(miss) > 0) miss_mess <- paste0(" The following indice(s) are missing: ", miss) else miss_mess <- ""
    error_message <- paste0("Error in reading the aggregation list. Be sure to include all existing topics and to avoid duplicates.", dup_mess, miss_mess)
    stop(error_message)
    }

  newK <- length(merging_list)

  # x$za[[1]]
  reAssign <- rep(1:newK, times = lengths(merging_list))[order(unlist(merging_list, use.names = FALSE))]
  # reAssign
  # x$za[[1]]
  # reAssign[x$za[[1]]]
  x$za <- lapply(x$za, function(x) reAssign[x])

  alpha <- matrix(0, newK, ncol(x$alpha))
  for (i in 1:newK) {
    alpha[i, ] <- colSums(x$alpha[reAssign == i, , drop = FALSE])
  }
  x$alpha <- alpha
  beta <- matrix(0, newK, ncol(x$beta))
  for (i in 1:newK) {
    beta[i, ] <- colSums(x$beta[reAssign == i, , drop = FALSE])
  }
  x$beta <- beta

  x$theta <- matrix(1/newK, length(x$tokens), newK)
  x$phi <- array(1/nrow(x$vocabulary), dim = c(dim(x$phi)[1:2], newK))

  x$K <- newK

  stopifnot(check_integrity(x))
  attr(x, "labels") <- list(L1 = names(merging_list))
  x <- grow(x, 0, displayProgress = FALSE)

  x
}
