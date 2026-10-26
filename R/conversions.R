# From other packages -----------------------------------------------------

# Original round-then-repair strategy, with residual choices based on the
# untouched source counts and a unit-donor fallback for stalled row corrections.
round_posterior_counts_heuristic <- function(expected, col_totals, row_totals) {
  counts <- pmax(round(expected), 0)
  difference <- col_totals - colSums(counts)
  for (j in which(difference != 0)) {
    while (difference[j] != 0) {
      possible <- if (difference[j] > 0) {
        rep(TRUE, nrow(counts))
      } else {
        counts[, j] > 0
      }
      residual <- (expected[possible, j] - counts[possible, j]) *
        sign(difference[j])
      i <- which(possible)[which.max(residual)]
      counts[i, j] <- counts[i, j] + sign(difference[j])
      difference[j] <- col_totals[j] - sum(counts[, j])
    }
  }
  if (!is.null(row_totals)) {
    difference <- as.integer(row_totals - rowSums(counts))
    while (any(difference < 0)) {
      threshold <- 1L
      repeat {
        pool <- counts[difference < 0]
        nonzero <- sum(pool > threshold)
        if (nonzero == 0L) {
          threshold <- 0L
          deduct <- 1L
          break
        }
        deduct <- ceiling(abs(min(difference)) / nonzero)
        if (deduct <= threshold) {
          break
        }
        threshold <- threshold + 1L
      }
      counts[difference < 0][counts[difference < 0] > threshold] <-
        counts[difference < 0][counts[difference < 0] > threshold] - deduct
      difference <- as.integer(row_totals - rowSums(counts))
    }
    col_difference <- as.integer(col_totals - colSums(counts))
    if (sum(difference) > 0) {
      if (nrow(counts) == 1L) {
        counts <- counts + matrix(col_difference, nrow = 1)
      } else if (ncol(counts) == 1L) {
        counts <- counts + matrix(difference, ncol = 1)
      } else {
        counts <- counts + stats::r2dtable(1, difference, col_difference)[[1]]
      }
    }
  }
  storage.mode(counts) <- "integer"
  stopifnot(all(colSums(counts) == col_totals), all(counts >= 0))
  if (!is.null(row_totals)) {
    stopifnot(all(rowSums(counts) == row_totals))
  }
  counts
}

# Choose the fast corrected original heuristic or the exact weighted optimizer.
# For the optimizer, source probability error is sum(weights * (counts - expected)^2)
# when weights are inverse squared posterior denominators. Do not rescale the
# source expectations: prior subtraction can make valid expectations negative.
round_posterior_counts <- function(
  expected,
  col_totals,
  row_totals = NULL,
  weights = NULL,
  method = c("heuristic", "optimized")
) {
  method <- match.arg(method)
  stopifnot(
    is.matrix(expected),
    all(is.finite(expected)),
    length(col_totals) == ncol(expected),
    all(is.finite(col_totals)),
    all(col_totals >= 0),
    all(col_totals == as.integer(col_totals))
  )
  if (!is.null(row_totals)) {
    stopifnot(
      length(row_totals) == nrow(expected),
      all(is.finite(row_totals)),
      all(row_totals >= 0),
      all(row_totals == as.integer(row_totals)),
      sum(row_totals) == sum(col_totals)
    )
  }
  expected <- unname(expected)
  if (method == "heuristic") {
    return(round_posterior_counts_heuristic(expected, col_totals, row_totals))
  }
  if (is.null(weights)) {
    weights <- matrix(1, nrow(expected), ncol(expected))
  }
  stopifnot(
    identical(dim(weights), dim(expected)),
    all(is.finite(weights)),
    all(weights > 0)
  )
  weights <- weights / max(weights)
  # Start at the unconstrained cell optima, then solve each column exactly.
  counts <- pmax(round(expected), 0)
  for (j in seq_len(ncol(counts))) {
    left <- col_totals[j] - sum(counts[, j])
    while (left != 0) {
      if (left > 0) {
        cost <- weights[, j] * (2 * (counts[, j] - expected[, j]) + 1)
      } else {
        cost <- weights[, j] * (2 * (expected[, j] - counts[, j]) + 1)
        cost[counts[, j] == 0] <- Inf
      }
      # Prefer earlier rows for tied allocations, later rows for tied removals.
      i <- if (left > 0) {
        which.min(cost)
      } else {
        utils::tail(which(cost == min(cost)), 1L)
      }
      counts[i, j] <- counts[i, j] + sign(left)
      left <- left - sign(left)
    }
  }
  if (!is.null(row_totals) && any(rowSums(counts) != row_totals)) {
    # The column optima have no negative-cost residual cycles. Successive
    # shortest paths preserve optimality while correcting their row totals.
    # A row-to-row edge transfers one token within a column; its cost is the
    # exact change in the quadratic objective. Retain the cheapest such column.
    # https://courses.csail.mit.edu/6.854/17/Scribe/s11-minCostFlow.html
    excess <- rowSums(counts) - row_totals
    nr <- nrow(counts)
    add <- weights * (2 * (counts - expected) + 1)
    remove <- weights * (2 * (expected - counts) + 1)
    remove[counts == 0] <- Inf
    costs <- matrix(Inf, nr, nr)
    columns <- matrix(0L, nr, nr)
    for (i in seq_len(nr)) {
      for (k in seq_len(nr)) {
        if (i == k) {
          next
        }
        j <- which.min(remove[i, ] + add[k, ])
        costs[i, k] <- remove[i, j] + add[k, j]
        columns[i, k] <- j
      }
    }
    while (any(excess > 0)) {
      distance <- rep(Inf, nr)
      distance[excess > 0] <- 0
      error <- rep(0, nr)
      parent <- integer(nr)
      # Bellman-Ford permits negative transfer costs. Track accumulated floating
      # point error so mathematically tied paths do not form predecessor cycles.
      for (iteration in seq_len(nr - 1L)) {
        changed <- FALSE
        for (i in seq_len(nr)) {
          if (!is.finite(distance[i])) {
            next
          }
          candidate <- distance[i] + costs[i, ]
          candidate_error <- error[i] +
            32 * .Machine$double.eps * (abs(distance[i]) + abs(costs[i, ]))
          improved <- which(
            is.finite(candidate) &
              (!is.finite(distance) |
                candidate < distance - candidate_error - error)
          )
          if (length(improved)) {
            distance[improved] <- candidate[improved]
            error[improved] <- candidate_error[improved]
            parent[improved] <- i
            changed <- TRUE
          }
        }
        if (!changed) break
      }
      sinks <- which(excess < 0)
      end <- sinks[which.min(distance[sinks])]
      stopifnot(is.finite(distance[end]))
      sink <- end
      path_length <- 0L
      touched <- matrix(integer(0), ncol = 2)
      while (parent[end] != 0L) {
        path_length <- path_length + 1L
        stopifnot(path_length < nr)
        i <- parent[end]
        j <- columns[i, end]
        counts[i, j] <- counts[i, j] - 1
        counts[end, j] <- counts[end, j] + 1
        touched <- rbind(touched, c(i, j), c(end, j))
        end <- i
      }
      stopifnot(excess[end] > 0)
      excess[end] <- excess[end] - 1
      excess[sink] <- excess[sink] + 1
      touched <- unique(touched)
      add[touched] <- weights[touched] *
        (2 * (counts[touched] - expected[touched]) + 1)
      remove[touched] <- weights[touched] *
        (2 * (expected[touched] - counts[touched]) + 1)
      remove[touched[counts[touched] == 0, , drop = FALSE]] <- Inf
      # Only edges touching changed cells can have a different cheapest column.
      # Rescan a row pair only when its cached minimum has increased.
      for (j in unique(touched[, 2])) {
        affected <- unique(touched[touched[, 2] == j, 1])
        pairs <- unique(rbind(
          as.matrix(expand.grid(affected, seq_len(nr))),
          as.matrix(expand.grid(seq_len(nr), affected))
        ))
        for (pair in seq_len(nrow(pairs))) {
          i <- pairs[pair, 1]
          k <- pairs[pair, 2]
          if (i == k) {
            next
          }
          cost <- remove[i, j] + add[k, j]
          if (columns[i, k] == j && cost > costs[i, k]) {
            best <- which.min(remove[i, ] + add[k, ])
            costs[i, k] <- remove[i, best] + add[k, best]
            columns[i, k] <- best
          } else if (cost < costs[i, k]) {
            costs[i, k] <- cost
            columns[i, k] <- j
          }
        }
      }
    }
  }
  storage.mode(counts) <- "integer"
  stopifnot(all(colSums(counts) == col_totals), all(counts >= 0))
  if (!is.null(row_totals)) {
    stopifnot(all(rowSums(counts) == row_totals))
  }
  counts
}

#' Conversions from other packages to LDA
#'
#' @description These functions converts estimated models from other topic
#'   modeling packages to the format used by \pkg{sentopics}.
#'
#' @param x an estimated topic model from \pkg{stm}, \pkg{topicmodels} or
#'   \pkg{seededlda}.
#' @param list the list containing an estimated model from \pkg{lda}.
#' @param docs for some objects, the documents used to initialize the model.
#' @param count_method how integer counts are reconstructed for \pkg{stm},
#'   variational \pkg{topicmodels}, and \pkg{keyATM} models. `"heuristic"` (the
#'   default) uses the original fast round-then-repair strategy with corrected
#'   residual selection and a unit-donor fallback. `"optimized"` minimizes squared
#'   error in the reconstructed conditional probabilities at greater computational
#'   cost. Both preserve the required document, word and topic totals.
#' @param alpha for \pkg{lda} models, the document-topic mixture hyperparameter.
#'   If missing, the hyperparameter will be set to `50/K`.
#' @param eta for \pkg{lda} models, the topic-word mixture hyperparameter. Other
#'   packages refer to this hyperparameter as *beta*. If missing, the
#'   hyperparameter will be set to `0.01`.
#' @param ... arguments passed to other methods.
#'
#' @details Some models do not store the topic assignment of each word (for
#'   example, estimated through variational inference). For these, the
#'   conversion is limited and some functionalities of \pkg{sentopics} will be
#'   disabled. The list of affected functions is subject to change and currently
#'   includes \code{\link[=fit.sentopicsmodel]{fit()}}, [merge_topics()] and [rJST.LDA()].
#'
#'   Since models from the \pkg{lda} package are simply lists of outputs, the
#'   function `as.LDA_lda()` is not related to the other methods and should be
#'   applied directly on lists containing a model.
#'
#'   With `count_method = "heuristic"`, any remaining count deficits are filled
#'   using [stats::r2dtable()]. This follows R's current random state; use
#'   [set.seed()] before conversion for reproducible results.
#'
#'   With `count_method = "optimized"`, each reconstructed integer count table
#'   minimizes the sum of squared differences between its implied conditional
#'   probabilities and the source probabilities, subject to its required margins.
#'   The implied probabilities include the conversion's model priors. This uses
#'   a deterministic integer minimum-cost flow computation, up to floating-point
#'   precision. Both methods retain the source `theta` and `phi` unchanged.
#'
#' @return A S3 list of class `LDA`, as if it was created and estimated using
#'   [LDA()] and \code{\link[=fit.sentopicsmodel]{fit()}}.
#'
#' @rdname as.LDA
#' @export
#' @examples
#' \donttest{
#' ## stm
#' library("stm")
#' stm <- stm(poliblog5k.docs, poliblog5k.voc, K=25,
#'            prevalence=~rating, data=poliblog5k.meta,
#'            max.em.its=2, init.type="Spectral")
#' as.LDA(stm, docs = poliblog5k.docs)
#' # Opt in to the closest feasible conditional probabilities
#' as.LDA(stm, docs = poliblog5k.docs, count_method = "optimized")
#'
#' ## lda
#' library("lda")
#' data("cora.documents")
#' data("cora.vocab")
#' lda <- lda.collapsed.gibbs.sampler(cora.documents,
#'                                    5, ## Num clusters
#'                                    cora.vocab,
#'                                    100, ## Num iterations
#'                                    0.1,
#'                                    0.1)
#' LDA <- as.LDA_lda(lda, docs = cora.documents, alpha = .1, eta = .1)
#'
#' ## topicmodels
#' data("AssociatedPress", package = "topicmodels")
#' lda <- topicmodels::LDA(AssociatedPress[1:20,],
#'                         control = list(alpha = 0.1), k = 2)
#' LDA <- as.LDA(lda, docs = AssociatedPress[1:20,])
#'
#' ## seededlda
#' library("quanteda")
#' library("seededlda")
#' lda <- textmodel_lda(dfm(ECB_press_conferences_tokens),
#'                      k = 6, max_iter = 100)
#' LDA <- as.LDA(lda)
#'
#' ## keyATM
#' library("keyATM")
#' data(keyATM_data_bills, package = "keyATM")
#' keyATM_docs <- keyATM_read(keyATM_data_bills$doc_dfm)
#' out <- keyATM(docs = keyATM_docs, model = "base",
#'               no_keyword_topics = 5,
#'               keywords = keyATM_data_bills$keywords)
#' LDA <- as.LDA(out, docs = keyATM_docs)
#' }
as.LDA <- function(x, ...) {
  UseMethod("as.LDA")
}
#' @rdname as.LDA
#' @export
as.LDA.STM <- function(
  x,
  docs,
  count_method = c("heuristic", "optimized"),
  ...
) {
  count_method <- match.arg(count_method)
  ## TODO: better identification of priors
  ## mu + sigma for logistic-normal => translate to? using eta instead?
  ## prior for beta? 0?

  K <- x$settings$dim$K

  doc.length <- as.integer(unlist(lapply(docs, function(x) sum(x[2, ]))))
  theta <- x$theta
  theta_prior <- rep(0, ncol(theta))

  zd <- rebuild_L1d_from_posterior(doc.length, theta, theta_prior)
  zd <- t(zd)

  zd <- round_posterior_counts(zd, doc.length, method = count_method)

  phi <- t(exp(x$beta$logbeta[[1]]))
  beta <- t(phi)
  beta[] <- .Machine$double.eps

  ## adjust zw to integer
  zw <- rebuild_zw_from_posterior2(zd, phi, beta)
  weights <- NULL
  if (count_method == "optimized") {
    weights <- matrix(
      1 / pmax(rowSums(zd) + rowSums(beta), 1)^2,
      nrow(zw),
      ncol(zw)
    )
  }
  zw <- round_posterior_counts(
    zw,
    x$settings$dim$wcounts$x,
    rowSums(zd),
    weights,
    method = count_method
  )

  ## Attempt to recreate ZA (not working)
  # za <- rebuild_za_ARRAY(zd, zw)
  # za <- lapply(seq_along(docs), function(d) {
  #   do.call(c,
  #           lapply(seq_along(x$vocab), function(v) {
  #             rep(1:K, times = za[v, , d])
  #           }))
  # })
  #
  # which(x$vocab=="price")
  # t(za[786,,]) |> colSums()
  # zw[, 786]
  #
  # dw <- unname(convert(quanteda::dfm(tokens), "matrix")[, types(tokens)])
  # storage.mode(dw) <- "integer"

  ## tokens
  build_tokens <- get("build_tokens", envir = getNamespace("quanteda"))
  make_docvars <- get("make_docvars", envir = getNamespace("quanteda"))
  tokens <- build_tokens(
    x = lapply(docs, function(doc) rep(doc[1, ], times = doc[2, ])),
    types = x$vocab,
    docvars = make_docvars(length(docs))
  )
  stopifnot(isTRUE(all.equal(
    unname(lengths(cleanPadding(tokens))),
    doc.length
  )))

  vocabulary <- makeVocabulary(tokens, NULL, 1L)

  LDA <- structure(
    list(
      tokens = vocabulary$toks,
      vocabulary = vocabulary$vocabulary,
      K = K,
      alpha = as.matrix(rep(0, K)),
      beta = beta,
      it = x$convergence$its,
      zd = zd,
      zw = zw,
      theta = theta,
      phi = phi,
      logLikelihood = x$convergence$bound
    ),
    class = c("LDA", "sentopicsmodel"),
    reverse = TRUE,
    Sdim = "L2",
    approx = TRUE
  )
  LDA <- as.LDA(reorder_sentopicsmodel(LDA))

  LDA
}


#' @rdname as.LDA
#' @export
as.LDA.LDA_Gibbs <- function(x, docs, ...) {
  K <- x@k
  alpha <- x@alpha
  beta <- x@beta
  beta[] <- x@control@delta

  tokens <- quanteda::as.tokens(quanteda::as.dfm(docs))
  vocabulary <- makeVocabulary(tokens, NULL, 1L)

  za <- split(
    x@z,
    rep(seq_len(length(tokens)), times = lengths(cleanPadding(tokens)))
  )

  LDA <- structure(
    list(
      tokens = vocabulary$toks,
      vocabulary = vocabulary$vocabulary,
      K = K,
      alpha = as.matrix(rep(alpha, K)),
      beta = beta,
      it = x@iter,
      theta = x@gamma,
      phi = exp(t(x@beta)),
      za = za,
      logLikelihood = NULL
    ),
    class = c("LDA", "sentopicsmodel"),
    reverse = TRUE,
    Sdim = "L2"
  )
  LDA <- fit(LDA, 0, displayProgress = FALSE)

  LDA
}

#' @rdname as.LDA
#' @export
as.LDA.LDA_VEM <- function(
  x,
  docs,
  count_method = c("heuristic", "optimized"),
  ...
) {
  count_method <- match.arg(count_method)
  K <- x@k
  alpha <- x@alpha
  alpha <- rep(alpha, K)

  dfm <- quanteda::as.dfm(docs)
  tokens <- quanteda::as.tokens(dfm)
  dfm <- as.matrix(dfm)
  vocabulary <- makeVocabulary(tokens, NULL, 1L)

  doc.length <- lengths(tokens, use.names = FALSE)
  zd <- rebuild_L1d_from_posterior(lengths(tokens), x@gamma, alpha)
  zd <- t(zd)

  zd <- round_posterior_counts(zd, doc.length, method = count_method)

  phi <- t(exp(x@beta))
  beta <- x@beta
  beta[] <- .Machine$double.eps

  ## adjust zw to integer
  zw <- rebuild_zw_from_posterior2(zd, phi, beta)
  weights <- NULL
  if (count_method == "optimized") {
    weights <- matrix(
      1 / pmax(rowSums(zd) + rowSums(beta), 1)^2,
      nrow(zw),
      ncol(zw)
    )
  }
  zw <- round_posterior_counts(
    zw,
    colSums(dfm),
    rowSums(zd),
    weights,
    method = count_method
  )

  LDA <- structure(
    list(
      tokens = vocabulary$toks,
      vocabulary = vocabulary$vocabulary,
      K = K,
      alpha = as.matrix(alpha),
      beta = beta,
      it = x@iter,
      zd = zd,
      zw = zw,
      theta = x@gamma,
      phi = phi,
      logLikelihood = x@logLiks
    ),
    class = c("LDA", "sentopicsmodel"),
    reverse = TRUE,
    Sdim = "L2",
    approx = TRUE
  )
  LDA <- as.LDA(reorder_sentopicsmodel(LDA))

  LDA
}

#' @rdname as.LDA
#' @export
as.LDA.textmodel_lda <- function(x, ...) {
  version <- x$version

  if (!(version >= "1.2.0")) {
    stop(
      "Conversion is not allowed from models created using a version of `seededlda` below 1.2.0"
    )
  }

  labels <- colnames(x$theta)

  beta <- x$phi
  beta[] <- x$beta
  alpha <- x$alpha

  tokens <- as.tokens(x$data)
  vocabulary <- makeVocabulary(tokens, NULL, 1L)

  zd <- rebuild_L1d_from_posterior(
    lengths(cleanPadding(tokens)),
    x$theta,
    alpha
  )
  zd <- t(zd)
  zd <- round(zd)
  storage.mode(zd) <- "integer"

  zw <- t(as.matrix(x$words))
  zw <- round(zw)
  storage.mode(zw) <- "integer"

  LDA <- structure(
    list(
      tokens = vocabulary$toks,
      vocabulary = vocabulary$vocabulary,
      K = x$k,
      alpha = as.matrix(x$alpha),
      beta = beta,
      it = x$last_iter,
      theta = x$theta,
      phi = t(x$phi),
      zd = zd,
      zw = zw,
      logLikelihood = NULL
    ),
    class = c("LDA", "sentopicsmodel"),
    reverse = TRUE,
    Sdim = "L2",
    approx = TRUE,
    labels = list(L1 = colnames(x$theta))
  )
  LDA <- as.LDA(reorder_sentopicsmodel(LDA))

  LDA
}

#' @rdname as.LDA
#' @export
# @usage as.LDA.lda(list, docs, alpha, eta)
as.LDA_lda <- function(list, docs, alpha, eta) {
  zw <- list$topics
  zd <- list$document_sums
  vocab <- colnames(zw)

  K <- nrow(zw)

  build_tokens <- get("build_tokens", envir = getNamespace("quanteda"))
  make_docvars <- get("make_docvars", envir = getNamespace("quanteda"))
  tokens <- build_tokens(
    x = lapply(docs, function(doc) rep(doc[1, ] + 1L, times = doc[2, ])),
    types = colnames(zw),
    docvars = make_docvars(length(docs))
  )

  stopifnot(isTRUE(all.equal(
    unname(lengths(cleanPadding(tokens))),
    colSums(zd)
  )))

  za <- mapply(
    function(doc, assignments) rep(assignments + 1L, times = doc[2, ]),
    docs,
    list$assignments
  )

  vocabulary <- makeVocabulary(tokens, NULL, 1L)

  if (missing(alpha)) {
    alpha <- 50 / K
  }
  if (missing(eta)) {
    eta <- 0.01
  }
  beta <- zw
  beta[] <- eta

  LDA <- structure(
    list(
      tokens = vocabulary$toks,
      vocabulary = vocabulary$vocabulary,
      K = K,
      alpha = as.matrix(rep(alpha, K)),
      beta = beta,
      it = 1,
      theta = t(zd),
      phi = t(zw),
      za = za,
      logLikelihood = NULL
    ),
    class = c("LDA", "sentopicsmodel"),
    reverse = TRUE,
    Sdim = "L2"
  )
  LDA <- fit(LDA, 0, displayProgress = FALSE)

  LDA
}


#' @rdname as.LDA
#' @export
as.LDA.keyATM_output <- function(
  x,
  docs,
  count_method = c("heuristic", "optimized"),
  ...
) {
  count_method <- match.arg(count_method)
  K <- x$keyword_k + x$no_keyword_topics

  labels <- colnames(x$theta)

  alpha <- as.matrix(rep(utils::tail(x$values_iter$alpha_iter$alpha, 1), K))

  tokens <- quanteda::as.tokens(docs$W_raw)
  vocabulary <- makeVocabulary(tokens, NULL, 1L)

  # Align keyATM phi to the new vocabulary order
  reorder <- order(match(x$vocab, vocabulary$vocabulary$word))
  x$vocab <- x$vocab[reorder]
  stopifnot(identical(
    vocabulary$vocabulary$word,
    x$vocab
  ))
  x$phi <- x$phi[, reorder]
  beta <- x$phi
  beta[] <- x$priors$beta
  stopifnot(identical(
    order(match(x$vocab, vocabulary$vocabulary$word)),
    seq(x$V)
  ))

  zd <- rebuild_L1d_from_posterior(
    lengths(cleanPadding(tokens)),
    x$theta,
    alpha
  )
  zd <- unname(t(zd))
  doc.length <- lengths(tokens, use.names = FALSE)

  weights <- NULL
  if (count_method == "optimized") {
    weights <- matrix(
      rep(1 / pmax(doc.length + sum(alpha), 1)^2, each = nrow(zd)),
      nrow(zd)
    )
  }
  zd <- round_posterior_counts(
    zd,
    doc.length,
    x$topic_counts,
    weights,
    method = count_method
  )

  phi <- t(x$phi)

  ## adjust zw to integer
  dfm <- quanteda::dfm(tokens)
  dfm <- dfm[, vocabulary$vocabulary$word] # Ensure correct order
  zw <- rebuild_zw_from_posterior2(zd, phi, beta)
  weights <- NULL
  if (count_method == "optimized") {
    weights <- matrix(
      1 / pmax(rowSums(zd) + rowSums(beta), 1)^2,
      nrow(zw),
      ncol(zw)
    )
  }
  zw <- round_posterior_counts(
    zw,
    quanteda::colSums(dfm),
    rowSums(zd),
    weights,
    method = count_method
  )

  LDA <- structure(
    list(
      tokens = vocabulary$toks,
      vocabulary = vocabulary$vocabulary,
      K = K,
      alpha = alpha,
      beta = beta,
      it = x$options$iterations,
      theta = x$theta,
      phi = phi,
      zd = zd,
      zw = zw,
      logLikelihood = NULL
    ),
    class = c("LDA", "sentopicsmodel"),
    reverse = TRUE,
    Sdim = "L2",
    approx = TRUE,
    labels = list(L1 = colnames(x$theta))
  )
  LDA <- as.LDA(reorder_sentopicsmodel(LDA))

  LDA
}


# To LDAvis ---------------------------------------------------------------

#' Visualize an LDA model using \pkg{LDAvis}
#'
#' This function call \pkg{LDAvis} to create a dynamic visualization of an
#' estimated topic model.
#'
#' @param x an `LDA` model
#' @param ... further arguments passed on to [LDAvis::createJSON()] and
#'   [LDAvis::serVis()].
#'
#' @details The CRAN release of \pkg{LDAvis} does not support UTF-8 characters
#'   and automatically reorder topics. To solve these two issues, please install
#'   the development version of \pkg{LDAvis} from github
#'   (`devtools::install_github("cpsievert/LDAvis")`).
#'
#' @return Nothing, called for its side effects.
#' @export
#'
#' @seealso [plot.sentopicsmodel()]
#'
#' @examples
#' lda <- LDA(ECB_press_conferences_tokens)
#' lda <- fit(lda, 100)
#' LDAvis(lda)
LDAvis <- function(x, ...) {
  mis <- missingSuggets(c("LDAvis", "servr"))
  if (length(mis) > 0) {
    stop(
      "Suggested packages are missing for the LDAvis function.\n",
      "Please install first the following packages: ",
      paste0(mis, collapse = ", "),
      ".\n",
      "Install command: install.packages(",
      paste0("'", mis, "'", collapse = ", "),
      ")"
    )
  }

  stopifnot(inherits(x, c("LDA", "sentopicsmodel")))
  zw <- rebuild_zw(as.sentopicsmodel(x))
  zd <- rebuild_zd(as.sentopicsmodel(x))
  json <- LDAvis::createJSON(
    phi = t(x$phi),
    theta = x$theta,
    doc.length = colSums(zd),
    vocab = x$vocabulary$word,
    term.frequency = colSums(zw),
    reorder.topics = FALSE,
    ...
  )
  LDAvis::serVis(json, encoding = "UTF-8", ...)
}


# To sentopicsmodel --------------------------------------------------------

#' @name sentopics-conversions
#' @rdname sentopics-conversions
#' @keywords internal
#'
#' @title Internal conversions between **sentopics** models.
#'
#' @param x A **sentopics** model
#'
#' @return A **sentopics** model of the relevant class
#' @export
as.sentopicsmodel <- function(x) {
  UseMethod("as.sentopicsmodel")
}
as.sentopicsmodel_defaults <- function(x) {
  ### perhaps create too many objects
  ### especially in multi_chains, recreate things that are stored in base
  if (is.null(x$L2)) {
    x$L2 <- 1
  }
  if (is.null(x$L2prior)) {
    x$L2prior <- x$L1prior
  }
  if (is.null(x$L2post) & x$it > 0) {
    L2post <- array(1, dim = c(1, x$L1, length(x$tokens)))
    dimnames(L2post) <- c(
      list(L2 = levels(x$vocabulary$lexicon)),
      dimnames(x$L1post)[2],
      dimnames(x$L1post)[1]
    )
    x$L2post <- L2post
  }
  if (is.null(x$L1cycle)) {
    x$L1cycle <- 0
  }
  if (is.null(x$L2cycle)) {
    x$L2cycle <- 0
  }
  if (is.null(x$logLikelihood)) {
    logLikelihood <- 0
    attr(logLikelihood, "components") <- list(
      logLikelihoodW = 0,
      logLikelihoodL1 = 0,
      logLikelihoodL2 = 0
    )
    x$logLikelihood <- logLikelihood
  }
  x
}
#' @export
as.sentopicsmodel.LDA <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c(
      "L1",
      "L2",
      "L1prior",
      "L2prior",
      "L1post",
      "L2post",
      "L1cycle",
      "L2cycle",
      "logLikelihoodL1",
      "logLikelihoodL2"
    ),
    c(
      "K",
      "S",
      "alpha",
      "gamma",
      "theta",
      "pi",
      "alphaCycle",
      "gammaCycle",
      "logLikelihoodK",
      "logLikelihoodS"
    )
  )
  if (x$it > 0) {
    if (length(dim(x$phi)) < 3) {
      names <- dimnames(x$phi)
      names[3] <- list(sent = levels(x$vocabulary$lexicon))
      x$phi <- array(
        x$phi,
        dim = c(nrow(x$phi), 1, ncol(x$phi)),
        dimnames = names[c(1, 3, 2)]
      )
    }
  }
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- setdiff(class(x), "LDA")
  as.sentopicsmodel_defaults(x)
}
#' @export
as.sentopicsmodel.rJST <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c(
      "L1",
      "L2",
      "L1prior",
      "L2prior",
      "L1post",
      "L2post",
      "L1cycle",
      "L2cycle",
      "logLikelihoodL1",
      "logLikelihoodL2"
    ),
    c(
      "K",
      "S",
      "alpha",
      "gamma",
      "theta",
      "pi",
      "alphaCycle",
      "gammaCycle",
      "logLikelihoodK",
      "logLikelihoodS"
    )
  )
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- setdiff(class(x), "rJST")
  as.sentopicsmodel_defaults(x)
}
#' @export
as.sentopicsmodel.JST <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c(
      "L1",
      "L2",
      "L1prior",
      "L2prior",
      "L1post",
      "L2post",
      "L1cycle",
      "L2cycle",
      "logLikelihoodL1",
      "logLikelihoodL2"
    ),
    c(
      "S",
      "K",
      "gamma",
      "alpha",
      "pi",
      "theta",
      "gammaCycle",
      "alphaCycle",
      "logLikelihoodS",
      "logLikelihoodK"
    )
  )
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- setdiff(class(x), "JST")

  as.sentopicsmodel_defaults(x)
}
#' @export
as.sentopicsmodel.sentopicsmodel <- function(x) {
  x
}
#' @export
as.sentopicsmodel.multi_chains <- function(x) {
  attr(x, "containedClass") <- "sentopicsmodel"
  x
}
#' @export
as.sentopicsmodel.default <- function(x) {
  stop("Unexpected object passed to as.sentopicsmodel")
}

# From sentopicsmodel ------------------------------------------------------

#' @export
as.LDA.sentopicsmodel <- function(x, ...) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c(
      "K",
      "S",
      "alpha",
      "gamma",
      "theta",
      "pi",
      "alphaCycle",
      "gammaCycle",
      "logLikelihoodK",
      "logLikelihoodS"
    ),
    c(
      "L1",
      "L2",
      "L1prior",
      "L2prior",
      "L1post",
      "L2post",
      "L1cycle",
      "L2cycle",
      "logLikelihoodL1",
      "logLikelihoodL2"
    )
  )
  # correct phi theta pi structure and names
  if (x$it > 0) {
    # dimnames(x$theta) <- list(doc_id = names(x$tokens), topic = paste0("topic", 1:x$K))
    dimnames(x$L1post) <- list(
      doc_id = names(x$tokens),
      topic = create_labels(x, "LDA", flat = FALSE)[["L1"]]
    )
    dimnames(x$phi) <- list(
      word = x$vocabulary$word,
      sent = levels(x$vocabulary$lexicon),
      topic = create_labels(x, "LDA", flat = FALSE)[["L1"]]
    )
    x$phi <- x$phi[, 1, ]
  }
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  # x$phi <- x$phi[,,] ## this drops unneeded sentiment dimension ### Need to account for labels..
  x$S <- x$gamma <- x$pi <- x$alphaCycle <- x$gammaCycle <- x$logLikelihoodS <- NULL
  class(x) <- union("LDA", class(x))
  x
}
#' @export
as.LDA.multi_chains <- function(x, ...) {
  # atrs <- attributes(x)
  # x <- lapply(unclass(x), as.LDA)
  # attributes(x) <- atrs
  attr(x, "containedClass") <- "LDA"
  x
}
#' @export
as.LDA.default <- function(x, ...) {
  stop("Unexpected object passed to as.LDA")
}

#' @rdname sentopics-conversions
#' @export
as.rJST <- function(x) {
  UseMethod("as.rJST")
}
#' @export
as.rJST.sentopicsmodel <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c(
      "K",
      "S",
      "alpha",
      "gamma",
      "theta",
      "pi",
      "alphaCycle",
      "gammaCycle",
      "logLikelihoodK",
      "logLikelihoodS"
    ),
    c(
      "L1",
      "L2",
      "L1prior",
      "L2prior",
      "L1post",
      "L2post",
      "L1cycle",
      "L2cycle",
      "logLikelihoodL1",
      "logLikelihoodL2"
    )
  )
  # correct phi theta pi structure and names
  if (x$it > 0) {
    dimnames(x$L1post) <- list(
      doc_id = names(x$tokens),
      topic = create_labels(x, "rJST", flat = FALSE)[["L1"]]
    )
    dimnames(x$L2post) <- list(
      sent = create_labels(x, "rJST", flat = FALSE)[["L2"]],
      topic = create_labels(x, "rJST", flat = FALSE)[["L1"]],
      doc_id = names(x$tokens)
    )
    dimnames(x$phi) <- list(
      word = x$vocabulary$word,
      sent = create_labels(x, "rJST", flat = FALSE)[["L2"]],
      topic = create_labels(x, "rJST", flat = FALSE)[["L1"]]
    )
  }
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- union("rJST", class(x))
  x
}
#' @export
as.rJST.multi_chains <- function(x) {
  # atrs <- attributes(x)
  # x <- lapply(unclass(x), as.rJST)
  # attributes(x) <- atrs
  attr(x, "containedClass") <- "rJST"
  x
}
#' @export
as.rJST.default <- function(x) {
  stop("Unexpected object passed to as.rJST")
}

#' @rdname sentopics-conversions
#' @export
as.JST <- function(x) {
  UseMethod("as.JST")
}
#' @export
as.JST.sentopicsmodel <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c(
      "S",
      "K",
      "gamma",
      "alpha",
      "pi",
      "theta",
      "gammaCycle",
      "alphaCycle",
      "logLikelihoodS",
      "logLikelihoodK"
    ),
    c(
      "L1",
      "L2",
      "L1prior",
      "L2prior",
      "L1post",
      "L2post",
      "L1cycle",
      "L2cycle",
      "logLikelihoodL1",
      "logLikelihoodL2"
    )
  )
  # correct phi theta pi structure and names
  if (x$it > 0) {
    dimnames(x$L1post) <- list(
      doc_id = names(x$tokens),
      sent = levels(x$vocabulary$lexicon)
    )
    dimnames(x$L2post) <- list(
      topic = create_labels(x, "JST", flat = FALSE)[["L2"]],
      sent = create_labels(x, "JST", flat = FALSE)[["L1"]],
      doc_id = names(x$tokens)
    )
    dimnames(x$phi) <- list(
      word = x$vocabulary$word,
      topic = create_labels(x, "JST", flat = FALSE)[["L2"]],
      sent = create_labels(x, "JST", flat = FALSE)[["L1"]]
    )
  }
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- union("JST", class(x))
  x
}
#' @export
as.JST.multi_chains <- function(x) {
  # atrs <- attributes(x)
  # x <- lapply(unclass(x), as.JST)

  # ## rename base in attributes
  # rename <- stats::setNames(names(atrs$base), names(atrs$base))
  # translate <- stats::setNames(
  #   c("S", "K", "gamma", "alpha", "pi", "theta", "gammaCycle", "alphaCycle", "logLikelihoodS", "logLikelihoodK"),
  #   c("L1", "L2", "L1prior", "L2prior", "L1post", "L2post", "L1cycle", "L2cycle", "logLikelihoodL1", "logLikelihoodL2")
  # )
  # rename <- replace(rename, names(translate), translate)[rename]
  # names(atrs$base) <- rename

  # attributes(x) <- atrs
  attr(x, "containedClass") <- "JST"
  x
}
#' @export
as.JST.default <- function(x) {
  stop("Unexpected object passed to as.JST")
}

# Others ------------------------------------------------------------------

#' @importFrom quanteda as.tokens
#' @export
quanteda::as.tokens

#' Convert back a dfm to a tokens object
#'
#' @param x [quanteda::dfm] to be coerced
#' @param concatenator only used for consistency with the generic
#' @param tokens optionally, the tokens from which the dfm was created.
#'   Providing the initial tokens will ensure that the word order will be
#'   respected in the coerced object.
#' @param ignore_list a character vector of words that should not be removed
#'   from the initial tokens object. Useful to avoid removing some lexicon word
#'   following the usage of [quanteda::dfm_trim()].
#' @param padding if `TRUE`, leaves an empty string where the removed tokens
#'   previously existed. The use of padding is encouraged to improve the
#'   behavior of the coherence metrics (see [coherence()]) that rely on word
#'   positions.
#' @param case_insensitive only used when the `tokens` argument is provided.
#'   Default to `FALSE`. This function removes words in the initial [tokens]
#'   based on the remaining features in the [quanteda::dfm] object. This check is
#'   case-sensitive by default, and can be relaxed by setting this argument to
#'   `TRUE`.
#' @param ... unused
#'
#' @return a quanteda [quanteda::tokens] object.
#' @seealso [quanteda::as.tokens()] [quanteda::dfm()]
#' @export
#' @examples
#' library("quanteda")
#' dfm <- dfm(ECB_press_conferences_tokens, tolower = FALSE)
#' dfm <- dfm_trim(dfm, min_termfreq = 200)
#' as.tokens(dfm)
#' as.tokens(dfm, tokens = ECB_press_conferences_tokens)
#' as.tokens(dfm, tokens = ECB_press_conferences_tokens, padding = FALSE)
as.tokens.dfm <- function(
  x,
  concatenator = NULL,
  tokens = NULL,
  ignore_list = NULL,
  case_insensitive = FALSE,
  padding = TRUE,
  ...
) {
  if (!is.null(tokens)) {
    if (!is.null(ignore_list)) {
      keep <- c(dimnames(x)$features, ignore_list)
    } else {
      keep <- dimnames(x)$features
    }
    ntypes <- length(dimnames(x)$features)
    ## force removal of padding in the dfm object
    if (!padding) {
      keep <- setdiff(keep, "")
    }
    res <- quanteda::as.tokens(
      quanteda::tokens_select(
        tokens,
        keep,
        selection = "keep",
        case_insensitive = case_insensitive,
        padding = padding
      )
    )
    if (length(quanteda::types(res)) + 1 < ntypes) {
      warning(
        "The returned tokens object has less types than the number of column in the dfm input. This could indicate that:\n\t1. The input dfm was lowercase, unlike the provided tokens object.\n\t2. The dfm does not originate from the tokens object.\nTo solve the first problem, consider using the argument `case_insensitive = TRUE` or using the function `quanteda::tokens_tolower()`"
      )
    }
    res
  } else {
    # quanteda::as.tokens(apply(quanteda::dfm_remove(x, ""), 1, function(x) rep(names(x), times = x)))

    if (min(x) < 0) {
      stop("Dfm input should not contain negative values")
    }

    #faster
    # tmp <- quanteda::convert(quanteda::t(x), to = "tripletlist")
    # word <- rep(tmp$document, times = tmp$frequency)
    # ff <- factor(rep(tmp$feature, times = tmp$frequency), rownames(x))
    # res <- as.tokens(split(word, ff))
    # attr(res, "docvars") <- attr(x, "docvars")
    # res

    # R CMD check
    feature <- frequency <- document <- NULL

    vocab <- colnames(x)
    docs <- rownames(x)
    colnames(x) <- seq_len(ncol(x))
    quanteda::docnames(x) <- seq_len(nrow(x))
    tmp <- quanteda::convert(x, to = "tripletlist")
    tmp <- lapply(tmp, as.integer)
    data.table::setDT(tmp)

    missing <- setdiff(seq_len(nrow(x)), unique(tmp$document))

    if (length(missing) > 0) {
      tmp <- rbind(
        tmp,
        data.table::data.table(
          document = missing,
          feature = 1L,
          frequency = 0L
        )
      )
    }

    toks <- tmp[,
      list(toks = list(rep(feature, times = frequency))),
      by = document
    ]
    toks <- toks[order(document)]

    build_tokens <- get("build_tokens", envir = getNamespace("quanteda"))
    make_docvars <- get("make_docvars", envir = getNamespace("quanteda"))

    res <- build_tokens(
      x = toks$toks,
      types = vocab,
      docvars = make_docvars(nrow(toks), docname = docs)
    )

    attr(res, "docvars") <- attr(x, "docvars")
    res
  }
}
