
# From other packages -----------------------------------------------------

#' Conversions from other packages to LDA
#'
#' @description These functions converts estimated models from other topic
#'   modeling packages to the format used by \pkg{sentopics}.
#'
#' @param x an estimated topic model from \pkg{stm}, \pkg{topicmodels} or
#'   \pkg{seededlda}.
#' @param list the list containing an estimated model from \pkg{lda}.
#' @param docs for some objects, the documents used to initialize the model.
#' @param alpha for \pkg{lda} models, the document-topic mixture hyperparameter.
#'   If missing, the hyperparameter will be set to `50/K`.
#' @param eta for \pkg{lda} models, the topic-word mixture hyperparameter. Other
#'   packages refer to this hypeparameter as *beta*. If missing, the
#'   hyperparameter will be set to `0.01`.
#' @param ... arguments passed to other methods.
#'
#' @details Some models do not store the topic assignment of each word (for
#'   example, estimated through variational inference). For these, the
#'   conversion is limited and some functionalities of \pkg{sentopics} will be
#'   disabled. The list of affected functions is subject to change and currently
#'   includes \code{\link[=fit.sentopicmodel]{fit()}}, [mergeTopics()] and [rJST.LDA()].
#'
#'   Since models from the \pkg{lda} package are simply lists of outputs, the
#'   function `as.LDA_lda()` is not related to the other methods and should be
#'   applied directly on lists containing a model.
#'
#' @return A S3 list of class `LDA`, as if it was created and estimated using
#'   [LDA()] and \code{\link[=fit.sentopicmodel]{fit()}}.
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
as.LDA.STM <- function(x, docs, ...) {
  ## TODO: better identification of priors
  ## mu + sigma for logistic-normal => translate to? using eta instead?
  ## prior for beta? 0?
  
  K <- x$settings$dim$K
  
  doc.length <- as.integer(unlist(lapply(docs, function(x) sum(x[2, ]))))
  theta <- x$theta
  theta_prior <- rep(0, ncol(theta))
  
  zd <- rebuild_L1d_from_posterior(doc.length, theta, theta_prior)
  zd <- t(zd)
  
  ## adjust zd to integer
  diff <- doc.length - as.integer(colSums(round(zd)))
  for (i in which(diff != 0)) {
    while (diff[i] != 0L) {
      possible <- zd[, i] > (-diff[i] - .5)
      idx <- which.min( (zd[possible, i] - round(zd[possible, i])*diff[i] ))
      zd[which(possible)[idx], i] <- zd[which(possible)[idx], i] + 1*sign(diff[i])
      diff[i] <- as.integer(doc.length[i] - sum(round(zd[, i])))
    }
  }
  zd <- round(zd)
  storage.mode(zd) <- "integer"
  stopifnot(isTRUE(all.equal(colSums(zd), doc.length)))
  
  
  phi <- t(exp(x$beta$logbeta[[1]]))
  beta <- t(phi)
  beta[] <- .Machine$double.eps
  
  ## adjust zw to integer
  zw <- rebuild_zw_from_posterior2(zd, phi, beta)
  diff <- x$settings$dim$wcounts$x - as.integer(colSums(round(zw)))
  for (i in which(diff != 0)) {
    while (diff[i] != 0L) {
      possible <- (zw[, i] > (-diff[i] - .5))
      idx <- which.min( (zw[possible, i] - round(zw[possible, i])*diff[i] ))
      zw[which(possible)[idx], i] <- zw[which(possible)[idx], i] + 1*sign(diff[i])
      diff[i] <- as.integer(x$settings$dim$wcounts$x[i] - sum(round(zw[, i])))
    }
  }
  zw <- round(zw)
  diff2 <- as.integer(rowSums(zd) - rowSums(round(zw)))
  ## quickly adjust second dimension
  while (any(diff2 < 0)) {
    count <- 1L;
    while (TRUE) {
      non_zero <- length(zw[diff2 < 0][zw[diff2 < 0] > count])
      deduct <- ceiling(abs(min(diff2))/non_zero)
      if (deduct <= count) break
      else count <- count + 1
    }
    zw[diff2 < 0][zw[diff2 < 0] > count] <- zw[diff2 < 0][zw[diff2 < 0] > count] - deduct 
    diff2 <- as.integer(rowSums(zd) - rowSums(round(zw)))
  }
  diff <- x$settings$dim$wcounts$x - as.integer(colSums(round(zw)))
  zw <- zw + stats::r2dtable(1, diff2, diff)[[1]]
  storage.mode(zw) <- "integer"
  stopifnot(isTRUE(all.equal(colSums(zw), x$settings$dim$wcounts$x)))
  stopifnot(isTRUE(all.equal(rowSums(zw), rowSums(zd))))
  
  
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
    docvars = make_docvars(length(docs)))
  stopifnot(isTRUE(all.equal(unname(lengths(cleanPadding(tokens))),
                             doc.length)))

  vocabulary <- makeVocabulary(tokens, NULL, 1L)

  LDA <- structure(list(
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
  ), class = c("LDA", "sentopicmodel"), reversed = TRUE, Sdim = "L2",
  approx = TRUE)
  LDA <- as.LDA(reorder_sentopicmodel(LDA))

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
  
  za <- split(x@z, rep(seq_len(length(tokens)),
                       times = lengths(cleanPadding(tokens))))
  
  LDA <- structure(list(
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
  ), class = c("LDA", "sentopicmodel"), reversed = TRUE, Sdim = "L2")
  LDA <- fit(LDA, 0, displayProgress = FALSE)
  
  LDA
}

#' @rdname as.LDA
#' @export
as.LDA.LDA_VEM <- function(x, docs, ...) {
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
  
  ## adjust zd to integer
  diff <- doc.length - as.integer(colSums(round(zd)))
  for (i in which(diff != 0)) {
    while (diff[i] != 0L) {
      possible <- zd[, i] > (-diff[i] - .5)
      idx <- which.min( (zd[possible, i] - round(zd[possible, i])*diff[i] ))
      zd[which(possible)[idx], i] <- zd[which(possible)[idx], i] + 1*sign(diff[i])
      diff[i] <- as.integer(doc.length[i] - sum(round(zd[, i])))
    }
  }
  zd <- round(zd)
  storage.mode(zd) <- "integer"
  stopifnot(isTRUE(all.equal(colSums(zd), doc.length)))
  
  phi <- t(exp(x@beta))
  beta <- x@beta
  beta[] <- .Machine$double.eps
  
  ## adjust zw to integer
  zw <- rebuild_zw_from_posterior2(zd, phi, beta)
  diff <- as.integer(colSums(dfm) - colSums(round(zw)))
  for (i in which(diff != 0)) {
    while (diff[i] != 0L) {
      possible <- (zw[, i] > (-diff[i] - .5))
      idx <- which.min( (zw[possible, i] - round(zw[possible, i])*diff[i] ))
      zw[which(possible)[idx], i] <- zw[which(possible)[idx], i] + 1*sign(diff[i])
      diff[i] <- as.integer(sum(dfm[, i]) - sum(round(zw[, i])))
    }
  }
  zw <- round(zw)
  diff2 <- as.integer(rowSums(zd) - rowSums(round(zw)))
  ## quickly adjust second dimension
  while (any(diff2 < 0)) {
    count <- 1L;
    while (TRUE) {
      non_zero <- length(zw[diff2 < 0][zw[diff2 < 0] > count])
      deduct <- ceiling(abs(min(diff2))/non_zero)
      if (deduct <= count) break
      else count <- count + 1
    }
    zw[diff2 < 0][zw[diff2 < 0] > count] <- zw[diff2 < 0][zw[diff2 < 0] > count] - deduct 
    diff2 <- as.integer(rowSums(zd) - rowSums(round(zw)))
  }
  diff <- as.integer(colSums(dfm)) - as.integer(colSums(round(zw)))
  zw <- zw + stats::r2dtable(1, diff2, diff)[[1]]
  storage.mode(zw) <- "integer"
  stopifnot(isTRUE(all.equal(colSums(zw), unname(colSums(dfm)))))
  stopifnot(isTRUE(all.equal(rowSums(zw), rowSums(zd))))
  
  LDA <- structure(list(
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
  ), class = c("LDA", "sentopicmodel"), reversed = TRUE, Sdim = "L2",
  approx = TRUE)
  LDA <- as.LDA(reorder_sentopicmodel(LDA))
  
  LDA
}

#' @rdname as.LDA
#' @export
as.LDA.textmodel_lda <- function(x, ...) {
  
  labels <- colnames(x$theta)
  
  beta <- x$phi
  beta[] <- x$beta
  alpha <- rep(x$alpha, x$k)
  
  tokens <- as.tokens(x$data)
  vocabulary <- makeVocabulary(tokens, NULL, 1L)
  
  zd <- rebuild_L1d_from_posterior(lengths(cleanPadding(tokens)), x$theta, alpha)
  zd <- t(zd)
  zd <- round(zd)
  storage.mode(zd) <- "integer"
  
  zw <- t(as.matrix(x$words))
  zw <- round(zw)
  storage.mode(zw) <- "integer"
  
  LDA <- structure(list(
    tokens = vocabulary$toks,
    vocabulary = vocabulary$vocabulary,
    K = x$k,
    alpha = as.matrix(rep(x$alpha, x$k)),
    beta = beta,
    it = x$last_iter,
    theta = x$theta,
    phi = t(x$phi),
    zd = zd,
    zw = zw,
    logLikelihood = NULL
  ), class = c("LDA", "sentopicmodel"), reversed = TRUE, Sdim = "L2",
  approx = TRUE, labels = list(L1 = colnames(x$theta)))
  LDA <- as.LDA(reorder_sentopicmodel(LDA))
  
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
    docvars = make_docvars(length(docs)))
  
  stopifnot(isTRUE(all.equal(unname(lengths(cleanPadding(tokens))),
                             colSums(zd))))
  
  za <- mapply(function(doc, assignments) rep(assignments + 1L, times = doc[2, ]),
               docs, list$assignments)
  
  vocabulary <- makeVocabulary(tokens, NULL, 1L)
  
  if (missing(alpha)) alpha <- 50/K
  if (missing(eta)) eta <- 0.01
  beta <- zw
  beta[] <- eta
  
  LDA <- structure(list(
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
  ), class = c("LDA", "sentopicmodel"), reversed = TRUE, Sdim = "L2")
  LDA <- fit(LDA, 0, displayProgress = FALSE)
  
  LDA
}



#' @rdname as.LDA
#' @export
as.LDA.keyATM_output <- function(x, docs, ...) {
  K = x$keyword_k + x$no_keyword_topics
  
  
  labels <- colnames(x$theta)
  
  beta <- x$phi
  beta[] <- x$priors$beta
  
  alpha <- as.matrix(rep(utils::tail(x$values_iter$alpha_iter$alpha, 1), K))
  
  
  tokens <- quanteda::as.tokens(docs$W_raw)
  vocabulary <- makeVocabulary(tokens, NULL, 1L)
  
  zd <- rebuild_L1d_from_posterior(lengths(cleanPadding(tokens)), x$theta, alpha)
  zd <- unname(t(zd))
  doc.length <- lengths(tokens, use.names = FALSE)
  
  ## adjust zd to integer
  diff <- doc.length - as.integer(colSums(round(zd)))
  for (i in which(diff != 0)) {
    while (diff[i] != 0L) {
      possible <- zd[, i] > (-diff[i] - .5)
      idx <- which.min( (zd[possible, i] - round(zd[possible, i])*diff[i] ))
      zd[which(possible)[idx], i] <- zd[which(possible)[idx], i] + 1*sign(diff[i])
      diff[i] <- as.integer(doc.length[i] - sum(round(zd[, i])))
    }
  }
  zd <- round(zd)
  diff2 <- as.integer(x$topic_counts - rowSums(zd))
  ## quickly adjust second dimension
  while (any(diff2 < 0)) {
    count <- 1L;
    while (TRUE) {
      non_zero <- length(zd[diff2 < 0][zd[diff2 < 0] > count])
      deduct <- ceiling(abs(min(diff2))/non_zero)
      if (deduct <= count) break
      else count <- count + 1
    }
    zd[diff2 < 0][zd[diff2 < 0] > count] <- zd[diff2 < 0][zd[diff2 < 0] > count] - deduct 
    diff2 <- as.integer(x$topic_counts - rowSums(zd))
  }
  diff <- doc.length - as.integer(colSums(round(zd)))
  zd <- zd + stats::r2dtable(1, diff2, diff)[[1]]
  storage.mode(zd) <- "integer"
  stopifnot(isTRUE(all.equal(colSums(zd), doc.length)))
  stopifnot(isTRUE(all.equal(rowSums(zd), x$topic_counts)))
  
  
  
  phi <- t(x$phi)
  
  ## adjust zw to integer
  dfm <- quanteda::dfm(tokens)
  zw <- rebuild_zw_from_posterior2(zd, phi, beta)
  diff <- as.integer(quanteda::colSums(dfm) - colSums(round(zw)))
  for (i in which(diff != 0)) {
    while (diff[i] != 0L) {
      possible <- (zw[, i] > (-diff[i] - .5))
      idx <- which.min( (zw[possible, i] - round(zw[possible, i])*diff[i] ))
      zw[which(possible)[idx], i] <- zw[which(possible)[idx], i] + 1*sign(diff[i])
      diff[i] <- as.integer(sum(dfm[, i]) - sum(round(zw[, i])))
    }
  }
  zw <- round(zw)
  diff2 <- as.integer(rowSums(zd) - rowSums(round(zw)))
  ## quickly adjust second dimension
  while (any(diff2 < 0)) {
    count <- 1L;
    while (TRUE) {
      non_zero <- length(zw[diff2 < 0][zw[diff2 < 0] > count])
      deduct <- ceiling(abs(min(diff2))/non_zero)
      if (deduct <= count) break
      else count <- count + 1
    }
    zw[diff2 < 0][zw[diff2 < 0] > count] <- zw[diff2 < 0][zw[diff2 < 0] > count] - deduct 
    diff2 <- as.integer(rowSums(zd) - rowSums(round(zw)))
  }
  diff <- as.integer(quanteda::colSums(dfm)) - as.integer(colSums(round(zw)))
  zw <- zw + stats::r2dtable(1, diff2, diff)[[1]]
  storage.mode(zw) <- "integer"
  stopifnot(isTRUE(all.equal(colSums(zw), unname(quanteda::colSums(dfm)))))
  stopifnot(isTRUE(all.equal(rowSums(zw), rowSums(zd))))
  
  LDA <- structure(list(
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
  ), class = c("LDA", "sentopicmodel"), reversed = TRUE, Sdim = "L2",
  approx = TRUE, labels = list(L1 = colnames(x$theta)))
  LDA <- as.LDA(reorder_sentopicmodel(LDA))
  
  LDA
}



# To LDAvis ---------------------------------------------------------------


#' Visualize a LDA model using \pkg{LDAvis}
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
#' @seealso [plot.sentopicmodel()]
#'
#' @examples
#' lda <- LDA(ECB_press_conferences_tokens)
#' lda <- fit(lda, 100)
#' LDAvis(lda)
LDAvis <- function(x, ...) {
  mis <- missingSuggets(c("LDAvis", "servr"))
  if (length(mis) > 0) stop("Suggested packages are missing for the LDAvis function.\n",
                            "Please install first the following packages: ",
                            paste0(mis, collapse = ", "),".\n",
                            "Install command: install.packages(",
                            paste0("'", mis, "'", collapse = ", "),")" )
  
  stopifnot(inherits(x, c("LDA", "sentopicmodel")))
  zw <- rebuild_zw(as.sentopicmodel(x))
  zd <- rebuild_zd(as.sentopicmodel(x))
  json <- LDAvis::createJSON(
    phi = t(x$phi),
    theta = x$theta,
    doc.length = colSums(zd),
    vocab = x$vocabulary$word,
    term.frequency = colSums(zw),
    reorder.topics = FALSE,
    ...)
  LDAvis::serVis(json, encoding = "UTF-8", ...)
}


# To sentopicmodel --------------------------------------------------------

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
as.sentopicmodel <- function(x) {
  UseMethod("as.sentopicmodel")
}
as.sentopicmodel_defaults <- function(x) {
  ### perhaps create too many objects
  ### especially in multiChains, recreate things that are stored in base
  if (is.null(x$L2)) x$L2 <- 1
  if (is.null(x$L2prior)) x$L2prior <- x$L1prior
  if (is.null(x$L2post) & x$it > 0) {
    L2post <- array(1, dim = c(1, x$L1, length(x$tokens)))
    dimnames(L2post) = c(list(L2 = levels(x$vocabulary$lexicon)),
                              dimnames(x$L1post)[2],
                              dimnames(x$L1post)[1])
    x$L2post <- L2post
  }
  if (is.null(x$initLDA)) x$initLDA <- 0
  if (is.null(x$smooth)) x$smooth <- 0
  if (is.null(x$initType)) x$initType <- 1
  if (is.null(x$L1cycle)) x$L1cycle <- 0
  if (is.null(x$L2cycle)) x$L2cycle <- 0
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
as.sentopicmodel.LDA <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c("L1", "L2", "L1prior", "L2prior", "L1post", "L2post", "L1cycle", "L2cycle", "logLikelihoodL1", "logLikelihoodL2"),
    c("K", "S", "alpha", "gamma", "theta", "pi", "alphaCycle", "gammaCycle", "logLikelihoodK", "logLikelihoodS")
  )
  if (x$it > 0) {
    if (length(dim(x$phi)) < 3) {
      names <- dimnames(x$phi)
      names[3] <- list(sent = levels(x$vocabulary$lexicon))
      x$phi <- array(x$phi,
                     dim = c(nrow(x$phi), 1, ncol(x$phi)),
                     dimnames = names[c(1,3,2)])
    }
  }
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- setdiff(class(x), "LDA")
  as.sentopicmodel_defaults(x)
}
#' @export
as.sentopicmodel.rJST <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c("L1", "L2", "L1prior", "L2prior", "L1post", "L2post", "L1cycle", "L2cycle", "logLikelihoodL1", "logLikelihoodL2"),
    c("K", "S", "alpha", "gamma", "theta", "pi", "alphaCycle", "gammaCycle", "logLikelihoodK", "logLikelihoodS")
  )
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- setdiff(class(x), "rJST")
  as.sentopicmodel_defaults(x)
}
#' @export
as.sentopicmodel.JST <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c("L1", "L2", "L1prior", "L2prior", "L1post", "L2post", "L1cycle", "L2cycle", "logLikelihoodL1", "logLikelihoodL2"),
    c("S", "K", "gamma", "alpha", "pi", "theta", "gammaCycle", "alphaCycle", "logLikelihoodS", "logLikelihoodK")
  )
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- setdiff(class(x), "JST")

  as.sentopicmodel_defaults(x)
}
#' @export
as.sentopicmodel.sentopicmodel <- function(x) {
  x
}
#' @export
as.sentopicmodel.multiChains <- function(x) {
  attr(x, "containedClass") <- "sentopicmodel"
  x
}
#' @export
as.sentopicmodel.default <- function(x) {
  stop("Unexpected object passed to as.sentopicmodel")
}

# From sentopicmodel ------------------------------------------------------

#' @export
as.LDA.sentopicmodel <- function(x, ...) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c("K", "S", "alpha", "gamma", "theta", "pi", "alphaCycle", "gammaCycle", "logLikelihoodK", "logLikelihoodS"),
    c("L1", "L2", "L1prior", "L2prior", "L1post", "L2post", "L1cycle", "L2cycle", "logLikelihoodL1", "logLikelihoodL2")
  )
  # correct phi theta pi structure and names
  if (x$it > 0) {
    # dimnames(x$theta) <- list(doc_id = names(x$tokens), topic = paste0("topic", 1:x$K))
    dimnames(x$L1post) <- list(doc_id = names(x$tokens), topic = create_labels(x, "LDA", flat = FALSE)[["L1"]])
    dimnames(x$phi) <- list(word = x$vocabulary$word, sent = levels(x$vocabulary$lexicon), topic = create_labels(x, "LDA", flat = FALSE)[["L1"]])
    x$phi <- x$phi[ ,1, ]
  }
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  # x$phi <- x$phi[,,] ## this drops un-needed sentiment dimension ### Need to account for labels..
  x$S <- x$gamma <- x$pi <- x$initLDA <- x$smooth <- x$initType <- x$alphaCycle <- x$gammaCycle <- x$logLikelihoodS <- NULL
  class(x) <- union("LDA", class(x))
  x
}
#' @export
as.LDA.multiChains <- function(x, ...) {
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
as.rJST.sentopicmodel <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c("K", "S", "alpha", "gamma", "theta", "pi", "alphaCycle", "gammaCycle", "logLikelihoodK", "logLikelihoodS"),
    c("L1", "L2", "L1prior", "L2prior", "L1post", "L2post", "L1cycle", "L2cycle", "logLikelihoodL1", "logLikelihoodL2")
  )
  # correct phi theta pi structure and names
  if (x$it > 0) {
    dimnames(x$L1post) <- list(doc_id = names(x$tokens), topic = create_labels(x, "rJST", flat = FALSE)[["L1"]])
    dimnames(x$L2post) <- list(sent = create_labels(x, "rJST", flat = FALSE)[["L2"]], topic = create_labels(x, "rJST", flat = FALSE)[["L1"]], doc_id = names(x$tokens))
    dimnames(x$phi) <- list(word = x$vocabulary$word, sent = create_labels(x, "rJST", flat = FALSE)[["L2"]], topic = create_labels(x, "rJST", flat = FALSE)[["L1"]])
  }
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- union("rJST", class(x))
  x
}
#' @export
as.rJST.multiChains <- function(x) {
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
as.JST.sentopicmodel <- function(x) {
  rename <- stats::setNames(names(x), names(x))
  translate <- stats::setNames(
    c("S", "K", "gamma", "alpha", "pi", "theta", "gammaCycle", "alphaCycle", "logLikelihoodS", "logLikelihoodK"),
    c("L1", "L2", "L1prior", "L2prior", "L1post", "L2post", "L1cycle", "L2cycle", "logLikelihoodL1", "logLikelihoodL2")
  )
  # correct phi theta pi structure and names
  if (x$it > 0) {
    dimnames(x$L1post) <- list(doc_id = names(x$tokens), sent = levels(x$vocabulary$lexicon))
    dimnames(x$L2post) <- list(topic = create_labels(x, "JST", flat = FALSE)[["L2"]], sent = create_labels(x, "JST", flat = FALSE)[["L1"]], doc_id = names(x$tokens))
    dimnames(x$phi) <- list(word = x$vocabulary$word, topic = create_labels(x, "JST", flat = FALSE)[["L2"]], sent = create_labels(x, "JST", flat = FALSE)[["L1"]])
  }
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  class(x) <- union("JST", class(x))
  x
}
#' @export
as.JST.multiChains <- function(x) {
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
#'   based on the remaining features in the [dfm] object. This check is
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
as.tokens.dfm <- function(x, concatenator = NULL, tokens = NULL, ignore_list = NULL, case_insensitive = FALSE, padding = TRUE, ...) {
  if (!is.null(tokens)) {
    if (!is.null(ignore_list)) keep <- c(dimnames(x)$features, ignore_list) else
      keep <- dimnames(x)$features
    ntypes <- length(dimnames(x)$features)
    ## force removal of padding in the dfm object
    if (!padding) keep <- setdiff(keep, "")
    res <- quanteda::as.tokens(
      quanteda::tokens_select(tokens,
                              keep,
                              selection = "keep",
                              case_insensitive = case_insensitive,
                              padding = padding
      )
    )
    if (length(quanteda::types(res)) + 1 < ntypes) warning("The returned tokens object has less types than the number of column in the dfm input. This could indicate that:\n\t1. The input dfm was lowercase, unlike the provided tokens object.\n\t2. The dfm does not originate from the tokens object.\nTo solve the first problem, consider using the argument `case_insensitive = TRUE` or using the function `quanteda::tokens_tolower()`")
    res
  } else {
    # quanteda::as.tokens(apply(quanteda::dfm_remove(x, ""), 1, function(x) rep(names(x), times = x)))
    
    if (min(x) < 0) stop("Dfm input should not contain negative values")
    
    
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
        ))
    }
    
    toks <- tmp[, list(toks = list(rep(feature, times = frequency))), by = document]
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
