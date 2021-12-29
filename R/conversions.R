

# To sentopicmodel --------------------------------------------------------

#' @name sentopics-conversions
#' @rdname as.cDLA
#'
#' @title Title
#'
#' @param x ...
#'
#' @return a relevant `sentopicmodel` object
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

  # TODO: ?
  # # correct phi theta pi structure and names
  # if (x$it > 0) {
  #   dimnames(x$theta) <- list(doc_id = names(x$tokens), topic = paste0("topic", 1:x$T))
  #   dimnames(x$pi) <- list(sent = paste0("sent", 1:x$S), topic = paste0("topic", 1:x$T), doc_id = names(x$tokens))
  #   dimnames(x$phi) <- list(word = x$vocabulary$word, sent = paste0("sent", 1:x$S), topic = paste0("topic", 1:x$T))
  # }

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


#' @rdname as.cDLA
#' @param x ...
#'
#' @export
as.LDA <- function(x) {
  UseMethod("as.LDA")
}
#' @export
as.LDA.sentopicmodel <- function(x) {
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
  }
  rename <- replace(rename, names(translate), translate)[rename]
  names(x) <- rename
  # x$phi <- x$phi[,,] ## this drops un-needed sentiment dimension ### Need to account for labels..
  x$S <- x$gamma <- x$pi <- x$initLDA <- x$smooth <- x$initType <- x$alphaCycle <- x$gammaCycle <- x$logLikelihoodS <- NULL
  class(x) <- union("LDA", class(x))
  x
}
#' @export
as.LDA.multiChains <- function(x) {
  # atrs <- attributes(x)
  # x <- lapply(unclass(x), as.LDA)
  # attributes(x) <- atrs
  attr(x, "containedClass") <- "LDA"
  x
}
#' @export
as.LDA.default <- function(x) {
  stop("Unexpected object passed to as.LDA")
}

#' @rdname as.cDLA
#' @param x ...
#'
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

#' @rdname as.cDLA
#' @param x ...
#'
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
#' @param x dfm to be coerced
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
#' @param ... unused
#'
#' @return a quanteda [quanteda::tokens] object.
#' @export
as.tokens.dfm <- function(x, concatenator = NULL, tokens = NULL, ignore_list = NULL, padding = TRUE, ...) {
  if (!is.null(tokens)) {
    if (!is.null(ignore_list)) keep <- quanteda::types(tokens) %in% c(dimnames(x)$features, ignore_list) else
      keep <- quanteda::types(tokens) %in% dimnames(x)$features
    quanteda::as.tokens(
      quanteda::tokens_select(tokens,
                              quanteda::types(tokens)[keep],
                              selection = "keep",
                              case_insensitive = FALSE,
                              padding = padding
      )
    )
  } else {
    # quanteda::as.tokens(apply(quanteda::dfm_remove(x, ""), 1, function(x) rep(names(x), times = x)))

    #faster
    tmp <- quanteda::convert(quanteda::t(x), to = "tripletlist")
    word <- rep(tmp$document, times = tmp$frequency)
    ff <- factor(rep(tmp$feature, times = tmp$frequency), rownames(x))
    res <- as.tokens(split(word, ff))
    attr(res, "docvars") <- attr(x, "docvars")
    res
  }
}
