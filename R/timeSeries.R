#' Internal sentiment
#'
#' @family sentopics helpers
#'
#' @author Olivier Delmarcelle
#'
#' @description Compute, extract or replace the internal sentiment of a
#'   `sentopicmodel`. The internal sentiment is used to create time series using
#'   the functions [sentiment_series()] or [sentiment_topics()]. If the input
#'   model contains a sentiment layer, sentiment can be computed directly from
#'   the output of the model. Otherwise, sentiment obtained externally should be
#'   added for each document.
#'
#' @param x a `sentopicmodel` created from the [LDA()], [JST()], [rJST()] or
#'   [sentopicmodel()] function
#' @param method the method used to compute sentiment, see "Methods" below.
#'   Ignored if an internal sentiment is already stored, unless `override` is
#'   `TRUE`.
#' @param override by default, the function computes sentiment only if no
#'   internal sentiment is already stored within the `sentopicmodel` object.
#'   This avoid, for example, erasing externally provided sentiment. Set to
#'   `TRUE` to force computation of new sentiment values. Only useful for models
#'   with a sentiment layer.
#' @param quiet if `FALSE`, print a message when internal sentiment is found.
#' @param include_docvars if `TRUE` the function will return all docvars stored
#'   in the internal `tokens` object of the model
#'
#' @return A data.frame with the stored sentiment per document.
#'
#' @details The computed sentiment varies depending on the model. For [LDA],
#'   sentiment computation is not possible.
#'
#'   For [JST], the sentiment is computed on a per-document basis according to
#'   the document-level sentiment mixtures.
#'
#'   For a [rJST] model, a sentiment is computed for each topic, resulting in
#'   `K` sentiment values per document. In that case, the `.sentiment` column is
#'   an average of the `K` sentiment values, weighted by they respective topical
#'   proportions.
#'
#' @section Methods:
#'
#'   The function accepts two methods of computing sentiment:
#'
#'   - **proportional** computes the difference between the estimated positive
#'   and negative proportions for each document (and possibly each topic).
#'   \deqn{positive - negative}
#'
#'   - **proportionalPol** computes the difference between positive and negative
#'   proportions, divided by the sum of positive and negative proportions. As a
#'   result, the computed sentiment lies within the (-1;1) interval.
#'   \deqn{\frac{positive - negative}{positive + negative}}{(positive -
#'   negative) / (positive + negative)}
#'
#'   Both methods will lead to the same result for a JST model containing only
#'   negative and positive sentiments.
#'
#' @note The internal sentiment is stored internally in the *docvars* of the
#'   topic model. This means that sentiment may also be accessed through the
#'   [docvars()] function, although this is discouraged.
#'
#' @export
#' @examples
#' \donttest{# example dataset already contains ".sentiment" docvar
#' docvars(ECB_press_conferences_tokens)
#' # sentiment is automatically stored in the sentopicmodel object
#' lda <- LDA(ECB_press_conferences_tokens)
#' sentopics_sentiment(lda)
#'
#' # sentiment can be removed or modified by the assignment operator
#' sentopics_sentiment(lda) <- NULL
#' sentopics_sentiment(lda) <- docvars(ECB_press_conferences_tokens, ".sentiment")
#'
#' # for JST models, sentiment can be computed from the output of the model
#' jst <- JST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#' jst <- fit(jst, 100)
#' sentopics_sentiment(jst, override = TRUE) # replace existing sentiment
#'
#' ## for rJST models one sentiment value is computed by topic
#' rjst <- rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#' rjst <- fit(rjst, 100)
#' sentopics_sentiment(rjst, override = TRUE)}
sentopics_sentiment <- function(
  x,
  method = c("proportional", "proportionalPol"),
  override = FALSE,
  quiet = FALSE,
  include_docvars = FALSE
) {
  ## CMD check
  .id <- positive <- negative <- topic <- L1_prob <- NULL

  docvars <- attr(x$tokens, "docvars")

  if (!override & ".sentiment" %in% names(docvars)) {
    if (!quiet & !inherits(x, "LDA")) {
      message(
        "'.sentiment' docvars found. Returning these values. To re-compute sentiment, please set `override = TRUE`."
      )
    }
    if (include_docvars) {
      res <- data.table(.id = names(x$tokens), docvars(x$tokens))
      if (".sentiment_scaled" %in% names(docvars)) {
        data.table::setcolorder(
          res,
          c(".id", ".sentiment", ".sentiment_scaled")
        )
      } else {
        data.table::setcolorder(res, c(".id", ".sentiment"))
      }
    } else {
      if (".sentiment_scaled" %in% names(docvars)) {
        res <-
          data.table(
            .id = names(x$tokens),
            .sentiment = docvars$`.sentiment`,
            .sentiment_scaled = docvars$`.sentiment_scaled`
          )
      } else {
        res <- data.table(
          .id = names(x$tokens),
          .sentiment = docvars$`.sentiment`
        )
      }
      if (attr(x, "Sdim") == "L2") {
        idx <- grepl("^\\.s_", names(docvars))
        res <- cbind(res, docvars[, idx])
      }
    }
    return(res[])
  }

  if (inherits(x, "LDA")) {
    stop(
      "Impossible to compute sentiment for an LDA model. Please input first a '.sentiment' docvars by either\n\t1: ensuring the presence of a '.sentiment' docvars in the dfm or tokens object used to create the model.\n\t2: using `sentopics_sentiment(x) <- value` to register a vector of sentiment values in the topic model object."
    )
  }
  if (any(!c("positive", "negative") %in% levels(x$vocabulary$lexicon))) {
    stop(
      "Sentiment computation requires defined positive and negative sentiment. Ensure that a lexicon containing negative and positive categories was provided when creating the model or input a '.sentiment' docvars by either\n\t1: ensuring the presence of a '.sentiment' docvars in the dfm or tokens object used to create the model.\n\t2: using `sentopics_sentiment(x) <- value` to register a vector of sentiment values in the topic model object."
    )
  }

  method <- match.arg(method)
  melted <- melt(x, include_docvars = FALSE)
  ## store order to reverse dcast ordering
  # ord <- order(unique(melted$.id))

  switch(
    method,
    proportionalPol = {
      fn <- function(dt) {
        dt[, list(
          .id,
          .sentiment = (positive - negative) / (positive + negative)
        )]
      }
    },
    proportional = {
      fn <- function(dt) {
        dt[, list(.id, .sentiment = (positive - negative))]
      }
    }
  )

  if (attr(x, "Sdim") == "L1") {
    ## then it is JST

    ## discard topics, only need L1_prob
    # res <- dcast(melted, .id ~ sent, value.var = "L1_prob", fun.aggregate = mean)
    res <- dcast(
      melted[, list("L1_prob" = mean(L1_prob)), by = c("sent", ".id")],
      .id ~ sent,
      value.var = "L1_prob"
    )
    res <- fn(res)

    ## recover initial ordering
    setkey(res, NULL)
    # res <- res[order(ord)]
    res <- res[match(unique(melted$.id), .id)]
    stopifnot(identical(res$.id, names(x$tokens)))

    docvars$`.sentiment` <- res$`.sentiment`
    data.table::setattr(x$tokens, "docvars", docvars)
    message("Sentiment computed and assigned internally")
  } else {
    LIST <- lapply(
      stats::setNames(
        levels(melted$topic),
        nm = paste0(".s_", levels(melted$topic))
      ),
      function(t) {
        fn(dcast(melted[topic == t], .id ~ sent, value.var = "prob"))
      }
    )
    res <- data.table::rbindlist(LIST, idcol = "topic")
    res <- dcast(res, .id ~ topic, value.var = ".sentiment")

    ## recover initial ordering
    setkey(res, NULL)
    # res <- res[order(ord)]
    res <- res[match(unique(melted$.id), .id)]
    stopifnot(identical(res$.id, names(x$tokens)))

    res$.sentiment <- rowSums(as.matrix(res, rownames = ".id") * x$theta)
    data.table::setcolorder(res, c(".id", ".sentiment"))

    docvars <- utils::modifyList(docvars, res[, -".id"])
    data.table::setattr(x$tokens, "docvars", docvars)
    message("Sentiment computed and assigned internally")
  }
  ## to recompute at the end of merge_topics, need this attribute
  data.table::setattr(x, "sentiment_not_external", TRUE)

  if (include_docvars) {
    res <- cbind(res, docvars(x$tokens))
  }

  res[]
}


#' @rdname sentopics_sentiment
#' @param value a numeric vector of sentiment to input into the model.
#' @export
`sentopics_sentiment<-` <- function(x, value) {
  if (!inherits(x, "sentopicmodel")) {
    stop(
      "Replacement of internal sentiment is only possible for topic models of package `sentopics`"
    )
  }
  if (anyNA(value)) {
    stop("NA sentiment not allowed.")
  }

  docvars <- attr(x$tokens, "docvars")
  if (".sentiment" %in% names(docvars) & !is.null(value)) {
    message("Replacing existing '.sentiment' docvars")
  }

  if (!is.null(value)) {
    x$tokens$`.sentiment` <- value
  }
  if (is.null(value)) {
    x$tokens$`.sentiment` <- NULL
    idx <- names(docvars)[
      names(docvars) %in%
        paste0(".s_", sentopics_labels(x, flat = FALSE)[["topic"]])
    ]
    if (length(idx) > 0) {
      for (i in idx) {
        eval(substitute(x$tokens$y <- NULL, list(y = i)))
        # eval(data.table::substitute2(x$tokens$y <- NULL, list(y = i)))
      }
    }
  }
  attr(x, "sentiment_not_external") <- NULL

  x
}

#' Internal date
#' @author Olivier Delmarcelle
#' @family sentopics helpers
#' @inheritParams sentopics_sentiment
#' @description Extract or replace the internal dates of a `sentopicmodel`. The
#'   internal dates are used to create time series using the functions
#'   [sentiment_series()] or [sentiment_topics()]. Dates should be provided by
#'   using `sentopics_date(x) <- value` or by storing a '.date' docvars in
#'   the [tokens] object used to create the model.
#' @export
#'
#' @note The internal date is stored internally in the *docvars* of the topic
#'   model. This means that dates may also be accessed through the [docvars()]
#'   function, although this is discouraged.
#'
#' @return a data.frame with the stored date per document.
#' @examples
#' # example dataset already contains ".date" docvar
#' docvars(ECB_press_conferences_tokens)
#' # dates are automatically stored in the sentopicmodel object
#' lda <- LDA(ECB_press_conferences_tokens)
#' sentopics_date(lda)
#'
#' # dates can be removed or modified by the assignment operator
#' sentopics_date(lda) <- NULL
#' sentopics_date(lda) <- docvars(ECB_press_conferences_tokens, ".date")
sentopics_date <- function(x, include_docvars = FALSE) {
  docvars <- quanteda::docvars(x$tokens)
  if (!".date" %in% names(docvars)) {
    stop(
      "No dates stored internally. Please add dates to the documents by either\n\t1: ensuring the presence of a '.date' docvars in the dfm or tokens object used to create the model.\n\t2: using `sentopics_date(x) <- value` to register a vector of Dates in the topic model object."
    )
  }
  if (include_docvars) {
    res <- data.table(.id = names(x$tokens), docvars(x$tokens))
    data.table::setcolorder(res, c(".id", ".date"))
  } else {
    res <- data.table(.id = names(x$tokens), .date = as.Date(docvars$`.date`))
  }
  res[]
}
#' @rdname sentopics_date
#' @param value a `Date`-coercible vector of dates to input into the model.
#' @export
`sentopics_date<-` <- function(x, value) {
  if (!inherits(x, "sentopicmodel")) {
    stop(
      "Replacement of internal date is only possible for topic models of package `sentopics`"
    )
  }
  if (anyNA(value)) {
    stop("NA date not allowed.")
  }

  docvars <- attr(x$tokens, "docvars")
  if (".date" %in% names(docvars) & !is.null(value)) {
    message("Replacing existing '.date' docvars")
  }

  if (!is.null(value)) {
    x$tokens$`.date` <- as.Date(value)
  }
  if (is.null(value)) {
    x$tokens$`.date` <- NULL
  }

  x
}


#' Setting topic or sentiment labels
#'
#' @author Olivier Delmarcelle
#' @family sentopics helpers
#' @inheritParams sentopics_sentiment
#' @description Extract or replace the labels of a `sentopicmodel`. The replaced
#'   labels will appear in most functions dealing with the output of the
#'   `sentomicmodel`.
#' @param flat if FALSE, return a list of dimension labels instead of a
#'   character vector.
#' @export
#' @seealso merge_topics
#' @return a character vector of topic/sentiment labels.
#' @examples
#' # by default, sentopics_labels() generate standard topic names
#' lda <- LDA(ECB_press_conferences_tokens)
#' sentopics_labels(lda)
#'
#' # to change labels, a named list must be provided
#' sentopics_labels(lda) <- list(
#'  topic = paste0("superTopic", 1:lda$K)
#' )
#' sentopics_labels(lda)
#'
#' # using NULL remove labels
#' sentopics_labels(lda) <- NULL
#' sentopics_labels(lda)
#'
#' # also works for JST/rJST models
#' jst <- JST(ECB_press_conferences_tokens)
#' sentopics_labels(jst) <- list(
#'   topic = paste0("superTopic", 1:jst$K),
#'   sentiment = c("negative", "neutral", "positive")
#' )
#' sentopics_labels(jst)
#'
#' # setting flat = FALSE return a list or labels for each dimension
#' sentopics_labels(jst, flat = FALSE)
sentopics_labels <- function(x, flat = TRUE) {
  res <- create_labels(x, flat = flat)
  if (!flat) {
    params <- sentopicmodel_params(x)
    names(res) <- c(params$L1_name, params$L2_name)[seq_along(res)]
  }
  res
}
#' @rdname sentopics_labels
#' @param value a list of future labels for the topic model. The list should be
#'   named and contain a character vector for each dimension to label. See the
#'   examples for a correct usage.
#' @export
`sentopics_labels<-` <- function(x, value) {
  if (is.null(value)) {
    attr(x, "labels") <- NULL
    x <- fit(x, 0, displayProgress = FALSE)
    return(x)
  }

  if (!is.list(x)) {
    stop("Only accepts list as input")
  }
  if (length(setdiff(names(value), c("topic", "sentiment"))) > 0) {
    stop("List should only contain named components 'topic' or 'sentiment'.")
  }
  if (length(value) < 1) {
    warning("Empty input, nothing is replaced.")
  }
  if (length(names(value)) < 1) {
    warning("Input list should be named.")
  }

  params <- sentopicmodel_params(x)
  if (is.null(attr(x, "labels"))) {
    attr(x, "labels") <- list()
  }
  if (params$L1_name %in% names(value)) {
    if (length(value[[params$L1_name]]) != params$L1) {
      stop(
        "The number of ",
        params$L1_name,
        " labels should match the number of topics."
      )
    }
    attr(x, "labels")[["L1"]] <- value[[params$L1_name]]
    if (params$Sdim == "L1") {
      levels(x$vocabulary$lexicon) <- value[[params$L1_name]]
    }
  }
  if (params$L2_name %in% names(value)) {
    if (length(value[[params$L2_name]]) != params$L2) {
      stop(
        "The number of ",
        params$L2_name,
        " labels should match the number of topics."
      )
    }
    attr(x, "labels")[["L2"]] <- value[[params$L2_name]]
    if (params$Sdim == "L2") {
      levels(x$vocabulary$lexicon) <- value[[params$L2_name]]
    }
  }

  ## force update of labels on theta phi ect..
  x <- fit(x, 0, displayProgress = FALSE)

  ## manually adjust stored sentiment in docvars (if any)
  docvars <- attr(x$tokens, "docvars")
  idx <- grepl("^\\.s_", names(docvars))
  if (any(idx) & !is.null(value$topic)) {
    names(docvars)[idx] <- paste0(".s_", value$topic)
    attr(x$tokens, "docvars") <- docvars
  }

  x
}


#' Compute a sentiment time series
#'
#' @family series functions
#' @inheritParams sentiment_topics
#'
#' @param x a [LDA()], [JST()] or [rJST()] model populated with internal dates
#'   and/or internal sentiment.
#'
#' @description Compute a sentiment time series based on the internal sentiment
#'   and dates of a `sentopicmodel`. The time series computation supports
#'   multiple sampling period and optionally allow computing a moving average.
#'
#' @return A time series of sentiment, stored as an [xts::xts] or
#'   data.frame.
#' @export
#' @seealso sentopics_sentiment sentopics_date
#' @examples
#' lda <- LDA(ECB_press_conferences_tokens)
#' series <- sentiment_series(lda, period = "month")
#'
#' # JST and rJST models can use computed sentiment from the sentiment layer,
#' # but the model must be estimated first.
#' rjst <- rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#' sentiment_series(rjst)
#'
#' sentopics_sentiment(rjst) <- NULL ## remove existing sentiment
#' rjst <- fit(rjst, 10) ## estimating the model is then needed
#' sentiment_series(rjst)
#'
#' # note the presence of both raw and scaled sentiment values
#' # in the initial object
#' sentopics_sentiment(lda)
#' sentopics_sentiment(rjst)
sentiment_series <- function(
  x,
  period = c("year", "quarter", "month", "day"),
  rolling_window = 1,
  scale = TRUE,
  scaling_period = c("1900-01-01", "2099-12-31"),
  as.xts = TRUE,
  ...
) {
  ## CMD check
  .date <- .sentiment <- sentiment <- NULL

  period <- match.arg(period)

  mis <- c()
  if (rolling_window > 1) {
    mis <- c("zoo")
  }
  if (as.xts) {
    mis <- c(mis, "xts")
  }
  mis <- missingSuggets(mis)
  if (length(mis) > 0) {
    stop(
      "Suggested packages are missing for the sentiment_series function.\n",
      "Please install first the following packages: ",
      paste0(mis, collapse = ", "),
      ".\n",
      "Install command: install.packages(",
      paste0("'", mis, "'", collapse = ", "),
      ")"
    )
  }

  res <- cbind(
    sentopics_sentiment(x, quiet = TRUE),
    sentopics_date(x)
  )
  ## TODO: maybe expand to multiple columns?
  # res <- res[, lapply(.SD, mean),
  #            keyby = list(date = floor_date(.date, period)),
  #            .SDcols = ".sentiment"]
  res <- res[,
    list(sentiment = mean(.sentiment)),
    keyby = list(date = floor_date(.date, period))
  ]

  if (rolling_window > 1) {
    ## Store existing dates
    idx <- res$date
    ## Fill empty periods
    res <- merge(
      data.table(date = seq(min(res$date), max(res$date), by = period)),
      res,
      by = "date",
      all.x = TRUE
    )

    ## Set default parameter for rolling
    dots <- list(...)
    if (is.null(dots$FUN)) {
      dots$FUN <- mean
    }
    if (is.null(dots$na.rm)) {
      dots$na.rm <- TRUE
    }
    if (is.null(dots$fill)) {
      dots$fill <- NA
    }
    if (is.null(dots$align)) {
      dots$align <- "right"
    }

    # res <- res[, list(date, sentiment = zoo::rollapply(sentiment, rolling_window, FUN = FUN, na.rm = na.rm, fill = fill, align = align, ...))]
    res <- res[, list(
      date,
      sentiment = do.call(
        zoo::rollapply,
        c(
          list(data = sentiment, width = rolling_window),
          dots
        )
      )
    )]

    res <- res[date %in% idx]
  }

  if (scale) {
    if (nrow(res) < 2) {
      stop(
        "At least two periods are required to scale de series. Please the date range of documents or use a shorter period."
      )
    }
    ## Set default parameter for scaling
    dots <- list(...)
    if (is.null(dots$na.rm)) {
      na.rm <- TRUE
    }
    if (is.null(dots$trim)) {
      trim <- 0
    }
    params <- res[
      date >= scaling_period[1] & date <= scaling_period[2],
      c(
        sigma = stats::sd(sentiment, na.rm = na.rm),
        mu = mean(sentiment, na.rm = na.rm, trim = trim)
      )
    ]

    res[, sentiment := (sentiment - params["mu"]) / params["sigma"]]
    ## Add scaled sentiment at the document level by reference
    {
      docvars <- attr(x$tokens, "docvars")
      docvars$`.sentiment_scaled` <- (docvars$`.sentiment` - params["mu"]) /
        params["sigma"]

      ## dealing with multiple values for rJST
      idx <- grepl("^\\.s_", names(docvars), perl = TRUE) &
        !grepl("_scaled$", names(docvars), perl = TRUE)
      if (any(idx)) {
        idx <- names(docvars)[idx]
        for (s in idx) {
          docvars[[paste0(s, "_scaled")]] <- (docvars[[s]] - params["mu"]) /
            params["sigma"]
        }
      }

      ## putting back the results in docvars
      data.table::setattr(x$tokens, "docvars", docvars)
    }
  }

  # res <- res[, list(s = mean(s)), by = list(date = floor_date(res[[date_docvar]], period))]
  if (as.xts & length(missingSuggets("xts")) == 0) {
    res <- xts::as.xts(res, dateFormat = "Date")
  }

  if (scale) {
    data.table::setattr(res, "scaling_parameters", params)
  }
  res[]
}


#' Breakdown the sentiment into topical components
#'
#' @family series functions
#' @inheritParams sentiment_topics
#'
#' @description Break down the sentiment series obtained with
#'   [sentiment_series()] into topical components. Sentiment is broken down at
#'   the document level using estimated topic proportions, then processed to
#'   create a time series and its components.
#'
#' @return A time series of sentiment, stored as an [xts::xts] object or as a
#'   data.frame.
#' @export
#'
#' @details The sentiment is broken down at the sentiment level assuming the
#'   following composition: \deqn{s = \sum^K_{i=1} s_i \times \theta_i}, where
#'   \eqn{s_i} is the sentiment of topic i and \eqn{theta_i} the proportion of
#'   topic i in a given document. For an LDA model, the sentiment of each topic
#'   is considered equal to the document sentiment (i.e. \eqn{s_i = s \forall i
#'   \in K}). The topical sentiment attention, defined by \eqn{s*_i = s_i \times
#'   \theta_i} represent the effective sentiment conveyed by a topic in a
#'   document. The topical sentiment attention of all documents in a period are
#'   averaged to compute the breakdown of the sentiment time series.
#' @seealso sentopics_sentiment sentopics_date
#' @examples
#' \donttest{lda <- LDA(ECB_press_conferences_tokens)
#' lda <- fit(lda, 100)
#' sentiment_breakdown(lda)
#'
#' # plot shortcut
#' plot_sentiment_breakdown(lda)
#'
#' # also available for rJST models (with topic-level sentiment)
#' rjst <- rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#' rjst <- fit(rjst, 100)
#' sentopics_sentiment(rjst, override = TRUE)
#' plot_sentiment_breakdown(rjst)}
sentiment_breakdown <- function(
  x,
  period = c("year", "quarter", "month", "day", "identity"),
  rolling_window = 1,
  scale = TRUE,
  scaling_period = c("1900-01-01", "2099-12-31"),
  plot = c(FALSE, TRUE, "silent"),
  as.xts = TRUE,
  ...
) {
  ## CMD check
  .id <- .date <- .sentiment <- .sentiment_scaled <- sentiment <- value <-
    variable <- width <- Topic <- date_center <- theta <- s <- ..cols <-
      prob <- NULL

  if (!inherits(x, c("LDA", "rJST"))) {
    stop("`sentiment_breakdown` is only implemented for LDA and rJST models.")
  }

  period <- match.arg(period)
  plot <- as.character(plot)
  plot <- match.arg(plot)
  plot <- as.logical(plot)

  mis <- c()
  if (rolling_window > 1) {
    mis <- c("zoo")
  }
  if (as.xts) {
    mis <- c(mis, "xts")
  }
  if (!isFALSE(plot)) {
    mis <- c(mis, "ggplot2")
  }
  mis <- missingSuggets(mis)
  if (length(mis) > 0) {
    stop(
      "Suggested packages are missing for the sentiment_breakdown function.\n",
      "Please install first the following packages: ",
      paste0(mis, collapse = ", "),
      ".\n",
      "Install command: install.packages(",
      paste0("'", mis, "'", collapse = ", "),
      ")"
    )
  }

  # proportions <- dcast(melt(x), .id ~ topic, value.var = "prob", fun.aggregate = sum)
  proportions <- dcast(
    melt(x)[, list(prob = sum(prob)), by = c("topic", ".id")],
    .id ~ topic,
    value.var = "prob"
  )

  if (scale & period != "identity") {
    invisible(sentiment_series(
      x,
      period = period,
      rolling_window = rolling_window,
      scale = scale,
      scaling_period = scaling_period,
      ...
    ))
    tmp_sent <- sentopics_sentiment(x, quiet = TRUE)
    cols <- grepl("^\\.s", names(tmp_sent)) & grepl("_scaled$", names(tmp_sent))
    cols <- names(tmp_sent)[cols]
    tmp_sent <- eval(substitute(
      tmp_sent[, c(".id", ..cols)],
      list(..cols = cols)
    ))
    # TODO: re-activate once data.table 1.14.3 is released.
    # tmp_sent <- tmp_sent[, c(".id", ..cols), env = I(list(..cols = cols))]
    names(tmp_sent) <- gsub(
      "(^\\.(?!id))|(_scaled$)",
      "",
      names(tmp_sent),
      perl = TRUE
    )
  } else {
    tmp_sent <- sentopics_sentiment(x, quiet = TRUE)
    cols <- grepl("^\\.s", names(tmp_sent)) &
      !grepl("_scaled$", names(tmp_sent))
    cols <- names(tmp_sent)[cols]
    tmp_sent <- eval(substitute(
      tmp_sent[, c(".id", ..cols)],
      list(..cols = cols)
    ))
    # TODO: re-activate once data.table 1.14.3 is released.
    # tmp_sent <- tmp_sent[, c(".id", ..cols), env = I(list(..cols = cols))]
    names(tmp_sent) <- gsub(
      "(^\\.(?!id))|(_scaled$)",
      "",
      names(tmp_sent),
      perl = TRUE
    )
  }

  proportions <- merge(
    merge(
      sentopics_date(x),
      tmp_sent,
      by = ".id",
      sort = FALSE
    ),
    proportions,
    by = ".id",
    sort = FALSE
  )

  ## early return
  if (period == "identity") {
    sCols <- names(proportions)[grepl("^s_", names(proportions))]
    if (length(sCols) == 0) {
      sCols <- "sentiment"
    }
    thetaCols <- sentopics_labels(x, flat = FALSE)[["topic"]]
    breakdown <- eval(substitute(
      proportions[, c(
        list(.id = .id, date = .date, sentiment = sentiment),
        mapply(
          function(s_i, theta_i) s_i * theta_i,
          theta_i = .SD[, theta],
          s_i = .SD[, s],
          SIMPLIFY = FALSE
        )
      )],
      list(s = sCols, theta = thetaCols)
    ))
    # TODO: re-activate once data.table 1.14.3 is released.
    # breakdown <- proportions[, c(
    #   list(.id = .id, date = .date, sentiment = sentiment),
    #   mapply(function(s_i, theta_i) s_i * theta_i,
    #          theta_i = .SD[, theta], s_i = .SD[, s],
    #          SIMPLIFY = FALSE)),
    #   env = I(list( s = sCols, theta = thetaCols))]
    return(breakdown)
  }

  if (length(tmp_sent) <= 2) {
    ## then there is only one sentiment column
    if (inherits(x, "rJST")) {
      warning(
        "Sentiment for the rJST model comes from an external source. This means that the sentiment layer of the model is ignored. Was it really your intent? Perhaps should you run `sentopics_sentiment(x, override = TRUE)` on the model before calling this function, or instead remove the sentiment layer by using an LDA model."
      )
    }
    breakdown <- proportions[,
      c(
        list(sentiment = mean(sentiment)),
        lapply(.SD, function(x) mean(x * sentiment))
      ),
      .SDcols = -c(".id", ".date", "sentiment"),
      by = list(date = floor_date(.date, period))
    ]
  } else {
    ## deal with topical sentiment values (rJST)
    sCols <- names(proportions)[grepl("^s_", names(proportions))]
    thetaCols <- sentopics_labels(x, flat = FALSE)[["topic"]]
    breakdown <- eval(substitute(
      proportions[,
        c(
          list(sentiment = mean(sentiment)),
          mapply(
            function(s_i, theta_i) mean(s_i * theta_i),
            theta_i = .SD[, theta],
            s_i = .SD[, s],
            SIMPLIFY = FALSE
          )
        ),
        by = list(date = floor_date(.date, period))
      ],
      list(s = sCols, theta = thetaCols)
    ))
    # TODO: re-activate once data.table 1.14.3 is released.
    # breakdown <- proportions[, c(list(sentiment = mean(sentiment)),
    #                              mapply(function(s_i, theta_i) mean(s_i * theta_i),
    #                                     theta_i = .SD[, theta], s_i = .SD[, s],
    #                                     SIMPLIFY = FALSE)),
    #                          by = list(date = floor_date(.date, period)),
    #                          env = I(list( s = sCols, theta = thetaCols))]
  }

  breakdown <- breakdown[order(date)]

  ## quick check
  if (
    !isTRUE(all.equal(
      breakdown$sentiment - rowSums(breakdown[, -c("date", "sentiment")]),
      rep(0, nrow(breakdown))
    ))
  ) {
    stop(
      "Computation of breakdown failed. Please contact the author of the package to work on a solution."
    )
  }

  if (rolling_window > 1) {
    ## Store existing dates
    idx <- breakdown$date
    ## Fill empty periods
    breakdown <- merge(
      data.table(
        date = seq(min(breakdown$date), max(breakdown$date), by = period)
      ),
      breakdown,
      by = "date",
      all.x = TRUE
    )

    ## Set default parameter for rolling
    dots <- list(...)
    if (is.null(dots$FUN)) {
      dots$FUN <- mean
    }
    if (is.null(dots$na.rm)) {
      dots$na.rm <- TRUE
    }
    if (is.null(dots$fill)) {
      dots$fill <- NA
    }
    if (is.null(dots$align)) {
      dots$align <- "right"
    }

    cols <- setdiff(names(breakdown), c("date"))
    breakdown[,
      (cols) := lapply(.SD, function(col) {
        do.call(
          zoo::rollapply,
          c(
            list(data = col, width = rolling_window),
            dots
          )
        )
      }),
      .SDcols = cols
    ]

    breakdown <- breakdown[date %in% idx]
  }

  ## Prepare plot
  if (!isFALSE(plot)) {
    ## Store existing dates
    idx <- breakdown$date
    ## Fill empty periods
    breakdown <- merge(
      data.table(
        date = seq(min(breakdown$date), max(breakdown$date), by = period)
      ),
      breakdown,
      by = "date",
      all.x = TRUE
    )

    plot_data <- breakdown[, lapply(.SD, nafill, type = "locf")]
    plot_data <- stats::na.omit(melt(
      plot_data,
      id.vars = "date",
      variable.name = "Topic"
    ))
    plot_data[, width := days_period(date, period)]
    plot_data[,
      date_center := as.POSIXct(date) + as.difftime(width / 2, units = "days")
    ]

    p_breakdown <- ggplot2::ggplot(
      plot_data[Topic != "sentiment"],
      ggplot2::aes(x = date_center, y = value, fill = Topic)
    ) +
      ggplot2::geom_col(
        alpha = .8,
        position = ggplot2::position_stack(reverse = TRUE),
        width = plot_data[Topic != "sentiment"]$width * 24 * 60 * 60
      ) +
      ggplot2::geom_line(
        data = plot_data[Topic == "sentiment"],
        ggplot2::aes(x = date_center, y = value, group = 1L),
        inherit.aes = FALSE,
        linewidth = .8
      ) +
      ggplot2::scale_fill_manual(values = make_colors(x, "L1")) +
      ggplot2::ylab("Sentiment") +
      ggplot2::xlab("Date") +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      # scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%B") +
      # theme_classic(base_size = 12) +
      # theme(legend.position = "bottom") +
      ggplot2::ggtitle("Sentiment breakdown")
    if (isTRUE(plot)) {
      print(p_breakdown)
    }
    breakdown <- breakdown[date %in% idx]
  }

  if (as.xts & length(missingSuggets("xts")) == 0) {
    breakdown <- xts::as.xts(breakdown, dateFormat = "Date")
  }

  if (!isFALSE(plot)) {
    attr(breakdown, "plot") <- p_breakdown
  }

  breakdown
}
#' @rdname sentiment_breakdown
#' @export
plot_sentiment_breakdown <- function(
  x,
  period = c("year", "quarter", "month", "day"),
  rolling_window = 1,
  scale = TRUE,
  scaling_period = c("1900-01-01", "2099-12-31"),
  ...
) {
  period <- match.arg(period)
  res <- sentiment_breakdown(
    x,
    period,
    rolling_window,
    scale,
    scaling_period,
    plot = "silent",
    as.xts = FALSE,
    ...
  )
  attr(res, "plot")
}

#' Compute time series of topical sentiments
#'
#' @family series functions
#'
#' @description Derive topical time series of sentiment from a [LDA()] or
#'   [rJST()] model. The time series are created by leveraging on estimated
#'   topic proportions and internal sentiment (for `LDA` models) or topical
#'   sentiment (for `rJST` models).
#'
#' @param x a [LDA()] or [rJST()] model populated with internal dates and/or
#'   internal sentiment.
#' @param period the sampling period within which the sentiment of documents
#'   will be averaged. `period = "identity"` is a special case that will return
#'   document-level variables before the aggregation happens. Useful to rapidly
#'   compute topical sentiment at the document level.
#' @param rolling_window if greater than 1, determines the rolling window to
#'   compute a moving average of sentiment. The rolling window is based on the
#'   period unit and rely on actual dates (i.e, is not affected by unequally
#'   spaced data points).
#' @param scale if `TRUE`, the resulting time series will be scaled to a mean of
#'   zero and a standard deviation of 1. This argument also has the side effect
#'   of attaching scaled sentiment values as *docvars* to the input object with
#'   the `_scaled` suffix.
#' @param scaling_period the date range over which the scaling should be
#'   applied. Particularly useful to normalize only the beginning of the time
#'   series.
#' @param plot if `TRUE`, prints a plot of the time series and attaches it as an
#'   attribute to the returned object. If `'silent'`, do not print the plot but
#'   still attaches it as an attribute.
#' @param plot_ridgelines if `TRUE`, time series are plotted as ridgelines.
#'   Requires `ggridges` package installed. If `FALSE`, the plot will use only
#'   standards `ggplot2` functions. If the argument is missing and the package
#'   `ggridges` is not installed, this will quietly switch to a `ggplot2`
#'   output.
#' @param as.xts if `TRUE`, returns an [xts::xts] object. Otherwise, returns a
#'   data.frame.
#' @param ... other arguments passed on to [zoo::rollapply()] or [mean()] and
#'   [sd()].
#'
#' @details A topical sentiment is computed at the document level for each
#'   topic. For an LDA model, the sentiment of each topic is considered equal to
#'   the document sentiment (i.e. \eqn{s_i = s \forall i \in K}). For a rJST
#'   model, these result from the proportions in the sentiment layer under each
#'   topic. To compute the topical time series, the topical sentiment of all
#'   documents in a period are aggregated according to their respective topic
#'   proportion. For example, for a given topic, the topical sentiment in period
#'   \eqn{t} is computed using: \deqn{s_t = \frac{\sum_{d = 1}^D s_d \times
#'   \theta_d}{\sum_{d = 1}^D \theta_d}}, where \eqn{s_d} is the sentiment of
#'   the topic in document d and \eqn{theta_d} the topic proportion in a
#'   document d.
#' @seealso sentopics_sentiment sentopics_date
#' @return an [xts::xts] or data.frame containing the time series of topical
#'   sentiments.
#' @export
#' @examples
#' \donttest{lda <- LDA(ECB_press_conferences_tokens)
#' lda <- fit(lda, 100)
#' sentiment_topics(lda)
#'
#' # plot shortcut
#' plot_sentiment_topics(lda, period = "month", rolling_window = 3)
#' # with or without ridgelines
#' plot_sentiment_topics(lda, period = "month", plot_ridgelines = FALSE)
#'
#' # also available for rJST models with internal sentiment computation
#' rjst <- rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#' rjst <- fit(rjst, 100)
#' sentopics_sentiment(rjst, override = TRUE)
#' sentiment_topics(rjst)}
sentiment_topics <- function(
  x,
  period = c("year", "quarter", "month", "day", "identity"),
  rolling_window = 1,
  scale = TRUE,
  scaling_period = c("1900-01-01", "2099-12-31"),
  plot = c(FALSE, TRUE, "silent"),
  plot_ridgelines = TRUE,
  as.xts = TRUE,
  ...
) {
  ## CMD check
  .id <- .date <- .sentiment <- .sentiment_scaled <- sentiment <- value <-
    variable <- theta <- s <- ..cols <- prob <- NULL

  if (!inherits(x, c("LDA", "rJST"))) {
    stop("`sentiment_topics` is only implemented for LDA and rJST models.")
  }

  period <- match.arg(period)
  plot <- as.character(plot)
  plot <- match.arg(plot)
  plot <- as.logical(plot)

  mis <- c()
  if (rolling_window > 1) {
    mis <- c("zoo")
  }
  if (as.xts) {
    mis <- c(mis, "xts")
  }
  if (!isFALSE(plot)) {
    mis <- c(mis, "ggplot2")
  }
  if (missing(plot_ridgelines) & length(missingSuggets("ggridges") > 0)) {
    plot_ridgelines <- FALSE
  }
  if (plot_ridgelines & !isFALSE(plot)) {
    mis <- c(mis, "ggridges")
  }
  mis <- missingSuggets(mis)
  if (length(mis) > 0) {
    stop(
      "Suggested packages are missing for the sentiment_topics function.\n",
      "Please install first the following packages: ",
      paste0(mis, collapse = ", "),
      ".\n",
      "Install command: install.packages(",
      paste0("'", mis, "'", collapse = ", "),
      ")"
    )
  }

  # proportions <- dcast(melt(x), .id ~ topic, value.var = "prob", fun.aggregate = sum)
  proportions <- dcast(
    melt(x)[, list(prob = sum(prob)), by = c("topic", ".id")],
    .id ~ topic,
    value.var = "prob"
  )

  if (scale & period != "identity") {
    invisible(sentiment_series(
      x,
      period = period,
      rolling_window = rolling_window,
      scale = scale,
      scaling_period = scaling_period,
      ...
    ))
    tmp_sent <- sentopics_sentiment(x, quiet = TRUE)
    cols <- grepl("^\\.s", names(tmp_sent)) & grepl("_scaled$", names(tmp_sent))
    cols <- names(tmp_sent)[cols]
    tmp_sent <- eval(substitute(
      tmp_sent[, c(".id", ..cols)],
      list(..cols = cols)
    ))
    # TODO: re-activate once data.table 1.14.3 is released.
    # tmp_sent <- tmp_sent[, c(".id", ..cols), env = I(list(..cols = cols))]
    names(tmp_sent) <- gsub(
      "(^\\.(?!id))|(_scaled$)",
      "",
      names(tmp_sent),
      perl = TRUE
    )
  } else {
    tmp_sent <- sentopics_sentiment(x, quiet = TRUE)
    cols <- grepl("^\\.s", names(tmp_sent)) &
      !grepl("_scaled$", names(tmp_sent))
    cols <- names(tmp_sent)[cols]
    tmp_sent <- eval(substitute(
      tmp_sent[, c(".id", ..cols)],
      list(..cols = cols)
    ))
    # TODO: re-activate once data.table 1.14.3 is released.
    # tmp_sent <- tmp_sent[, c(".id", ..cols), env = I(list(..cols = cols))]
    names(tmp_sent) <- gsub(
      "(^\\.(?!id))|(_scaled$)",
      "",
      names(tmp_sent),
      perl = TRUE
    )
  }

  proportions <- merge(
    merge(
      sentopics_date(x),
      tmp_sent,
      by = ".id",
      sort = FALSE
    ),
    proportions,
    by = ".id",
    sort = FALSE
  )

  ## early return
  if (period == "identity") {
    sCols <- names(proportions)[grepl("^s_", names(proportions))]
    if (length(sCols) == 0) {
      sCols <- "sentiment"
    }
    thetaCols <- sentopics_labels(x, flat = FALSE)[["topic"]]
    topical_sent <- eval(substitute(
      proportions[, c(
        list(.id = .id, date = .date),
        mapply(
          function(s_i, theta_i) s_i * theta_i,
          theta_i = .SD[, theta],
          s_i = .SD[, s],
          SIMPLIFY = FALSE
        )
      )],
      list(s = sCols, theta = thetaCols)
    ))
    # TODO: re-activate once data.table 1.14.3 is released.
    # topical_sent <- proportions[, c(
    #   list(.id = .id, date = .date),
    #   mapply(function(s_i, theta_i) s_i * theta_i,
    #          theta_i = .SD[, theta], s_i = .SD[, s],
    #          SIMPLIFY = FALSE)),
    #   env = I(list( s = sCols, theta = thetaCols))]
    return(topical_sent)
  }

  if (length(tmp_sent) <= 2) {
    ## then there is only one sentiment column
    if (inherits(x, "rJST")) {
      warning(
        "Sentiment for the rJST model comes from an external source. This means that the sentiment layer of the model is ignored. Was it really your intent? Perhaps should you run `sentopics_sentiment(x, override = TRUE)` on the model before calling this function, or instead remove the sentiment layer by using an LDA."
      )
    }
    topical_sent <- proportions[,
      c(lapply(.SD, function(w) sum(w * sentiment) / sum(w))),
      .SDcols = -c(".id", ".date", "sentiment"),
      by = list(date = floor_date(.date, period))
    ]
  } else {
    ## deal with topical sentiment values (rJST)
    sCols <- names(proportions)[grepl("^s_", names(proportions))]
    thetaCols <- sentopics_labels(x, flat = FALSE)[["topic"]]
    topical_sent <- eval(substitute(
      proportions[,
        mapply(
          function(s_i, theta_i) sum(s_i * theta_i) / sum(theta_i),
          theta_i = .SD[, theta],
          s_i = .SD[, s],
          SIMPLIFY = FALSE
        ),
        by = list(date = floor_date(.date, period))
      ],
      list(s = sCols, theta = thetaCols)
    ))
    # TODO: re-activate once data.table 1.14.3 is released.
    # topical_sent <- proportions[, mapply(function(s_i, theta_i) sum(s_i * theta_i) / sum(theta_i),
    #                                      theta_i = .SD[, theta], s_i = .SD[, s],
    #                                      SIMPLIFY = FALSE),
    #                             by = list(date = floor_date(.date, period)),
    #                             env = I(list( s = sCols, theta = thetaCols))]
  }

  topical_sent <- topical_sent[order(date)]

  if (rolling_window > 1) {
    ## Store existing dates
    idx <- topical_sent$date
    ## Fill empty periods
    topical_sent <- merge(
      data.table(
        date = seq(min(topical_sent$date), max(topical_sent$date), by = period)
      ),
      topical_sent,
      by = "date",
      all.x = TRUE
    )

    ## Set default parameter for rolling
    dots <- list(...)
    if (is.null(dots$FUN)) {
      dots$FUN <- mean
    }
    if (is.null(dots$na.rm)) {
      dots$na.rm <- TRUE
    }
    if (is.null(dots$fill)) {
      dots$fill <- NA
    }
    if (is.null(dots$align)) {
      dots$align <- "right"
    }

    cols <- setdiff(names(topical_sent), c("date"))
    topical_sent[,
      (cols) := lapply(.SD, function(col) {
        do.call(
          zoo::rollapply,
          c(
            list(data = col, width = rolling_window),
            dots
          )
        )
      }),
      .SDcols = cols
    ]

    topical_sent <- topical_sent[date %in% idx]
  }

  ## Prepare plot
  if (!isFALSE(plot)) {
    plot_data <- topical_sent[, lapply(.SD, nafill, type = "locf")]
    plot_data <- stats::na.omit(melt(plot_data, id.vars = "date"))

    if (plot_ridgelines) {
      plot_data <- plot_data[, list(
        date,
        value = value / max(abs(value)),
        variable
      )]

      p_topical_sent <-
        ggplot2::ggplot(
          plot_data,
          ggplot2::aes(
            date,
            height = value,
            y = variable,
            group = variable,
            fill = variable
          )
        ) +
        ggridges::geom_ridgeline(min_height = -100, scale = 1 / 2) +
        ggplot2::scale_fill_manual(values = make_colors(x, "L1")) +
        # ggplot2::scale_x_date(expand = c(0,0)) +
        ggplot2::guides(fill = "none") +
        ggplot2::ylab("Topical sentiment") +
        ggplot2::xlab("Date")
    } else {
      p_topical_sent <-
        ggplot2::ggplot(
          plot_data,
          ggplot2::aes(date, value, color = variable)
        ) +
        ggplot2::geom_line(linewidth = 1.5) +
        # ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE, title = "Topic")) +
        ggplot2::guides(color = "none") +
        ggplot2::scale_color_manual(values = make_colors(x, "L1")) +
        # ggplot2::scale_y_continuous(expand = c(0,0), labels = function(breaks) sprintf("%.f%%", breaks * 100) ) +
        ggplot2::scale_x_date(expand = c(0, 0)) +
        ggplot2::ylab("Topical sentiment") +
        ggplot2::xlab("Date") +
        ggplot2::facet_wrap(. ~ variable)
    }

    if (isTRUE(plot)) print(p_topical_sent)
  }

  if (as.xts & length(missingSuggets("xts")) == 0) {
    topical_sent <- xts::as.xts(topical_sent, dateFormat = "Date")
  }

  if (!isFALSE(plot)) {
    attr(topical_sent, "plot") <- p_topical_sent
  }

  topical_sent
}
#' @rdname sentiment_topics
#' @export
plot_sentiment_topics <- function(
  x,
  period = c("year", "quarter", "month", "day"),
  rolling_window = 1,
  scale = TRUE,
  scaling_period = c("1900-01-01", "2099-12-31"),
  plot_ridgelines = TRUE,
  ...
) {
  period <- match.arg(period)
  if (missing(plot_ridgelines) & length(missingSuggets("ggridges") > 0)) {
    plot_ridgelines <- FALSE
  }
  res <- sentiment_topics(
    x,
    period,
    rolling_window,
    scale,
    scaling_period,
    plot_ridgelines,
    plot = "silent",
    as.xts = FALSE,
    ...
  )
  attr(res, "plot")
}

#' Compute the topic or sentiment proportion time series
#'
#' @family series functions
#' @inheritParams sentiment_topics
#'
#' @param x a [LDA()], [JST()] or [rJST()] model populated with internal dates
#'   and/or internal sentiment.
#' @param complete if FALSE, only compute proportions at the upper level of the
#'   topic model hierarchy (topics for [rJST] and sentiment for [JST]). No
#'   effect on [LDA] models.
#'
#' @description Aggregate the topical or sentiment proportions at the document
#'   level into time series.
#'
#' @return A time series of proportions, stored as an [xts::xts] object or as a
#'   data.frame.
#' @export
#'
#' @seealso sentopics_sentiment sentopics_date
#' @examples
#' \donttest{lda <- LDA(ECB_press_conferences_tokens)
#' lda <- fit(lda, 100)
#' proportion_topics(lda)
#'
#' # plot shortcut
#' plot_proportion_topics(lda, period = "month", rolling_window = 3)
#' # with or without ridgelines
#' plot_proportion_topics(lda, period = "month", plot_ridgelines = FALSE)
#'
#' # also available for rJST and JST models
#' jst <- JST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
#' jst <- fit(jst, 100)
#' # including both layers
#' proportion_topics(jst)
#' # or not
#' proportion_topics(jst, complete = FALSE)}
proportion_topics <- function(
  x,
  period = c("year", "quarter", "month", "day", "identity"),
  rolling_window = 1,
  complete = TRUE,
  plot = c(FALSE, TRUE, "silent"),
  plot_ridgelines = TRUE,
  as.xts = TRUE,
  ...
) {
  ## CMD check
  .date <- value <- variable <- prob <- NULL

  period <- match.arg(period)
  plot <- as.character(plot)
  plot <- match.arg(plot)
  plot <- as.logical(plot)

  mis <- c()
  if (rolling_window > 1) {
    mis <- c("zoo")
  }
  if (as.xts) {
    mis <- c(mis, "xts")
  }
  if (!isFALSE(plot)) {
    mis <- c(mis, "ggplot2")
  }
  if (missing(plot_ridgelines) & length(missingSuggets("ggridges") > 0)) {
    plot_ridgelines <- FALSE
  }
  if (plot_ridgelines & !isFALSE(plot)) {
    mis <- c(mis, "ggridges")
  }
  mis <- missingSuggets(mis)
  if (length(mis) > 0) {
    stop(
      "Suggested packages are missing for the proportion_topics function.\n",
      "Please install first the following packages: ",
      paste0(mis, collapse = ", "),
      ".\n",
      "Install command: install.packages(",
      paste0("'", mis, "'", collapse = ", "),
      ")"
    )
  }

  if (complete) {
    proportions <- switch(
      class(x)[1],
      LDA = {
        dcast(melt(x), .id ~ topic, value.var = "prob")
      },
      rJST = {
        dcast(melt(x), .id ~ topic + sent, value.var = "prob")
      },
      JST = {
        dcast(melt(x), .id ~ sent + topic, value.var = "prob")
      },
      stop("Undefined input")
    )
  } else {
    proportions <- switch(
      class(x)[1],
      LDA = {
        dcast(melt(x), .id ~ topic, value.var = "prob")
      },
      rJST = {
        # dcast(melt(x), .id ~ topic, value.var = "prob", fun.aggregate = sum)
        dcast(
          melt(x)[, list(prob = sum(prob)), by = c("topic", ".id")],
          .id ~ topic,
          value.var = "prob"
        )
      },
      JST = {
        # dcast(melt(x), .id ~ sent, value.var = "prob", fun.aggregate = sum)
        dcast(
          melt(x)[, list(prob = sum(prob)), by = c("sent", ".id")],
          .id ~ sent,
          value.var = "prob"
        )
      },
      stop("Undefined input")
    )
  }

  proportions <- merge(
    sentopics_date(x),
    proportions,
    by = ".id",
    sort = FALSE
  )

  ## early return
  if (period == "identity") {
    data.table::setnames(proportions, ".date", "date")
    return(proportions)
  }

  proportions <- proportions[,
    c(lapply(.SD, mean)),
    .SDcols = -c(".id", ".date"),
    by = list(date = floor_date(`.date`, period))
  ]
  proportions <- proportions[order(date)]

  ## quick check
  if (
    !isTRUE(all.equal(
      rowSums(proportions[, -c("date")]),
      rep(1, nrow(proportions))
    ))
  ) {
    stop(
      "Computation of breakdown failed. Please contact the author of the package to work on a solution."
    )
  }

  if (rolling_window > 1) {
    ## Store existing dates
    idx <- proportions$date
    ## Fill empty periods
    proportions <- merge(
      data.table(
        date = seq(min(proportions$date), max(proportions$date), by = period)
      ),
      proportions,
      by = "date",
      all.x = TRUE
    )

    ## Set default parameter for rolling
    dots <- list(...)
    if (is.null(dots$FUN)) {
      dots$FUN <- mean
    }
    if (is.null(dots$na.rm)) {
      dots$na.rm <- TRUE
    }
    if (is.null(dots$fill)) {
      dots$fill <- NA
    }
    if (is.null(dots$align)) {
      dots$align <- "right"
    }

    cols <- setdiff(names(proportions), c("date"))
    proportions[,
      (cols) := lapply(.SD, function(col) {
        do.call(
          zoo::rollapply,
          c(
            list(data = col, width = rolling_window),
            dots
          )
        )
      }),
      .SDcols = cols
    ]

    proportions <- proportions[date %in% idx]
  }

  ## Prepare plot
  if (!isFALSE(plot)) {
    plot_data <- proportions[, lapply(.SD, nafill, type = "locf")]
    plot_data <- stats::na.omit(melt(plot_data, id.vars = "date"))

    if (inherits(x, "LDA") | !complete) {
      colorScope <- "L1"
    } else {
      colorScope <- c("L1", "L2")
    }

    # if (plot_ridgelines & length(missingSuggets("ggridges")) == 0) {
    if (plot_ridgelines) {
      plot_data <- plot_data[, list(
        date,
        value = value / max(abs(`value`)),
        `variable`
      )]

      p_proportions <-
        ggplot2::ggplot(
          plot_data,
          ggplot2::aes(
            date,
            height = value,
            y = variable,
            group = variable,
            fill = variable
          )
        ) +
        ggridges::geom_ridgeline(min_height = 0, scale = 1 / 1.1) +
        # ggplot2::scale_x_date(expand = c(0,0)) +
        ggplot2::scale_fill_manual(values = make_colors(x, colorScope)) +
        ggplot2::guides(fill = "none") +
        ggplot2::ylab(ifelse(
          inherits(x, "JST"),
          "Sentiment proportion",
          "Topical proportion"
        )) +
        ggplot2::xlab("Date")
    } else {
      # if (plot_ridgelines) message("Package `ggridges` is missing. Defaulting to standard ggplot.")

      # plot_data[, cum_value := cumsum(value), by = "date"]
      p_proportions <-
        ggplot2::ggplot(plot_data, ggplot2::aes(date, value, fill = variable)) +
        ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
        # ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE,
        #                                              title = ifelse(inherits(x, "JST"), "Sentiment", "Topic"))) +
        ggplot2::guides(fill = "none") +
        ggplot2::scale_fill_manual(values = make_colors(x, colorScope)) +
        ggplot2::scale_y_continuous(
          expand = c(0, 0),
          labels = function(breaks) sprintf("%.f%%", breaks * 100),
          limits = c(0, NA)
        ) +
        ggplot2::scale_x_date(expand = c(0, 0)) +
        ggplot2::ylab(ifelse(
          inherits(x, "JST"),
          "Sentiment proportion",
          "Topical proportion"
        )) +
        ggplot2::xlab("Date") +
        ggplot2::facet_wrap(. ~ variable)

      # for non-LDA models, draw line only at intersection between upper layer
      # labels
      ## With the update and the built-in facet wrap, the distinction no longer
      ## make sense
      # if (inherits(x, "LDA") | !complete) {
      p_proportions <- p_proportions +
        ggplot2::geom_line(position = ggplot2::position_stack(reverse = TRUE))
      # } else {
      #   tmp <- create_labels(x, flat = FALSE)
      #   tmp <- sapply(tmp$L1, paste0, "_", tmp$L2[length(tmp$L2)], USE.NAMES = FALSE)
      #   plot_data_alt <- plot_data[, list(variable, value = cumsum(value)), by = "date"]
      #   plot_data_alt <- plot_data_alt[variable %in% tmp]
      #   p_proportions <- p_proportions +
      #     ggplot2::geom_line(data = plot_data_alt)
      # }
    }

    if (isTRUE(plot)) print(p_proportions)
  }

  if (as.xts & length(missingSuggets("xts")) == 0) {
    proportions <- xts::as.xts(proportions, dateFormat = "Date")
  }

  if (!isFALSE(plot)) {
    attr(proportions, "plot") <- p_proportions
  }

  proportions
}
#' @rdname proportion_topics
#' @export
plot_proportion_topics <- function(
  x,
  period = c("year", "quarter", "month", "day"),
  rolling_window = 1,
  complete = TRUE,
  plot_ridgelines = TRUE,
  ...
) {
  period <- match.arg(period)
  if (missing(plot_ridgelines) & length(missingSuggets("ggridges") > 0)) {
    plot_ridgelines <- FALSE
  }
  res <- proportion_topics(
    x,
    period,
    rolling_window,
    complete,
    plot_ridgelines,
    plot = "silent",
    as.xts = FALSE,
    ...
  )
  attr(res, "plot")
}

#' @keywords internal
days_period <- function(date, period) {
  if (period == "day") {
    return(rep(1, length(date)))
  }
  attributes(date)
  tmp <- data.table::transpose(
    lapply(date, function(x) rev(seq(x, by = period, length.out = 2)))
  )
  for (i in seq_along(tmp)) {
    class(tmp[[i]]) <- "Date"
  }
  as.numeric(difftime(tmp[[1]], tmp[[2]]))
}

#' @keywords internal
make_colors <- function(x, dimrange = c("L1", "L2")) {
  params <- sentopicmodel_params(x)
  if (!("L2" %in% dimrange)) {
    params$L2 <- 1L
  }
  if (params$L1 < 10) {
    cols <- unlist(lapply(
      RColorBrewer::brewer.pal(max(params$L1, 3), "Set1"),
      spreadColor,
      params$L2,
      range = .2
    ))
  } else {
    cols <- unlist(lapply(
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(7, "Set1"))(
        params$L1
      ),
      spreadColor,
      params$L2,
      range = .2
    ))
  }
  cols
}

#' @keywords internal
sentopicmodel_params <- function(x) {
  x <- as.sentopicmodel(x)
  c(
    x[c(
      "L1",
      "L2",
      # "L1prior", "L2prior", "beta",
      "L1cycle",
      "L2cycle"
    )],
    attributes(x)[c("reversed", "Sdim")],
    L1_name = ifelse(attr(x, "reversed") == TRUE, "topic", "sentiment"),
    L2_name = ifelse(attr(x, "reversed") == FALSE, "topic", "sentiment")
  )
}
