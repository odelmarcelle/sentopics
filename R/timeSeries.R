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
#' @param method the method used to compute sentiment. Currently, only
#'   "proportionalPol" is implemented. Ignored if an internal sentiment is
#'   already stored.
#' @param override by default, the function computes sentiment only if no
#'   internal sentiment is already stored within the `sentopicmodel` object.
#'   This avoid, for example, erasing externally provided sentiment. Set to
#'   `TRUE` to force computation of new sentiment values.
#' @param quiet if `FALSE`, print a message when internal sentiment is found.
#' @param include_docvars if `TRUE` the function will return all docvars stored
#'   in the internal `tokens` object of the model
#'
#' @export
#' @examples
#' jst <- JST(ECB_speeches, lexicon = LoughranMcDonald)
#' sentopics_sentiment(jst)
#'
#' jst <- grow(jst, 10)
#' sentopics_sentiment(jst, override = TRUE)
#'
#' ## Multivariate sentiment for rJST models
#' rjst <- rJST(ECB_speeches, lexicon = LoughranMcDonald)
#' rjst <- grow(rjst, 10)
#' sentopics_sentiment(rjst, override = TRUE)
#'
#'
#'
#'
#'
#'
#'

sentopics_sentiment <- function(x,
                      method = "proportionalPol",
                      override = FALSE,
                      quiet = FALSE,
                      include_docvars = FALSE) {
  ## CMD check
  .id <- positive <- negative <- topic <- NULL

  docvars <- attr(x$tokens, "docvars")

  if (!override & ".sentiment" %in% names(docvars)) {
    if (!quiet) message("'.sentiment' docvars found. Returning these values. To re-compute sentiment, please set `override = TRUE`.")
    if (include_docvars) {
      res <- data.table(.id = names(x$tokens), docvars(x$tokens))
      if (".sentiment_scaled" %in% names(docvars))
        data.table::setcolorder(res, c(".id", ".sentiment", ".sentiment_scaled")) else
        data.table::setcolorder(res, c(".id", ".sentiment"))
    } else {
      if (".sentiment_scaled" %in% names(docvars)) res <-
          data.table(.id = names(x$tokens),
                     .sentiment = docvars$`.sentiment`,
                     .sentiment_scaled = docvars$`.sentiment_scaled`
          ) else res <-
              data.table(.id = names(x$tokens),
                         .sentiment = docvars$`.sentiment`)
    }
    return(res[])
  }

  if (inherits(x, "LDA")) stop("Impossible to compute sentiment for a LDA model. Please input first a '.sentiment' docvars by either \n\t1: ensuring the presence of a '.sentiment' docvars in the dfm or tokens object used to create the model.\n\t2: using `sentopics_sentiment(x) <- value` to register a vector of sentiment values in the topic model object.")

  method <- match.arg(method)
  melted <- melt(x, include_docvars = FALSE)
  ## store order to reverse dcast ordering
  ord <- order(unique(melted$.id))

  switch(method,
         proportionalPol = {
           fn <- function(dt) {
             dt[, list(.id, .sentiment = (positive - negative) / (positive + negative))]
           }
         })

  if ( names(melted)[1] != "sent" ) { ## then it is JST

    ## discard topics, only need L1_prob
    res <- dcast(melted, .id ~ sent, value.var = "L1_prob", fun.aggregate = mean)
    res <- fn(res)

  } else {
    LIST <- lapply(
      stats::setNames(levels(melted$topic), nm = paste0("s_", levels(melted$topic))),
      function(t) {
        fn(dcast(melted[topic == t], .id ~ sent, value.var = "prob"))
      })
    res <- data.table::rbindlist(LIST, idcol = "topic")
    res <- dcast(res, .id ~ topic, value.var = ".sentiment")
  }

  ## recover initial ordering
  setkey(res, NULL)
  res <- res[order(ord)]
  stopifnot(identical(res$.id, names(x$tokens)))

  docvars$`.sentiment` <- res$`.sentiment`
  data.table::setattr(x$tokens, "docvars", docvars)
  message("Sentiment computed and assigned internally")

  if (include_docvars) { res <- cbind(res, docvars(x$tokens)) }

  res[]
}


`sentopics_sentiment<-` <- function(x, value) {
  if (!inherits(x, "sentopicmodel")) stop("Replacement of internal sentiment is only possible for topic models of package `sentopics`")

  docvars <- attr(x$tokens, "docvars")
  if (".sentiment" %in% names(docvars) & !is.null(value)) {
    message("Replacing existing '.sentiment' docvars")
  }

  if (!is.null(value)) x$tokens$`.sentiment` <- value
  if (is.null(value)) x$tokens$`.sentiment` <- NULL
  x
}

#' Title
#' @family sentopics helpers
#' @inherit sentopics_sentiment
#' @export
#'
sentopics_date <- function(x, include_docvars = FALSE) {
  docvars <- quanteda::docvars(x$tokens)
  if (!".date" %in% names(docvars)) stop("No dates stored internally. Please add dates to the documents by either\n\t1: ensuring the presence of a '.date' docvars in the dfm or tokens object used to create the model.\n\t2: using `sentopics_date(x) <- value` to register a vector of Dates in the topic model object.")
  if (include_docvars) {
    res <- data.table(.id = names(x$tokens), docvars(x$tokens))
    data.table::setcolorder(res, c(".id", ".date"))
  } else {
    res <- data.table(.id = names(x$tokens), .date = as.Date(docvars$`.date`))
  }
  res[]
}
`sentopics_date<-` <- function(x, value) {
  if (!inherits(x, "sentopicmodel")) stop("Replacement of internal date is only possible for topic models of package `sentopics`")

  docvars <- attr(x$tokens, "docvars")
  if (".date" %in% names(docvars) & !is.null(value)) {
    message("Replacing existing '.date' docvars")
  }

  if (!is.null(value)) x$tokens$`.date` <- as.Date(value)
  if (is.null(value)) x$tokens$`.date` <- NULL

  x
}

#' Title
#' @family sentopics helpers
#' @inherit sentopics_sentiment
#' @export
#'
sentopics_labels <- function(x) {
  if (is.null(attr(x, "labels"))) res <- create_labels(as.sentopicmodel(x), class(x)[1])
  else {
    res <- is.null(attr(x, "labels"))
    res <- sapply(res$L1, paste0, "_", res$L2, simplify = FALSE)
  }
  res
}
`sentopics_labels<-` <- function(x, value) {

  if (is.null(value)) {
    attr(x, "labels") <- NULL
    return(x)
  }

  if (!is.list(x)) stop("Only accepts list as input")
  if ( length( setdiff(names(value), c("topic", "sentiment")) ) > 0 ) stop("List should only contain named components 'topic' or 'sentiment'.")
  if (length(value) < 1) warning("Empty input, nothing is replaced.")

  params <- sentopicmodel_params(x)
  if (is.null(attr(x, "labels"))) attr(x, "labels") <- list()
  if ( params$L1_name %in% names(value) ) {
    if ( length(value[[params$L1_name]]) != params$L1 ) stop("The number of ", params$L1_name, " labels should match the number of topics.")
    attr(x, "labels")[["L1"]] <- value[[params$L1_name]]
  }
  if ( params$L2_name %in% names(value) ) {
    if ( length(value[[params$L2_name]]) != params$L2 ) stop("The number of ", params$L2_name, " labels should match the number of topics.")
    attr(x, "labels")[["L2"]] <- value[[params$L2_name]]
  }

  ## force update of labels on theta phi ect..
  x <- grow(x, 0, displayProgress = FALSE)
  # class <- class(x)[1]
  # x <- as.sentopicmodel(x)
  # fun <- get(paste0("as.", class))
  # x <- fun(reorder_sentopicmodel(x))

  x
}

#' Title
#'
#' @inherit sentiment_topics
#' @export
#'
sentiment_series <- function(x,
                             period = c("year", "month", "day"),
                             rolling_window = 1,
                             scale =  TRUE,
                             scaling_period = c("1900-01-01", "2099-12-31"),
                             as.xts = TRUE,
                             ...) {
  ## CMD check
  .date <- .sentiment <- sentiment <- NULL

  period <- match.arg(period)

  mis <- c()
  if (rolling_window > 1) mis <- c("zoo")
  if (as.xts) mis <- c(mis, "xts")
  mis <- missingSuggets(mis)
  if (length(mis) > 0) stop("Suggested packages are missing for the sentiment_series function.\n",
                            "Please install first the following packages: ",
                            paste0(mis, collapse = ", "),".\n",
                            "Install command: install.packages(",
                            paste0("'", mis, "'", collapse = ", "),")" )

  # if (is.character(date_docvar)) {
  #   if (!date_docvar %in% colnames(docvars(x$tokens)))
  #     stop("\"", date_docvar, "\" not found in the docvars of the quanteda object. Please check that the input of the model retained his docvars using `docvars()`. Instead of providing a column name, you can also directly input a date vector to this function.")
  # } else if (inherits(date_docvar, "Date")) {
  #   x$tokens$date <- date_docvar
  #   date_docvar <- "date"
  # } else {
  #   stop("Incorrect input to date_docvar.")
  # }
  res <- cbind(
    sentopics_sentiment(x, quiet = TRUE),
    sentopics_date(x)
  )
  ## TODO: maybe expand to multiple columns?
  # res <- res[, lapply(.SD, mean),
  #            keyby = list(date = floor_date(.date, period)),
  #            .SDcols = ".sentiment"]
  res <- res[, list(sentiment = mean(.sentiment)),
             keyby = list(date = floor_date(.date, period))]

  if (rolling_window > 1) {
    ## Store existing dates
    idx <- res$date
    ## Fill empty periods
    res <- merge(data.table(date = seq(min(res$date),
                                       max(res$date),
                                       by = period)),
                 res, by = "date",
                 all.x = TRUE)

    ## Set default parameter for rolling
    dots <- list(...)
    if (is.null(dots$FUN)) dots$FUN <- mean
    if (is.null(dots$na.rm)) dots$na.rm <- TRUE
    if (is.null(dots$fill)) dots$fill <- NA
    if (is.null(dots$align)) dots$align <- "right"

    # res <- res[, list(date, sentiment = zoo::rollapply(sentiment, rolling_window, FUN = FUN, na.rm = na.rm, fill = fill, align = align, ...))]
    res <- res[, list(date, sentiment = do.call(zoo::rollapply, c(
      list(data = sentiment, width = rolling_window),
      dots)))]

    res <- res[date %in% idx]
  }

  if (scale) {
    ## Set default parameter for scaling
    dots <- list(...)
    if (is.null(dots$na.rm)) na.rm <- TRUE
    params <- res[date >= scaling_period[1] & date <= scaling_period[2],
                    c(sigma = stats::sd(sentiment, na.rm = na.rm), mu = mean(sentiment, na.rm = na.rm))]

    res[, sentiment := (sentiment - params["mu"]) / params["sigma"] ]
    ## Add scaled sentiment at the document level by reference
    {
      docvars <- attr(x$tokens, "docvars")
      docvars$`.sentiment_scaled` <- (docvars$`.sentiment` - params["mu"]) / params["sigma"]
      data.table::setattr(x$tokens, "docvars", docvars)
    }
  }

  # res <- res[, list(s = mean(s)), by = list(date = floor_date(res[[date_docvar]], period))]
  if (as.xts & length(missingSuggets("xts")) == 0)
    res <- xts::as.xts(res, dateFormat = "Date")

  if (scale) data.table::setattr(res, "scaling_parameters", params)
  res[]
}


#' Title
#'
#' @inherit sentiment_topics
#' @export
#'
sentiment_breakdown <- function(x,
                                period = c("year", "month", "day"),
                                rolling_window = 1,
                                scale =  TRUE,
                                scaling_period = c("1900-01-01", "2099-12-31"),
                                plot = c(FALSE, TRUE, "silent"),
                                as.xts = TRUE,
                                ...) {
  ## CMD check
  .id <- .date <- .sentiment <- .sentiment_scaled <- sentiment <-
    value <- variable <- width <- Topic <- date_center <- NULL

  period <- match.arg(period)
  plot <- as.character(plot)
  plot <- match.arg(plot)
  plot <- as.logical(plot)

  mis <- c()
  if (rolling_window > 1) mis <- c("zoo")
  if (as.xts) mis <- c(mis, "xts")
  if (!isFALSE(plot)) mis <- c(mis, "ggplot2")
  mis <- missingSuggets(mis)
  if (length(mis) > 0) stop("Suggested packages are missing for the sentiment_breakdown function.\n",
                            "Please install first the following packages: ",
                            paste0(mis, collapse = ", "),".\n",
                            "Install command: install.packages(",
                            paste0("'", mis, "'", collapse = ", "),")" )

  proportions <- dcast(melt(x), .id ~ topic, value.var = "prob")

  if (scale) {
    invisible(sentiment_series(x, period = period, rolling_window = rolling_window, scale = scale, scaling_period = scaling_period, ...))
    tmp_sent <- sentopics_sentiment(x, quiet = TRUE)[, list(.id, sentiment = .sentiment_scaled)]
  } else {
    tmp_sent <- sentopics_sentiment(x, quiet = TRUE)[, list(.id, sentiment = .sentiment)]
  }


  proportions <- merge(
    merge(
      sentopics_date(x),
      tmp_sent,
      by = ".id", sort = FALSE
    ),
    proportions,
    by = ".id", sort = FALSE
  )

  breakdown <- proportions[, c(list(sentiment = mean(sentiment)),
                               lapply(.SD, function(x) mean(x * sentiment) )), .SDcols = -c(".id", ".date", "sentiment"),
                           by = list(date = floor_date(.date, period))]
  breakdown <- breakdown[order(date)]

  if (rolling_window > 1) {
    ## Store existing dates
    idx <- breakdown$date
    ## Fill empty periods
    breakdown <- merge(data.table(date = seq(min(breakdown$date),
                                       max(breakdown$date),
                                       by = period)),
                 breakdown, by = "date",
                 all.x = TRUE)

    ## Set default parameter for rolling
    dots <- list(...)
    if (is.null(dots$FUN)) dots$FUN <- mean
    if (is.null(dots$na.rm)) dots$na.rm <- TRUE
    if (is.null(dots$fill)) dots$fill <- NA
    if (is.null(dots$align)) dots$align <- "right"

    cols <- setdiff(names(breakdown), c("date"))
    breakdown[,
      (cols) := lapply(.SD, function(col)
        do.call(zoo::rollapply, c(
          list(data = col, width = rolling_window),
          dots))
        ),
      .SDcols = cols]

    breakdown <- breakdown[date %in% idx]
  }

  ## Prepare plot
  if (!isFALSE(plot)) {
    ## Store existing dates
    idx <- breakdown$date
    ## Fill empty periods
    breakdown <- merge(data.table(date = seq(min(breakdown$date),
                                             max(breakdown$date),
                                             by = period)),
                       breakdown, by = "date",
                       all.x = TRUE)

    plot_data <- breakdown[, lapply(.SD, nafill, type = "locf")]
    plot_data <- stats::na.omit(melt(plot_data, id.vars = "date", variable.name = "Topic"))
    plot_data[, width := days_period(date, period)]
    plot_data[, date_center := as.POSIXct(date) + as.difftime(width / 2, units = "days")]

    p_breakdown <- ggplot2::ggplot(plot_data[Topic != "sentiment"], ggplot2::aes(x = date_center, y = value, fill = Topic)) +
      ggplot2::geom_col(alpha = .8, position = ggplot2::position_stack(reverse = TRUE), width = plot_data[Topic != "sentiment"]$width*24*60*60) +
      ggplot2::geom_line(data = plot_data[Topic == "sentiment"], ggplot2::aes(x = date_center, y = value, group = 1L), inherit.aes = FALSE, size = .8) +
      ggplot2::scale_fill_manual(values = make_colors(x, "L1")) +
      ggplot2::ylab("Sentiment") +
      ggplot2::xlab("Date") +
      # scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%B") +
      # theme_classic(base_size = 12) +
      # theme(legend.position = "bottom") +
      ggplot2::ggtitle("Breakdown of sentiment")
    if (isTRUE(plot)) print(p_breakdown)
    breakdown <- breakdown[date %in% idx]
  }


  if (as.xts & length(missingSuggets("xts")) == 0)
    breakdown <- xts::as.xts(breakdown, dateFormat = "Date")

  if (!isFALSE(plot)) attr(breakdown, "plot") <- p_breakdown

  breakdown

}
#' @rdname sentiment_breakdown
#' @export
#' @family plot functions
plot_sentiment_breakdown <- function(x,
                                     period = c("year", "month", "day"),
                                     rolling_window = 1,
                                     scale =  TRUE,
                                     scaling_period = c("1900-01-01", "2099-12-31"),
                                     ...) {
  res <- sentiment_breakdown(x, period, rolling_window, scale, scaling_period,
                             plot = "silent", as.xts = FALSE, ...)
  attr(res, "plot")
}

#' Title
#'
#' @param x ...
#' @param period ...
#' @param rolling_window ...
#' @param scale ...
#' @param scaling_period ...
#' @param plot ...
#' @param plot_ridgelines ...
#' @param as.xts ...
#' @param ... ...
#'
#' @return ...
#' @export
#'
sentiment_topics <- function(x,
                             period = c("year", "month", "day"),
                             rolling_window = 1,
                             scale =  TRUE,
                             scaling_period = c("1900-01-01", "2099-12-31"),
                             plot = c(FALSE,  TRUE, "silent"),
                             plot_ridgelines = TRUE,
                             as.xts = TRUE,
                             ...) {
  ## CMD check
  .id <- .date <- .sentiment <- .sentiment_scaled <- sentiment <-
    value <- variable <- NULL

  period <- match.arg(period)
  plot <- as.character(plot)
  plot <- match.arg(plot)
  plot <- as.logical(plot)

  mis <- c()
  if (rolling_window > 1) mis <- c("zoo")
  if (as.xts) mis <- c(mis, "xts")
  if (!isFALSE(plot)) mis <- c(mis, "ggplot2")
  if (plot_ridgelines & !isFALSE(plot)) mis <- c(mis, "ggridges")
  mis <- missingSuggets(mis)
  if (length(mis) > 0) stop("Suggested packages are missing for the sentiment_topics function.\n",
                            "Please install first the following packages: ",
                            paste0(mis, collapse = ", "),".\n",
                            "Install command: install.packages(",
                            paste0("'", mis, "'", collapse = ", "),")" )

  proportions <- dcast(melt(x), .id ~ topic, value.var = "prob")

  if (scale) {
    invisible(sentiment_series(x, period = period, rolling_window = rolling_window, scale = scale, scaling_period = scaling_period, ...))
    tmp_sent <- sentopics_sentiment(x, quiet = TRUE)[, list(.id, sentiment = .sentiment_scaled)]
  } else {
    tmp_sent <- sentopics_sentiment(x, quiet = TRUE)[, list(.id, sentiment = .sentiment)]
  }


  proportions <- merge(
    merge(
      sentopics_date(x),
      tmp_sent,
      by = ".id", sort = FALSE
    ),
    proportions,
    by = ".id", sort = FALSE
  )

  topical_sent <- proportions[, c(lapply(.SD, function(w) sum(w * sentiment) / sum(w) )),
                           .SDcols = -c(".id", ".date", "sentiment"),
                           by = list(date = floor_date(.date, period))]
  topical_sent <- topical_sent[order(date)]

  if (rolling_window > 1) {
    ## Store existing dates
    idx <- topical_sent$date
    ## Fill empty periods
    topical_sent <- merge(data.table(date = seq(min(topical_sent$date),
                                                max(topical_sent$date),
                                                by = period)),
                          topical_sent, by = "date",
                          all.x = TRUE)

    ## Set default parameter for rolling
    dots <- list(...)
    if (is.null(dots$FUN)) dots$FUN <- mean
    if (is.null(dots$na.rm)) dots$na.rm <- TRUE
    if (is.null(dots$fill)) dots$fill <- NA
    if (is.null(dots$align)) dots$align <- "right"

    cols <- setdiff(names(topical_sent), c("date"))
    topical_sent[,
              (cols) := lapply(.SD, function(col)
                do.call(zoo::rollapply, c(
                  list(data = col, width = rolling_window),
                  dots))
              ),
              .SDcols = cols]

    topical_sent <- topical_sent[date %in% idx]
  }

  ## Prepare plot
  if (!isFALSE(plot)) {
    plot_data <- topical_sent[, lapply(.SD, nafill, type = "locf")]
    plot_data <- stats::na.omit(melt(plot_data, id.vars = "date"))

    if (plot_ridgelines & length(missingSuggets("ggridges")) == 0) {
      plot_data <- plot_data[, list(date, value = value / max(abs(value)), variable)]

      p_topical_sent <-
        ggplot2::ggplot(plot_data, ggplot2::aes(date, height = value, y = variable, group = variable, fill = variable)) +
        ggridges::geom_ridgeline(min_height = -100, scale = 1/2) +
        ggplot2::scale_fill_manual(values = make_colors(x, "L1")) +
        # ggplot2::scale_x_date(expand = c(0,0)) +
        ggplot2::guides(fill = "none") +
        ggplot2::ylab("Topical sentiment")
    } else {

      if (plot_ridgelines) message("Package `ggridges` is missing. Defaulting to standard ggplot.")

      p_topical_sent <-
        ggplot2::ggplot(plot_data, ggplot2::aes(date, value, color = variable)) +
        ggplot2::geom_line(size = 1.5) +
        ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE, title = "Topic")) +
        ggplot2::scale_color_manual(values = make_colors(x, "L1")) +
        # ggplot2::scale_y_continuous(expand = c(0,0), labels = function(breaks) sprintf("%.f%%", breaks * 100) ) +
        ggplot2::scale_x_date(expand = c(0,0)) +
        ggplot2::theme_bw() +
        # ggplot2::geom_line(ggplot2::aes(date, cum_value, group = variable),
        #                    inherit.aes = FALSE) +
        ggplot2::ylab("Topical sentiment")
    }

    if (isTRUE(plot)) print(p_topical_sent)
  }

  if (as.xts & length(missingSuggets("xts")) == 0)
    topical_sent <- xts::as.xts(topical_sent, dateFormat = "Date")

  if (!isFALSE(plot)) attr(topical_sent, "plot") <- p_topical_sent

  topical_sent

}
#' @rdname sentiment_topics
#' @export
#' @family plot functions
plot_sentiment_topics <- function(x,
                                  period = c("year", "month", "day"),
                                  rolling_window = 1,
                                  scale =  TRUE,
                                  scaling_period = c("1900-01-01", "2099-12-31"),
                                  plot_ridgelines = TRUE,
                                  ...) {
  res <- sentiment_topics(x, period, rolling_window, scale, scaling_period,
                          plot_ridgelines, plot = "silent", as.xts = FALSE, ...)
  attr(res, "plot")
}

#' Title
#'
#' @inherit sentiment_topics
#' @export
#'
proportion_topics <- function(x,
                              period = c("year", "month", "day"),
                              rolling_window = 1,
                              plot = c(FALSE,  TRUE, "silent"),
                              plot_ridgelines = TRUE,
                              as.xts = TRUE,
                              ...) {
  ## CMD check
  .date <- value <- variable <- NULL

  period <- match.arg(period)
  plot <- as.character(plot)
  plot <- match.arg(plot)
  plot <- as.logical(plot)

  mis <- c()
  if (rolling_window > 1) mis <- c("zoo")
  if (as.xts) mis <- c(mis, "xts")
  if (!isFALSE(plot)) mis <- c(mis, "ggplot2")
  if (plot_ridgelines & !isFALSE(plot)) mis <- c(mis, "ggridges")
  mis <- missingSuggets(mis)
  if (length(mis) > 0) stop("Suggested packages are missing for the proportion_topics function.\n",
                            "Please install first the following packages: ",
                            paste0(mis, collapse = ", "),".\n",
                            "Install command: install.packages(",
                            paste0("'", mis, "'", collapse = ", "),")" )



  proportions <- dcast(melt(x), .id ~ topic, value.var = "prob")

  proportions <- merge(
    sentopics_date(x),
    proportions,
    by = ".id", sort = FALSE
  )

  proportions <- proportions[, c(lapply(.SD, mean)),
                              .SDcols = -c(".id", ".date"),
                              by = list(date = floor_date(`.date`, period))]
  proportions <- proportions[order(date)]

  if (rolling_window > 1) {
    ## Store existing dates
    idx <- proportions$date
    ## Fill empty periods
    proportions <- merge(data.table(date = seq(min(proportions$date),
                                                max(proportions$date),
                                                by = period)),
                          proportions, by = "date",
                          all.x = TRUE)

    ## Set default parameter for rolling
    dots <- list(...)
    if (is.null(dots$FUN)) dots$FUN <- mean
    if (is.null(dots$na.rm)) dots$na.rm <- TRUE
    if (is.null(dots$fill)) dots$fill <- NA
    if (is.null(dots$align)) dots$align <- "right"

    cols <- setdiff(names(proportions), c("date"))
    proportions[,
                 (cols) := lapply(.SD, function(col)
                   do.call(zoo::rollapply, c(
                     list(data = col, width = rolling_window),
                     dots))
                 ),
                 .SDcols = cols]

    proportions <- proportions[date %in% idx]
  }

  ## Prepare plot
  if (!isFALSE(plot)) {
    plot_data <- proportions[, lapply(.SD, nafill, type = "locf")]
    plot_data <- stats::na.omit(melt(plot_data, id.vars = "date"))


    if (plot_ridgelines & length(missingSuggets("ggridges")) == 0) {
      plot_data <- plot_data[, list(date, value = value / max(abs(`value`)), `variable`)]

      p_proportions <-
        ggplot2::ggplot(plot_data, ggplot2::aes(date, height = value, y = variable, group = variable, fill = variable)) +
        ggridges::geom_ridgeline(min_height = 0, scale = 1/1.1) +
        # ggplot2::scale_x_date(expand = c(0,0)) +
        ggplot2::scale_fill_manual(values = make_colors(x, "L1")) +
        ggplot2::guides(fill = "none") +
        ggplot2::ylab("Topical proportion")
      p_proportions
    } else {

      if (plot_ridgelines) message("Package `ggridges` is missing. Defaulting to standard ggplot.")

      # plot_data[, cum_value := cumsum(value), by = "date"]
      p_proportions <-
        ggplot2::ggplot(plot_data, ggplot2::aes(date, value, fill = variable)) +
        ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, title = "Topic")) +
        ggplot2::scale_fill_manual(values = make_colors(x, "L1")) +
        ggplot2::scale_y_continuous(expand = c(0,0), labels = function(breaks) sprintf("%.f%%", breaks * 100) ) +
        ggplot2::scale_x_date(expand = c(0,0)) +
        ggplot2::theme_bw() +
        # ggplot2::geom_line(ggplot2::aes(date, cum_value, group = variable),
        #                    inherit.aes = FALSE) +
        ggplot2::geom_line(position = ggplot2::position_stack(reverse = TRUE)) +
        ggplot2::ylab("Topical proportion")
    }


    if (isTRUE(plot)) print(p_proportions)
  }

  if (as.xts & length(missingSuggets("xts")) == 0)
    proportions <- xts::as.xts(proportions, dateFormat = "Date")

  if (!isFALSE(plot)) attr(proportions, "plot") <- p_proportions

  proportions

}
#' @rdname proportion_topics
#' @export
#' @family plot functions
plot_proportion_topics <- function(x,
                                   period = c("year", "month", "day"),
                                   rolling_window = 1,
                                   plot_ridgelines = TRUE,
                                   ...) {
  res <- proportion_topics(x, period, rolling_window, plot_ridgelines,
                             plot = "silent", as.xts = FALSE, ...)
  attr(res, "plot")
}

#' @keywords internal
days_period <- function(date, period) {
  attributes(date)
  tmp <- lapply(date, function(x)  rev(seq(x, by = period, length.out = 2)) ) |> data.table::transpose()
  for (i in seq_along(tmp)) {
    class(tmp[[i]]) <- "Date"
  }
  # tmp
  as.numeric(difftime(tmp[[1]], tmp[[2]]))
}

#' @keywords internal
make_colors <- function(x, dimrange = c("L1", "L2")) {
  params <- sentopicmodel_params(x)
  if (!("L2" %in% dimrange)) params$L2 <- 1L
  if (params$L1 < 10) {
    cols <- unlist(lapply(RColorBrewer::brewer.pal(max(params$L1, 3), "Set1"), spreadColor, params$L2, range = .2))
  }
  else cols <- unlist(lapply(
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(7, "Set1"))(params$L1),
    spreadColor, params$L2, range = .2))
  cols
}

#' @keywords internal
sentopicmodel_params <- function(x) {
  x <- as.sentopicmodel(x)
  c(
    x[c("L1", "L2",
        # "L1prior", "L2prior", "beta",
        "initLDA", "smooth", "initType", "L1cycle", "L2cycle")],
    attributes(x)[c("reversed", "Sdim")],
    L1_name = ifelse(attr(x, "reversed") == TRUE, "topic", "sentiment"),
    L2_name = ifelse(attr(x, "reversed") == FALSE, "topic", "sentiment")
  )
}


## TODO: REMOVE?
mixture_series <- function(x,
                             period = c("year", "month", "day"),
                             date_docvar = ".date",
                             as.xts = TRUE,
                             plot = TRUE) {
  period <- match.arg(period)
  if (is.character(date_docvar)) {
    if (!date_docvar %in% colnames(docvars(x$tokens)))
      stop("\"", date_docvar, "\" not found in the docvars of the quanteda object. Please check that the input of the model retained his docvars using `docvars()`. Instead of providing a column name, you can also directly input a date vector to this function.")
  } else if (inherits(date_docvar, "Date")) {
    x$tokens$date <- date_docvar
    date_docvar <- "date"
  } else {
    stop("Incorrect input to date_docvar.")
  }
  melted <- melt(x, include_docvars = TRUE)
  if (inherits(x, "LDA")) {
    L1_name <- "topic"
    L2_name <- "....PLACEHOLDER"
    melted$L1_prob <- melted$prob
    x$S <- 1
  } else {
    L1_name <- colnames(melted)[2]
    L2_name <- colnames(melted)[1]
  }

  # res <- melted[, lapply(.SD, mean),
  #                by = list(date = floor_date(melted[[date_docvar]], period),
  #                       L2_name = melted[[L2_name]],
  #                       melted[[L1_name]],
  #                       sentopic),
  #                .SDcols = c("L1_prob", "prob")]
  byList <- stats::setNames(list(
    floor_date(melted[[date_docvar]], period),
    melted[[L2_name]],
    melted[[L1_name]],
    melted[["sentopic"]]),
    c("date", L2_name, L1_name, "sentopic" ) )
  byList <- byList[!sapply(byList, is.null)]
  res <- melted[, lapply(.SD, mean),
                by = byList,
                .SDcols = c("L1_prob", "prob")]
  # res <- melted[, lapply(.SD, mean),
  #               by = setNames(list(
  #                 floor_date(melted[[date_docvar]], period),
  #                 melted[[L2_name]],
  #                 melted[[L1_name]],
  #                 melted[["sentopic"]]),
  #                 c("date", L2_name, L1_name, "sentopic" ) ),
  #               .SDcols = c("L1_prob", "prob")]
  if (plot) {
    lines <- unique(res[, c("date", ..L1_name, "L1_prob")])
    data.table::setnames(lines, L1_name, "L1")
    lines <- lines[, list(L1, y = cumsum(L1_prob)), by = "date"]

    if (attr(x, "reversed")) {
      L1 <- x$K
      L2 <- x$S
    } else {
      L1 <- x$S
      L2 <- x$K
    }

    if (L1 < 10) {
      cols <- unlist(lapply(RColorBrewer::brewer.pal(max(L1, 3), "Set1"), spreadColor, L2, range = .2))
    }
    else cols <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(7, "Set1"))(L1)


    if (inherits(x, "LDA")) {
      p <- ggplot2::ggplot(res, ggplot2::aes(date, prob, fill = topic))
    } else {
      p <- ggplot2::ggplot(res, ggplot2::aes(date, prob, fill = sentopic))
    }

    p <- p +
      ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::scale_fill_manual(values = cols,
                                 guide = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::scale_x_date(expand = c(0,0)) +
      ggplot2::theme_bw() +
      ggplot2::geom_line(data = lines, ggplot2::aes(date, y, group = L1),
                         inherit.aes = FALSE)
    print(p)
  }
  res$L1_prob <- NULL

  if (as.xts & length(missingSuggets("xts")) == 0) {
    if (inherits(x, "LDA")) {
      xts::as.xts(data.table::dcast(res, date ~ topic, value.var = "prob"))
    } else {
      xts::as.xts(data.table::dcast(res, date ~ sentopic, value.var = "prob"))
    }
  } else {
    res[order(date)]
  }
}
