# print -------------------------------------------------------------------

sentopics_print_extend <- function(extended = FALSE) {
  methods = c("fit", "topics", "top_words", "plot")
  explain <- c(
    "Estimate the model using Gibbs sampling",
    "Return the most important topic of each document",
    "Return a data.table with the top words of each topic/sentiment",
    "Plot a sunburst chart representing the estimated mixtures"
  )
  cat("------------------Useful methods------------------\n")
  cat(sprintf("%-10s:%s", methods, explain), sep = "\n")
  if (!extended) {
    cat(
      "This message is displayed once per session, unless calling `print(x, extended = TRUE)`\n"
    )
  }
}

#' Print method for sentopics models
#'
#' @description Print methods for **sentopics** models. Once per session (or
#'   forced by using `extended = TRUE`), it lists the most important function
#'   related to **sentopics** models.
#'
#' @rdname print.sentopicmodel
#'
#' @param x the model to be printed
#' @param extended if `TRUE`, extends the print to include some helpful related
#'   functions. Automatically displayed once per session.
#' @param ... not used
#'
#' @return No return value, called for side effects (printing).
#'
#' @export
print.sentopicmodel <- function(x, extended = FALSE, ...) {
  cat(
    "A sentopicmodel topic model with",
    x$L1,
    "topics and",
    x$L2,
    "sentiments. Currently fitted by",
    x$it,
    "Gibbs sampling iterations.\n"
  )
  if (getOption("sentopics_print_extended", TRUE) | extended) {
    sentopics_print_extend(extended)
    options(sentopics_print_extended = FALSE)
  }
  ##TODO: add models paremeters? ex: # cat("Model parameters: alpha = ")
}

#' @rdname print.sentopicmodel
#' @export
print.rJST <- function(x, extended = FALSE, ...) {
  cat(
    "A reverse-JST model with",
    x$K,
    "topics and",
    x$S,
    "sentiments. Currently fitted by",
    x$it,
    "Gibbs sampling iterations.\n"
  )
  if (getOption("sentopics_print_extended", TRUE) | extended) {
    sentopics_print_extend(extended)
    options(sentopics_print_extended = FALSE)
  }
}

#' @rdname print.sentopicmodel
#' @export
print.LDA <- function(x, extended = FALSE, ...) {
  cat(
    "An LDA model with",
    x$K,
    "topics. Currently fitted by",
    x$it,
    "Gibbs sampling iterations.\n"
  )
  if (getOption("sentopics_print_extended", TRUE) | extended) {
    sentopics_print_extend(extended)
    options(sentopics_print_extended = FALSE)
  }
}

#' @rdname print.sentopicmodel
#' @export
print.JST <- function(x, extended = FALSE, ...) {
  cat(
    "A JST model with",
    x$S,
    "sentiments and",
    x$K,
    "topics. Currently fitted by",
    x$it,
    "Gibbs sampling iterations.\n"
  )
  if (getOption("sentopics_print_extended", TRUE) | extended) {
    sentopics_print_extend(extended)
    options(sentopics_print_extended = FALSE)
  }
}

#' @export
print.multi_chains <- function(x, ...) {
  x <- lapply(x, identity)
  NextMethod()
}

#' @export
print.top_words <- function(x, ...) {
  if (!is.null(attr(x, "method"))) {
    colnames(x)[colnames(x) == "value"] <-
      paste0("value[", attr(x, "method"), "]")
  }
  NextMethod()
}

# plot --------------------------------------------------------------------

#' Plot a topic model using Plotly
#'
#' @description Summarize and plot a **sentopics** model using a sunburst chart
#'   from the [plotly::plotly] library.
#'
#' @param x a model created from the [LDA()], [JST()] or [rJST()] function and
#'   estimated with [fit()]
#' @param nWords the number of words per topic/sentiment to display in the outer
#'   layer of the plot
#' @param layers specifies the number of layers for the sunburst chart. This
#'   will restrict the output to the `layers` uppermost levels of the chart. For
#'   example, setting `layers = 1` will only display the top level of the
#'   hierarchy (topics for an LDA model).
#' @param sort if `TRUE`, sorts the plotted topics in a decreasing frequency.
#' @param ... not used
#'
#' @return A `plotly` sunburst chart.
#'
#' @export
#' @seealso [top_words()] [LDAvis()]
#' @examples
#' lda <- LDA(ECB_press_conferences_tokens)
#' lda <- fit(lda, 100)
#' plot(lda, nWords = 5)
#'
#' # only displays the topic proportions
#' plot(lda, layers = 1)
## TODO: add other methods than probability?
plot.sentopicmodel <- function(x, nWords = 15, layers = 3, sort = FALSE, ...) {
  mis <- missingSuggets(c("plotly", "RColorBrewer"))
  if (length(mis) > 0) {
    stop(
      "Suggested packages are missing for the plot.sentopicmodel function.\n",
      "Please install first the following packages: ",
      paste0(mis, collapse = ", "),
      ".\n",
      "Install command: install.packages(",
      paste0("'", mis, "'", collapse = ", "),
      ")"
    )
  }

  class <- class(x)[1]
  x <- as.sentopicmodel(x)
  real <- prob <- L1 <- L2 <- id <- value <- parent <- color <- NULL # due to NSE notes in R CMD check
  l1 <- l2 <- l3 <- NULL ## in case they exist in environment
  mixtureStats <- melt(x)
  L1_name <- colnames(mixtureStats)[2]
  L2_name <- colnames(mixtureStats)[1]
  # if (topicsOnly) method <- "topics" else method <- "probability"

  if (layers > 0) {
    l1 <- mixtureStats[,
      list(value = sum(prob) / length(x$tokens)),
      by = L1_name
    ]
    # l1$name <- l1[[L1_name]]
    l1$name <- create_labels(x, class, flat = FALSE)[["L1"]]
    l1$parent <- ""
    l1$id <- paste0("l1_", as.numeric(l1[[L1_name]]))
    if (x$L1 < 10) {
      if (x$L1 < 3) {
        l1$color <- RColorBrewer::brewer.pal(3, "Set1")[seq(nrow(l1))]
      } else {
        l1$color <- RColorBrewer::brewer.pal(nrow(l1), "Set1")
      }
    } else {
      l1$color <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(7, "Set1")
      )(x$L1)
    }

    l1$real <- l1$value
  }
  if (layers > 1) {
    l2 <- mixtureStats[,
      list(value = sum(prob) / length(x$tokens)),
      by = c(L1_name, L2_name)
    ]
    l2$name <- l2[[L2_name]]
    l2$parent <- paste0("l1_", as.numeric(l2[[L1_name]]))
    l2$id <- paste0(l2$parent, paste0("l2_", as.numeric(l2[[L2_name]])))
    l2$real <- l2$value
    ## Not working. Neet update
    # if (topicsOnly) l2 <- l2[, list(value = sum(value)), by = list(L1)][, id := paste0("l1_", L1, "l2_1")]
    for (i in l1[[L1_name]]) {
      cols <- spreadColor(l1[l1[[L1_name]] == i, color], x$L2)
      l2[l2[[L1_name]] == i, color := cols]
      l2[l2[[L1_name]] == i, real := (real / l1[l1[[L1_name]] == i, real])]
    }
  }
  if (layers > 2 | (layers > 1 & class == "LDA")) {
    l3 <- top_words_dt(x, nWords, "probability")
    l3$name <- l3$word
    l3$parent <- paste0("l1_", l3$L1, "l2_", l3$L2)
    l3$id <- paste0(l3$parent, l3$name)
    l3$real <- l3$value
    ## need to rescale l3 to sum up to l2. multiplying by 0.9 for a small space between groups
    ## It's okay as real is used to show the accurate probabilities
    for (i in unique(l3$parent)) {
      sub_l2 <- l2[id == i, value]
      sub_l3 <- l3[parent == i, value]

      l3[parent == i, "value"] <- (sub_l2 / sum(sub_l3) * 0.9) * sub_l3
      l3[parent == i, "color" := l2[id == i, "color"]]
    }
  }

  LIST <- list(l1[, -1], l2[, -(1:2)], l3[, -(1:3)])
  data <- data.table::rbindlist(LIST, use.names = TRUE)

  if (class == "LDA") {
    data <- data[!grepl("^l1_[0-9]+$", parent)]
    data$parent <- sub("l2_[0-9]+$", "", data$parent)
  }

  # reorder data clockwise
  if (sort) {
    data <- data[order(value)]
  } else {
    data <- data[.N:1, ]
  }

  fig <- plotly::plot_ly(
    ids = data$id,
    labels = data$name,
    parents = data$parent,
    values = data$value,
    type = "sunburst",
    rotation = "180",
    branchvalues = "total",
    leaf = list(opacity = 1),
    marker = list(colors = data$color),
    sort = FALSE,
    hovertemplate = "%{label}\n%{customdata:.2%}<extra></extra>",
    customdata = data$real,
    hoverlabel = list(font = list(size = 20))
  )

  fig
}


#' Plot the distances between topic models (chains)
#'
#' @description Plot the results of `chainsDistance(x)` using multidimensional
#'   scaling. See [chains_distances()] for details on the distance computation
#'   and [stats::cmdscale()] for the implementation of the multidimensional
#'   scaling.
#'
#' @inheritParams chains_distances
#'
#' @param ... not used
#' @seealso [chains_distances()] [cmdscale()]
#'
#' @return Invisibly, the coordinates of each topic model resulting from the
#'   multidimensional scaling.
#'
#' @examples
#' models <- LDA(ECB_press_conferences_tokens)
#' models <- fit(models, 10, nChains = 5)
#' plot(models)
#' @export
plot.multi_chains <- function(
  x,
  ...,
  method = c(
    "euclidean",
    "hellinger",
    "cosine",
    "minMax",
    "naiveEuclidean",
    "invariantEuclidean"
  )
) {
  if (attr(x, "nChains") < 3) {
    stop("At least 3 chains are required for proper plotting.")
  }
  method <- match.arg(method)
  d <- stats::as.dist(chains_distances(x, method))
  coord <- stats::cmdscale(d)
  ## Possible ggplot2 way of doing it
  # local({
  #   coord <- data.table::as.data.table(coord)
  #   coord$maxLik <- sapply(x, function(xx) max(xx$logLikelihood))
  #   coord$lastLik <- sapply(x, function(xx) tail(xx$logLikelihood, 1))
  #   coord
  #   print(ggplot(coord, aes(x = V1, y = V2, colour = lastLik)) + geom_point(size = 4))
  # })
  plot(
    coord[, 1],
    coord[, 2],
    type = "n",
    xlab = "Coordinate 1",
    ylab = "Coordinate 2",
    main = paste0("Chains multidimensional scaling of ", deparse(substitute(x)))
  )
  graphics::text(coord[, 1], coord[, 2], rownames(coord), cex = 0.8)
  graphics::abline(h = 0, v = 0, col = "gray75")
  invisible(coord)
}

# fit --------------------------------------------------------------------

#' @importFrom generics fit
#' @export
generics::fit

#' @name fit.sentopicmodel
#' @aliases grow
#' @title Estimate a topic model
#'
#' @description This function is used to estimate a topic model created by
#'   [LDA()], [JST()] or [rJST()]. In essence, this function iterates a Gibbs
#'   sampler MCMC.
#'
#' @param object a model created with the [LDA()], [JST()] or [rJST()] function.
#' @param iterations the number of iterations by which the model should be
#'   fitted.
#' @param nChains if set above 1, the model will be fitted multiple times
#'   from various starting positions. Latent variables will be re-initialized if
#'   `object` has not been fitted before.
#' @param displayProgress if `TRUE`, a progress bar will be displayed indicating
#'   the progress of the computation. When `nChains` is greater than 1, this
#'   requires the package \pkg{progressr} and optionally \pkg{progress}.
#' @param computeLikelihood if set to `FALSE`, does not compute the likelihood
#'   at each iteration. This can slightly decrease the computing time.
#' @param seed for reproducibility, a seed can be provided.
#' @param ... arguments passed to other methods. Not used.
#'
#' @return a `sentopicmodel` of the relevant model class if `nChains` is
#'   unspecified or equal to 1. A `multi_chains` object if `nChains` is greater
#'   than 1.
#'
#' @section Parallelism: When `nChains > 1`, the function can take advantage of
#'   [future.apply::future_lapply] (if installed) to spread the computation over
#'   multiple processes. This requires the specification of a parallel strategy
#'   using [future::plan()]. See the examples below.
#'
#' @seealso [LDA()], [JST()], [rJST()], [reset()]
#' @export
#' @examples
#' model <- rJST(ECB_press_conferences_tokens)
#' fit(model, 10)
#'
#' # -- Parallel computation --
#' require(future.apply)
#' future::plan("multisession", workers = 2) # Set up 2 workers
#' fit(model, 10, nChains = 2)
#'
#' future::plan("sequential") # Shut down workers
NULL

#' @rdname fit.sentopicmodel
#' @export
#' @usage NULL
fit.LDA <- function(
  object,
  iterations = 100,
  nChains = 1,
  displayProgress = TRUE,
  computeLikelihood = TRUE,
  seed = NULL,
  ...
) {
  if (isTRUE(attr(object, "approx"))) {
    stop("Not possible for approximated models")
  }
  as.LDA(fit(
    as.sentopicmodel(object),
    iterations = iterations,
    nChains = nChains,
    displayProgress = displayProgress,
    computeLikelihood = computeLikelihood,
    seed = seed
  ))
}

#' @rdname fit.sentopicmodel
#' @export
#' @usage NULL
fit.rJST <- function(
  object,
  iterations = 100,
  nChains = 1,
  displayProgress = TRUE,
  computeLikelihood = TRUE,
  seed = NULL,
  ...
) {
  as.rJST(fit(
    as.sentopicmodel(object),
    iterations = iterations,
    nChains = nChains,
    displayProgress = displayProgress,
    computeLikelihood = computeLikelihood,
    seed = seed
  ))
}

#' @rdname fit.sentopicmodel
#' @export
#' @usage NULL
fit.JST <- function(
  object,
  iterations = 100,
  nChains = 1,
  displayProgress = TRUE,
  computeLikelihood = TRUE,
  seed = NULL,
  ...
) {
  as.JST(fit(
    as.sentopicmodel(object),
    iterations = iterations,
    nChains = nChains,
    displayProgress = displayProgress,
    computeLikelihood = computeLikelihood,
    seed = seed
  ))
}

#' @rdname fit.sentopicmodel
#' @export
fit.sentopicmodel <- function(
  object,
  iterations = 100,
  nChains = 1,
  displayProgress = TRUE,
  computeLikelihood = TRUE,
  seed = NULL,
  ...
) {
  start_time <- Sys.time()

  ## force deep copy
  object <- data.table::copy(object)

  if (nChains == 1) {
    if (!is.null(seed)) {
      set.seed(seed * (object$it + 1))
    }
    ## rebuild c++ model
    base <- core(object) ## need to protect base$cleaned from gc
    cpp_model <- rebuild_cppModel(object, base)
    cpp_model$iterate(iterations, displayProgress, computeLikelihood)
    tmp <- extract_cppModel(cpp_model, base)
    object[names(tmp)] <- tmp
    reorder_sentopicmodel(object)
  } else if (nChains > 1) {
    ## CMD check
    chains <- NULL

    base <- core(object)
    core(object) <- NULL

    if (!is.null(seed)) {
      seed <- seed * (object$it + 1)
    }

    FUN <- function(i) {
      report_time <- Sys.time()
      report_processed <- 0L

      ## need to duplicate memory location of x$za. Otherwise, all chains
      ## share the same memory location
      object$za <- data.table::copy(object$za)
      rebuild_cppModel <- get(
        "rebuild_cppModel",
        envir = getNamespace("sentopics")
      )
      cpp_model <- rebuild_cppModel(object, base)
      ## generate different initial assignment for each chain
      if (object$it == 0 & i > 1) {
        cpp_model$initAssignments()
      }
      if (iterations > 0) {
        for (chunk in c(
          rep(chunkProgress, iterations %/% chunkProgress),
          iterations %% chunkProgress
        )) {
          cpp_model$iterate(chunk, FALSE, computeLikelihood)

          report_processed <- report_processed + chunk
          if ((Sys.time() - report_time) > 1) {
            # not too often
            difftime <- difftime(Sys.time(), start_time)
            p(
              amount = report_processed,
              message = sprintf("Elapsed: %.2f %s", difftime, units(difftime))
            )
            report_time <- Sys.time()
            report_processed <- 0L
          }
        }
      }

      if (report_processed > 0L) {
        # if remaining update
        difftime <- difftime(Sys.time(), start_time)
        p(
          amount = report_processed,
          message = sprintf("Elapsed: %.2f %s", difftime, units(difftime))
        )
      }

      extract_cppModel <- get(
        "extract_cppModel",
        envir = getNamespace("sentopics")
      )
      tmp <- extract_cppModel(cpp_model, base)
      object[names(tmp)] <- tmp
      object
    }

    expr <- quote({
      if (requireNamespace("future.apply", quietly = TRUE)) {
        if (is.null(seed)) {
          seed <- TRUE
        }
        environment(FUN) <- globalenv()
        chains <- future.apply::future_lapply(
          1:nChains,
          FUN,
          future.seed = seed,
          future.globals = list(
            object = object,
            base = base,
            iterations = iterations,
            chunkProgress = chunkProgress,
            computeLikelihood = computeLikelihood,
            p = p,
            start_time = start_time
          )
        )
      } else {
        if (requireNamespace("future", quietly = TRUE)) {
          if (!inherits(class(future::plan()), "sequential")) {
            message(
              "It seems that a parallel setup is registered, but `future.apply` is not installed. Did you omit installing it? Proceeding sequential computation..."
            )
          }
        }
        ## TODO: align RNGs?
        if (!is.null(seed)) {
          set.seed(seed)
        }
        chains <- lapply(1:nChains, FUN)
      }

      names(chains) <- paste0("chain", 1:nChains)
      class(chains) <- "multi_chains"
      attr(chains, "nChains") <- nChains
      attr(chains, "base") <- base
      attr(chains, "containedClass") <- "sentopicmodel"
    })

    if (displayProgress & !requireNamespace("progressr", quietly = TRUE)) {
      message(
        "The `progressr` package is required to track progress of multiple chains."
      )
      displayProgress <- FALSE
    }

    if (displayProgress) {
      ## determine how often the progress bar is refreshed (too high refresh rate can negatively affect performance)
      # chunkProgress <- min(max((iterations * nChains) %/% 10, 10), 1000, iterations)
      chunkProgress <- min(100, iterations)
      progressr::with_progress(
        {
          p <- progressr::progressor(
            steps = nChains * iterations + 0.001,
            enable = displayProgress
          )
          eval(expr)
          difftime <- difftime(Sys.time(), start_time)
          p(
            message = sprintf(
              "Done! Elapsed: %.2f %s",
              difftime,
              units(difftime)
            ),
            amount = 0.001
          )
        },
        handlers = custom_handler()
      )
    } else {
      chunkProgress <- iterations
      p <- function(...) {}
      environment(p) <- globalenv()
      eval(expr)
    }

    chains
  } else {
    stop("Incorrect number of chains specified.")
  }
}

#' @rdname fit.sentopicmodel
#' @export
fit.multi_chains <- function(
  object,
  iterations = 100,
  nChains = NULL,
  displayProgress = TRUE,
  computeLikelihood = TRUE,
  seed = NULL,
  ...
) {
  start_time <- Sys.time()

  ## CMD check
  chains <- NULL

  nChains <- attr(object, "nChains")
  base <- attr(object, "base")
  containedClass <- attr(object, "containedClass")

  object <- unclass(object) ## unclass to prevent usage of `[[.multi_chains`
  object <- lapply(object, as.sentopicmodel) ## return to sentopicmodel objects

  ## erase posterior in each chain to limit memory transfers
  for (i in seq_along(object)) {
    object[[i]][["L1post"]] <- object[[i]][["L2post"]] <- object[[i]][[
      "phi"
    ]] <- NULL
  }

  if (!is.null(seed)) {
    seed <- seed * (object[[1]]$it + 1)
  }

  FUN <- function(object) {
    report_time <- Sys.time()
    report_processed <- 0L

    rebuild_cppModel <- get(
      "rebuild_cppModel",
      envir = getNamespace("sentopics")
    )
    cpp_model <- rebuild_cppModel(object, base)

    for (chunk in c(
      rep(chunkProgress, iterations %/% chunkProgress),
      iterations %% chunkProgress
    )) {
      cpp_model$iterate(chunk, FALSE, computeLikelihood)

      report_processed <- report_processed + chunk
      if ((Sys.time() - report_time) > 1) {
        # not too often
        difftime <- difftime(Sys.time(), start_time)
        p(
          amount = report_processed,
          message = sprintf("Elapsed: %.2f %s", difftime, units(difftime))
        )
        report_time <- Sys.time()
        report_processed <- 0L
      }
    }
    if (report_processed > 0L) {
      # if remaining update
      difftime <- difftime(Sys.time(), start_time)
      p(
        amount = report_processed,
        message = sprintf("Elapsed: %.2f %s", difftime, units(difftime))
      )
    }

    extract_cppModel <- get(
      "extract_cppModel",
      envir = getNamespace("sentopics")
    )
    tmp <- extract_cppModel(cpp_model, base)
    object[names(tmp)] <- tmp
    object
  }

  expr <- quote({
    if (requireNamespace("future.apply", quietly = TRUE)) {
      if (is.null(seed)) {
        seed <- TRUE
      }
      environment(FUN) <- globalenv()
      chains <- future.apply::future_lapply(
        object,
        FUN,
        future.seed = seed,
        future.globals = list(
          object = object,
          base = base,
          iterations = iterations,
          chunkProgress = chunkProgress,
          computeLikelihood = computeLikelihood,
          p = p,
          start_time = start_time
        )
      )
    } else {
      if (requireNamespace("future", quietly = TRUE)) {
        if (!inherits(class(future::plan()), "sequential")) {
          message(
            "It seems that a parallel setup is registered, but `future.apply` is not installed. Did you omit installing it? Proceeding sequential computation..."
          )
        }
      }
      ## TODO: align RNGs?
      if (!is.null(seed)) {
        set.seed(seed)
      }
      chains <- lapply(object, FUN)
    }

    class(chains) <- "multi_chains"
    attr(chains, "nChains") <- nChains
    attr(chains, "base") <- base
    attr(chains, "containedClass") <- containedClass
  })

  if (displayProgress & !requireNamespace("progressr", quietly = TRUE)) {
    message(
      "The `progressr` package is required to track progress of multiple chains."
    )
    displayProgress <- FALSE
  }

  if (displayProgress) {
    ## determine how often the progress bar is refreshed (too high refresh rate can negatively affect performance)
    # chunkProgress <- min(max((iterations * nChains) %/% 10, 10), 1000, iterations)
    chunkProgress <- min(100, iterations)
    progressr::with_progress(
      {
        p <- progressr::progressor(
          steps = nChains * iterations + 0.001,
          enable = displayProgress
        )
        eval(expr)
        difftime <- difftime(Sys.time(), start_time)
        p(
          message = sprintf(
            "Done! Elapsed: %.2f %s",
            difftime,
            units(difftime)
          ),
          amount = 0.001
        )
      },
      handlers = custom_handler()
    )
  } else {
    chunkProgress <- iterations
    p <- function(...) {}
    environment(p) <- globalenv()
    eval(expr)
  }

  chains
}

# Retain `grow` methods for compatibility

#' @rdname fit.sentopicmodel
#' @export
#' @usage NULL
grow <- function(
  object,
  iterations = 100,
  nChains = 1,
  displayProgress = TRUE,
  computeLikelihood = TRUE,
  seed = NULL,
  ...
) {
  UseMethod("grow")
}
#' @rdname fit.sentopicmodel
#' @export
#' @usage NULL
grow.LDA <- fit.LDA
#' @rdname fit.sentopicmodel
#' @export
#' @usage NULL
grow.rJST <- fit.rJST
#' @rdname fit.sentopicmodel
#' @export
#' @usage NULL
grow.JST <- fit.JST
#' @rdname fit.sentopicmodel
#' @export
#' @usage NULL
grow.sentopicmodel <- fit.sentopicmodel
#' @rdname fit.sentopicmodel
#' @export
#' @usage NULL
grow.multi_chains <- fit.multi_chains

#' Re-initialize a topic model
#'
#' @author Olivier Delmarcelle
#'
#' @description This function is used re-initialize  a topic model, as if it was
#'   created from [LDA()], [JST()] or another model. The re-initialized model
#'   retains its original parameter specification.
#'
#' @param object a model created from the [LDA()], [JST()] or [rJST()] function and
#'   estimated with [fit()]
#'
#' @return a `sentopicmodel` of the relevant model class, with the iteration count
#'   reset to zero and re-initialized assignment of latent variables.
#'
#' @seealso [fit()]
#' @export
#' @examples
#' model <- LDA(ECB_press_conferences_tokens)
#' model <- fit(model, 10)
#' reset(model)
reset <- function(object) {
  ## make copy or assignment are reset by reference
  object$za <- data.table::copy(object$za)

  class <- class(object)[1]
  object <- as.sentopicmodel(object)
  object$it <- 0
  object$logLikelihood <- NULL
  object$phi <- object$L2post <- object$L1post <- NULL

  base <- core(object) ## need to protect base$cleaned from gc
  cpp_model <- rebuild_cppModel(object, base)
  cpp_model$initAssignments()
  # object <- utils::modifyList(object, extract_cppModel(cpp_model, core(object)))
  tmp <- extract_cppModel(cpp_model, base)
  object[names(tmp)] <- tmp

  fun <- get(paste0("as.", class))
  fun(reorder_sentopicmodel(object))
}


# melt --------------------------------------------------------------------

#' Replacement generic for [data.table::melt()]
#'
#' As of the CRAN release of the 1.14.8 version of **data.table**, the
#' [data.table::melt()] function is not a generic. This function aims to
#' temporary provide a generic to this function, so that [melt.sentopicmodel()]
#' can be effectively dispatched when used. Expect this function to disappear
#' shortly after the release of **data.table** 1.14.9.
#'
#' @param data an object to melt
#' @param ... arguments passed to other methods
#'
#' @return An unkeyed `data.table` containing the molten data.
#'
#' @seealso [data.table::melt()], [melt.sentopicmodel()]
#' @export
melt <- function(data, ...) {
  UseMethod("melt")
}
#' @export
melt.default <- function(data, ...) {
  data.table::melt(data, ...)
}
# TODO: re-activate once data.table 1.14.9 is released.
# #' @importFrom data.table melt
# #' @export
# data.table::melt

#' Melt for sentopicmodels
#'
#' @author Olivier Delmarcelle
#'
#' @description This function extracts the estimated document mixtures from a
#'   topic model and returns them in a long [data.table::data.table] format.
#'
#' @param data a model created from the [LDA()], [JST()] or [rJST()] function
#'   and estimated with [fit()]
#' @param ... not used
#' @param include_docvars if `TRUE`, the melted result will also include the
#'   *docvars* stored in the [tokens] object provided at model initialization
#'
#' @seealso [top_words()] for extracting representative words,
#'   [data.table::melt()] and [data.table::dcast()]
#'
#' @return A [data.table::data.table] in the long format, where each line is the estimated
#'   proportion of a single topic/sentiment for a document. For JST and rJST
#'   models, the probability is also decomposed into 'L1' and 'L2' layers,
#'   representing the probability at each layer of the topic-sentiment
#'   hierarchy.
#'
#' @export
#' @examples
#' # only returns topic proportion for LDA models
#' lda <- LDA(ECB_press_conferences_tokens)
#' lda <- fit(lda, 10)
#' melt(lda)
#'
#' # includes sentiment for JST and rJST models
#' jst <- JST(ECB_press_conferences_tokens)
#' jst <- fit(jst, 10)
#' melt(jst)
melt.sentopicmodel <- function(data, ..., include_docvars = FALSE) {
  if (data$it <= 0) {
    stop("Nothing to melt. Estimate the model with fit() first.")
  }
  class <- class(data)[1]
  data <- as.sentopicmodel(data)
  .id <- topic <- sentiment <- prob <- L1_prob <- L2_prob <- NULL # due to NSE notes in R CMD check

  L1stats <- data.table::as.data.table(
    data$L1post,
    sorted = FALSE,
    keep.rownames = ".id"
  )
  L1_name <- names(dimnames(data$L1post))[2]
  L1stats <- melt(
    L1stats,
    id = ".id",
    variable.factor = TRUE,
    variable.name = L1_name,
    value.name = "L1_prob"
  )
  if (attr(data, "Sdim") == "L1") {
    stopifnot(identical(
      levels(L1stats[[L1_name]]),
      levels(data$vocabulary$lexicon)
    ))
  }

  if (class != "LDA") {
    L2stats <- data$L2post
    names(dimnames(L2stats))[3] <- ".id"
    L2_name <- names(dimnames(L2stats))[1]

    L2stats <- data.table::as.data.table(
      L2stats,
      sorted = FALSE,
      value.name = "L2_prob"
    )
    for (i in 1:2) {
      L2stats[[i]] <- factor(L2stats[[i]])
    }
    if (attr(data, "Sdim") == "L1") {
      stopifnot(identical(
        levels(L1stats[[L1_name]]),
        levels(L1stats[[L1_name]])
      ))
    }
    if (attr(data, "Sdim") == "L2") {
      stopifnot(identical(
        levels(L2stats[[L2_name]]),
        levels(data$vocabulary$lexicon)
      ))
    }

    mixtureStats <- L2stats[L1stats, on = c(".id", L1_name)]
    mixtureStats[, prob := L2_prob * L1_prob]
    mixtureStats$`sentopic` <- interaction(
      mixtureStats[[L1_name]],
      mixtureStats[[L2_name]],
      sep = "_",
      lex.order = TRUE
    )
    data.table::setcolorder(
      mixtureStats,
      c(colnames(mixtureStats)[1:2], "sentopic")
    )

    ## quick integrity check
    if (
      !all(
        isTRUE(all.equal(sum(L2stats$L2_prob), data$L1 * length(data$tokens))),
        isTRUE(all.equal(sum(L1stats$L1_prob), length(data$tokens))),
        isTRUE(all.equal(sum(mixtureStats$prob), length(data$tokens)))
      )
    ) {
      stop(
        "Issue found in the computation. Please verify that the input object is correct."
      )
    }
  } else {
    mixtureStats <- L1stats
    data.table::setnames(mixtureStats, "L1_prob", "prob")
    mixtureStats <- mixtureStats[, list(topic, .id, prob)]
  }

  if (include_docvars) {
    docvars <- quanteda::docvars(data$tokens)
    docvars$.id <- names(data$tokens)
    mixtureStats <- merge(mixtureStats, docvars, sort = FALSE)
    data.table::setcolorder(mixtureStats, c(colnames(mixtureStats)[2:4], ".id"))
  }
  mixtureStats[]
}

# Accessors ---------------------------------------------------------------

## TODO: structure() for attributes ie structure(NextMethod(), class="foo")?
#' @export
`[[.multi_chains` <- function(x, i, ...) {
  base <- attr(x, "base")
  containedClass <- attr(x, "containedClass")

  x <- NextMethod()
  core(x) <- base

  fun <- get(paste0("as.", containedClass))
  fun(reorder_sentopicmodel(x))
}

##TODO: add test for this?
#' @export
`$.multi_chains` <- function(x, name, ...) {
  if (!name %in% names(x)) {
    #### This wont work with "alpha" "gamma" because they are under sentopicmodel form
    x <- attr(x, "base")
    x <- NextMethod()
  } else {
    base <- attr(x, "base")
    containedClass <- attr(x, "containedClass")
    x <- NextMethod()
    core(x) <- base
    fun <- get(paste0("as.", containedClass))
    x <- fun(reorder_sentopicmodel(x))
  }

  x
}

#' @export
`[.multi_chains` <- function(x, i, ...) {
  base <- attr(x, "base")
  nChains <- length(i)
  rng <- attr(x, "rng")[i]
  containedClass <- attr(x, "containedClass")
  x <- NextMethod()

  attr(x, "base") <- base
  attr(x, "nChains") <- nChains
  attr(x, "rng") <- rng
  attr(x, "containedClass") <- containedClass
  # TODO : check attributes?

  class(x) <- "multi_chains"

  x
}

#' @export
`as.list.multi_chains` <- function(x, copy = TRUE, ...) {
  x <- unclass(x)
  for (i in seq_along(x)) {
    if (copy) {
      core(x[[i]]) <- data.table::copy(attr(x, "base"))
    } else {
      core(x[[i]]) <- attr(x, "base")
    }
  }

  fun <- get(paste0("as.", attr(x, "containedClass")))
  x[] <- lapply(x, function(xi) fun(reorder_sentopicmodel(xi)))
  attr(x, "base") <- NULL
  x

  # res <- list()
  # for (i in seq_along(x)) {
  #   res[[i]] <- data.table::copy(x[[i]])
  # }
  # names(res) <- names(x)
  # res
}


# quanteda ----------------------------------------------------------------

#' @importFrom quanteda docvars
#' @export
quanteda::docvars

#' @method docvars sentopicmodel
#' @export
docvars.sentopicmodel <- function(x, field = NULL) {
  quanteda::docvars(x$tokens)
}

#' @importFrom quanteda tokens
#' @export
quanteda::tokens
