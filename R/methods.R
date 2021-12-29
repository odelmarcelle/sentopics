
# print -------------------------------------------------------------------


### TODO: are print methods OK? see tools::.print.via.format
#' @export
print.sentopicmodel <- function(x, ...) {
  cat("A sentopicmodel topic model with", x$L1, "topics and", x$L2, "sentiments. Currently grown by",
      x$it, "Gibbs sampling iterations.\n")
  methods = c("grow", "topics", "topWords", "plot")
  explain <- c("Iterates the model using Gibbs sampling",
               "Return the most important topic of each document",
               "Return a data.table with the top words of each topic/sentiment",
               "A sunburst chart representing the estimated mixtures")
  cat("------------------Useful methods------------------\n")
  cat(sprintf("%-10s:%s", methods, explain), sep = "\n")
  ##TODO: add models paremeters? ex: # cat("Model parameters: alpha = ")
}

#' @export
print.rJST <- function(x, ...) {
  cat("A reversed-JST model with", x$K, "topics and", x$S, "sentiments. Currently grown by",
      x$it, "Gibbs sampling iterations.\n")
}

#' @export
print.LDA <- function(x, ...) {
  cat("A LDA model with", x$K, "topics. Currently grown by",
      x$it, "Gibbs sampling iterations.\n")
}

#' @export
print.JST <- function(x, ...) {
  cat("A JST model with", x$S, "sentiments and", x$K, "topics. Currently grown by",
      x$it, "Gibbs sampling iterations.\n")
}

#' @export
print.multiChains <- function(x, ...) {
  x <- c(x)
  NextMethod()
}

#' @export
print.topWords <- function(x, generativeVocabulary = NULL, ...) {
  if (!is.null(attr(x, "method"))) colnames(x)[colnames(x) == "value"] <- attr(x, "method")
  NextMethod()
}

# summary -----------------------------------------------------------------

#' @export
summary.sentopicmodel <- function(object, ...) {
  if (object$it > 0) {
    plot(object$logLikelihood, main = "Model convergence", ylab = "logLikelihood", xlab = "iterations")

    list(
      vocabulary = object$vocabulary,
      topicMixtures = data.table::as.data.table(object$theta, sorted = FALSE, keep.rownames = "id"),
      sentimentMixtures = data.table::as.data.table(object$pi, sorted = FALSE),
      wordMixtures = data.table::as.data.table(object$phi, sorted = FALSE)
    )
  } else {
    list(vocabulary = object$vocabulary)
  }
}

# plot --------------------------------------------------------------------

#' @export
plot.sentopicmodel <- function(x, layers = 3, nWords = 15, topicsOnly = FALSE, ...) {

  mis <- missingSuggets(c("plotly", "RColorBrewer"))
  if (length(mis) > 0) stop("Suggested packages are missing for the plot.sentopicmodel function.\n",
                            "Please install first the following packages: ",
                            paste0(mis, collapse = ", "),".\n",
                            "Install command: install.packages(",
                            paste0("'", mis, "'", collapse = ", "),")" )

  class <- class(x)[1]
  x <- as.sentopicmodel(x)
  real <- prob <- L1 <- L2 <- id <- value <- parent <- color <- NULL # due to NSE notes in R CMD check
  l1 <- l2 <- l3 <- NULL ## in case they exist in environment
  mixtureStats <- melt(x)
  L1_name <- colnames(mixtureStats)[2]
  L2_name <- colnames(mixtureStats)[1]
  if (topicsOnly) method <- "topics" else method <- "probability"

  if (layers > 0) {
    l1 <- mixtureStats[, list(value = sum(prob) / length(x$tokens)), by = L1_name]
    # l1$name <- l1[[L1_name]]
    l1$name <- create_labels(x, class, flat = FALSE)[["L1"]]
    l1$parent <- ""
    l1$id <- paste0("l1_", as.numeric(l1[[L1_name]]))
    if (x$L1 < 10) {
      if (x$L1 < 3) l1$color <- RColorBrewer::brewer.pal(3, "Set1")[seq(nrow(l1))]
      else l1$color <- RColorBrewer::brewer.pal(nrow(l1), "Set1")
    }
    else l1$color <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(7, "Set1"))(x$L1)

    l1$real <- l1$value
  }
  if (layers > 1) {
    l2 <- mixtureStats[, list(value = sum(prob) / length(x$tokens)), by = c(L1_name, L2_name)]
    l2$name <- l2[[L2_name]]
    l2$parent <- paste0("l1_", as.numeric(l2[[L1_name]]))
    l2$id <- paste0(l2$parent, paste0("l2_", as.numeric(l2[[L2_name]])))
    l2$real <- l2$value
    ## Not working. Neet update
    if (topicsOnly) l2 <- l2[, list(value = sum(value)), by = list(L1)][, id := paste0("l1_", L1, "l2_1")]
    for (i in l1[[L1_name]]) {
      cols <- spreadColor(l1[l1[[L1_name]] == i, color], x$L2)
      l2[l2[[L1_name]] == i, color := cols]
      l2[l2[[L1_name]] == i, real := (real / l1[l1[[L1_name]] == i, real])]
    }
  }
  if (layers > 2) {
    l3 <- topWords_dt(x, nWords, "probability")
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

  ## Relic from topicsOnly?
  # if (identical(unique(l3$L2), 1L)) {
  #   l1$id <- paste0(l1$id, "sent1")
  #   fig <- plotly::plot_ly(
  #     ids = unlist(sapply(list(l1, l3), "[[", "id")),
  #     labels = unlist(sapply(list(l1, l3), "[[", "name")),
  #     parents = unlist(sapply(list(l1, l3), "[[", "parent")),
  #     values = unlist(sapply(list(l1, l3), "[[", "value")),
  #     type = "sunburst",
  #     branchvalues = "total",
  #     leaf = list(opacity = 1)
  #   )
  # } else {

  LIST <- list(l1[, -1], l2[, -(1:2)], l3[, -(1:3)])
  data <- data.table::rbindlist(LIST, use.names = TRUE)

  if (class == "LDA") {
    data <- data[!grepl("^l1_[0-9]+$", parent)]
    data$parent <- sub("l2_[0-9]+$", "", data$parent)
  }
  labels <- data$name
  # if (class %in% c("rJST", "LDA")) labels <-
  #   sub("l1_", "topic", sub("l2_", "sent", labels, fixed = TRUE), fixed = TRUE)
  # if (class == "JST") labels <-
  #   sub("l1_", "sent", sub("l2_", "topic", labels, fixed = TRUE), fixed = TRUE)

    fig <- plotly::plot_ly(
      ids = data$id,
      labels = labels,
      parents = data$parent,
      values = data$value,
      type = "sunburst",
      branchvalues = "total",
      leaf = list(opacity = 1),
      marker = list(colors = data$color),
      sort = FALSE,
      hovertemplate = "%{label}\n%{customdata:.2%}<extra></extra>",
      customdata = data$real,
      hoverlabel = list(font = list(size = 20))
    )

    # fig <- plotly::layout(fig, sunburstcolorway = unlist(sapply(LIST, "[[", "color")))
  # }

  fig
}


plotRJST <- function(x, nWords = 5, title = NULL, ...) {
  freq <- rebuild_zw(x, array = TRUE)
  freq <- aperm(freq, c(3, 1, 2))
  overall <- rowSums(freq)
  freq <- data.table::as.data.table(freq, sort = FALSE)
  if (attr(x, "reversed")) {
    colnames(freq) <- c("word", "sentiment", "topic", "value")
  } else  {
    colnames(freq) <- c("word", "topic", "sentiment", "value")
  }
  freq$overall <- rep(overall, times = x$T * x$S)
  freq$word <- rep(x$vocabulary$word, times = x$T * x$S)
  # freq
  freq_sub <- freq[order(-value), utils::head(.SD, nWords), by = list(topic, sentiment)][order(topic, sentiment)]
  tmp <- sapply(1:x$T, function(t) {
    freq_sub[topic == t]
    freq[topic == t & word %in% freq_sub[topic == t, word]]
  }, simplify = FALSE)
  tmp <- Reduce(rbind, tmp)

  # ggplot(tmp2, aes(x = word, y = value, fill = factor(sentiment))) +
  #   ggplot2::geom_col(ggplot2::aes(y = overall),
  #                     # Need to restrict overall to a single pair of
  #                     # topic/sentiment, otherwise it will sum the overal
  #                     # frequency multiple times
  #                     tmp[topic == 1 & sentiment == 1],
  #                     show.legend = FALSE, fill = "grey", alpha = .8) +
  #   geom_col() +
  #   facet_wrap(vars(topic), scales = "free") +
  #   ggplot2::coord_flip()


  # ggplot(tmp2, aes(x = tidytext::reorder_within(word, value, topic), y = value, fill = factor(sentiment))) +
  #   # geom_col() +
  #   ggplot2::geom_col(ggplot2::aes(y = overall),
  #                     # Need to restrict overall to a single pair of
  #                     # topic/sentiment, otherwise it will sum the overal
  #                     # frequency multiple times
  #                     position = position_dodge2(padding = 0),
  #                     show.legend = FALSE, fill = "grey", alpha = .8) +
  #   geom_col() +
  #   facet_wrap(vars(topic), scales = "free") +
  #   ggplot2::coord_flip() +
  #   tidytext::scale_x_reordered()

  ## manually adjust the ordering of factors for ggplot2
  ## from tidytext::reorder_within
  new_x <- paste(tmp$word, tmp$topic, sep = "___")
  target_order <- freq_sub
  target_order$word <- paste(target_order$word, target_order$topic, sep = "___")
  ## Remove conflicts when one word is selected for more than one sentiment
  target_order <- target_order[, .SD[which.max(value)], by = word]
  target_order <- target_order[order(-sentiment, value)]
  new_x <- factor(new_x, levels = target_order$word)

  ggplot2::ggplot(tmp, ggplot2::aes(x = new_x, y = value,
                                     fill = factor(sentiment))) +
    ggplot2::geom_col(ggplot2::aes(y = overall),
                      # Need to restrict overall to a single pair of
                      # topic/sentiment, otherwise it will sum the overal
                      # frequency multiple times
                      position = ggplot2::position_dodge2(padding = 0),
                      show.legend = FALSE, fill = "grey", alpha = .8) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::facet_wrap(ggplot2::vars(topic), scales = "free") +
    ggplot2::coord_flip() +
    tidytext::scale_x_reordered()


  # ggplot(tmp2, aes(x = tidytext::reorder_within(tidytext::reorder_within(word, value, topic), sentiment, topic), y = value, fill = factor(sentiment))) +
  #   # geom_col() +
  #   # ggplot2::geom_col(ggplot2::aes(y = overall),
  #   #                   # Need to restrict overall to a single pair of
  #   #                   # topic/sentiment, otherwise it will sum the overal
  #   #                   # frequency multiple times
  #   #                   position = position_dodge2(padding = 0),
  #   #                   show.legend = FALSE, fill = "grey", alpha = .8) +
  #   geom_col() +
  #   facet_wrap(vars(topic), scales = "free") +
  #   ggplot2::coord_flip() +
  #   tidytext::scale_x_reordered()



}

#' Plot the distances between topic models (chains)
#'
#' @description Plot the results of `chainsDistance(x)` using multidimensional
#'   scaling. See [chainsDistances()] for details on the distance computation
#'   and [stats::cmdscale()] for the implementation of the multidimensional
#'   scaling.
#'
#' @inheritParams chainsDistances
#'
#' @param ... not used here
#' @seealso [chainsDistances()] [cmdscale()]
#'
#' @examples
#' model <- LDA(ECB_speeches)
#' model <- grow(model, 10, nChains = 5)
#' plot(model)
#'
#' @export
plot.multiChains <- function(x, ..., method = c("euclidean", "hellinger", "cosine", "minMax", "stupidEuclidean", "invariantEuclidean")) {
  if (attr(x, "nChains") < 3) stop("At least 3 chains are required for proper plotting.")
  method <- match.arg(method)
  d <- stats::as.dist(chainsDistances(x, method))
  coord <- stats::cmdscale(d)
  # coord <- data.table::as.data.table(coord)
  # coord$maxLik <- sapply(x, function(xx) max(xx$logLikelihood))
  # coord$lastLik <- sapply(x, function(xx) tail(xx$logLikelihood, 1))
  # coord
  # ggplot(coord, aes(x = V1, y = V2, colour = lastLik)) + geom_point(size = 4)
  plot(coord[, 1], coord[, 2], type = "n", xlab = "Coordinate 1", ylab = "Coordinate 2", main = paste0("Chains multidimensional scaling of ", deparse(substitute(x))))
  graphics::text(coord[, 1], coord[, 2], rownames(coord), cex = 0.8)
  graphics::abline(h = 0, v = 0, col = "gray75")
}

# grow --------------------------------------------------------------------
# TODO: branch on initLDA if S = 1... faster
#' Estimate a topic model
#'
#' @author Olivier Delmarcelle
#'
#' @description This function is used to estimate a topic model created by [LDA()], [JST()], [rJST()] or [sentopicmodel()].
#'   In essence, this function iterates a Gibbs sampler MCMC.
#'
#' @param x a `sentopicmodel` created from the [LDA()], [JST()], [rJST()] or [sentopicmodel()] function
#' @param iterations the number of iterations by which the model should be grown
#' @param nChains if set above 1, the model will be grown into multiple chains from various starting positions.
#'   Latent variables will be re-initialized if `x` has not been grown before.
#' @param nCores if `nChains` is greater than 1, this allows to compute the the multiple chains in a parallel setting
#' @param displayProgress if `TRUE`, a progress bar will be displayed indicating the progress of the computation.
#'   Disabled when `nChains` is greated than 1.
#' @param computeLikelihood bolean. If set to FALSE, does not compute the likelihood at each iteration.
#'   This can slightly decrease the computing time.
#' @param seed for reproductibility, a seed can be provided
#'
#' @return a `sentopicmodel` of relevant model class if `nChains` is unspeficied or equal to 1. A `multiChains` object
#'   if `nChains` is greater than 1.
#'
#' @seealso [LDA()], [JST()], [rJST()] [sentopicmodel()]
#' @export
#' @examples
#' model <- rJST(ECB_speeches)
#' grow(model, 10)
grow <- function(x, iterations = 100, nChains = 1, nCores = 1, displayProgress = TRUE, computeLikelihood = TRUE, seed = NULL) {
  UseMethod("grow")
}
#' @export
grow.LDA <- function(x, iterations = 100, nChains = 1, nCores = 1, displayProgress = TRUE, computeLikelihood = TRUE, seed = NULL) {
  as.LDA(grow(as.sentopicmodel(x),
            iterations = iterations,
            nChains = nChains,
            nCores = nCores,
            displayProgress = displayProgress,
            computeLikelihood = computeLikelihood,
            seed = seed))
}
#' @export
grow.rJST <- function(x, iterations = 100, nChains = 1, nCores = 1, displayProgress = TRUE, computeLikelihood = TRUE, seed = NULL) {
  as.rJST(grow(as.sentopicmodel(x),
              iterations = iterations,
              nChains = nChains,
              nCores = nCores,
              displayProgress = displayProgress,
              computeLikelihood = computeLikelihood,
              seed = seed))
}
#' @export
grow.JST <- function(x, iterations = 100, nChains = 1, nCores = 1, displayProgress = TRUE, computeLikelihood = TRUE, seed = NULL) {
  as.JST(grow(as.sentopicmodel(x),
              iterations = iterations,
              nChains = nChains,
              nCores = nCores,
              displayProgress = displayProgress,
              computeLikelihood = computeLikelihood,
              seed = seed))
}
## TODO: importFrom is temporary, because doRNG calls %dopar% in the current environment. ImportFrom is needed until fix.
#' @importFrom foreach `%dopar%`
#' @export
grow.sentopicmodel <- function(x, iterations = 100, nChains = 1, nCores = 1, displayProgress = TRUE, computeLikelihood = TRUE, seed = NULL) {

  ## historical variable.. to be removed
  burn = 0

  ## force deep copy
  x <- data.table::copy(x)
  # x <- generateZWD(x, FALSE)
  # x$beta <- switchBeta(x$beta, x$T, x$S)

  if (nChains == 1) {
    if (!is.null(seed)) set.seed(seed * (x$it + 1)) # else set.seed(sample.int(2^30, 1)) ### if set.seed is not defined internal rng of cpp_model repeat every call
    ## rebuild c++ model
    base <- core(x)
    cpp_model <- rebuild_cppModel(x, base)

    cpp_model$iterate(iterations + burn, displayProgress, computeLikelihood)

    # x <- utils::modifyList(x, extract_cppModel(cpp_model, core(x))) ## slow if large tokens
    ## replaced by:
    tmp <- extract_cppModel(cpp_model, base)
    x[names(tmp)] <- tmp

    # x <- generateWTSD(x)
    # x$beta <- switchBeta(x$beta, x$T, x$S)

    # x$theta <- computeTheta(x)
    # x$pi <- computePi(x)
    # x$phi <- computePhi(x)
    # x$logLikelihood <- x$logLikelihoodW + x$logLikelihoodT + x$logLikelihoodS

    reorder_sentopicmodel(x)
  } else if (nChains > 1) {

    base <- core(x)
    core(x) <- NULL

    ### maybe not the most proper way to do this but...
    options("doFuture.foreach.export" = ".export")

    ## multiple chain setup is built based on parallel computation, if no back-end it will behave sequentially
    doFuture::registerDoFuture()
    if (nCores > 1) {
      cl <- parallel::makeCluster(nCores)
      future::plan("cluster", workers = cl)
      # if (!is.null(seed)) doRNG::registerDoRNG(seed * (x$it + 1), FALSE)
    } else {
      future::plan("sequential")
      # if (!is.null(seed)) doRNG::registerDoRNG(seed * (x$it + 1), FALSE)
    }

    if (!is.null(seed)) seed <- seed * (x$it + 1)

    ## determine how often the progress bar is refreshed (too high refresh rate can negatively affect performance)
    chunkProgress <- min(max(((burn + iterations) * nChains/nCores) %/% 10, 10), 1000, iterations)

    # if (displayProgress) progressr::handlers("progress") else progressr::handlers("void")
    # progressr::with_progress({
      start_time <- Sys.time()
      ############################################ WARNING : exporting p or using progressr seems to cause memory leaks.... investigate ? run_MC as example
      # p <- progressr::progressor(steps = nChains * ((burn + iterations) * 1000 + 3))
      chains <- doRNG::`%dorng%`(foreach::foreach(i = 1:nChains, .packages = c("sentopics"),
                                                  .export = c("x", "burn", "iterations", "chunkProgress", "start_time", "base", "computeLikelihood"),
                                                  .final = function(x) stats::setNames(x, paste0("chain", 1:nChains)), .options.RNG = seed),
                                 {
        # p(sprintf("Starting chain %d", i), class = "sticky", amount = 1)

        ## need to duplicate memory location of x$za. Otherwise, all chains
        ## share the same memory location
        x$za <- data.table::copy(x$za)

        # message(paste(ls(environment(), all.names = TRUE), collapse = "  "))
        # # message(ls(...future.env, all.names = TRUE))
        # message(paste0(sapply(mget(ls(environment(), all.names = TRUE)), object.size), collapse = " "))
        #
        # message(paste(ls(parent.frame(), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(2), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(3), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(3), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(4), all.names = TRUE), collapse = "  "))
        # message(paste0(sapply(mget(ls(parent.frame(4), all.names = TRUE), envir = parent.frame(4)), object.size), collapse = " "))
        # message(paste(ls(parent.frame(5), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(6), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(7), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(8), all.names = TRUE), collapse = "  "))
        # message(paste0(sapply(mget(ls(parent.frame(8), all.names = TRUE), envir = parent.frame(8)), object.size), collapse = " "))
        # message(paste(ls(parent.frame(9), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(10), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(11), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(12), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(13), all.names = TRUE), collapse = "  "))
        # message(paste(ls(parent.frame(14), all.names = TRUE), collapse = "  "))
        # message(ls(...future.env, all.names = TRUE))
        # message(paste0(sapply(mget(ls(parent.frame(), all.names = TRUE)), object.size), collapse = " "))

        # if (x$it != 0) {
        #   cpp_model <- sentopics:::rebuild_cppModel(x)
        # } else {
        #   cpp_model <- methods::new(sentopics:::model)
        #   cpp_model$init(
        #     x$tokens, x$V, x$T, x$S, x$lexicon, x$classVector,
        #     x$initAlpha, x$initBeta, x$initGamma, x$alphaCycle, x$gammaCycle, x$sentDelay, 1, x$annealing, x$annealing_base
        #   )
        #   cpp_model$alpha <- x$alpha
        #   cpp_model$gamma <- x$gamma
        #   cpp_model$beta <- x$beta
        # }
        # message("debug1")
        # message(quanteda::docvars(base$tokens, "class"))
        cpp_model <- sentopics:::rebuild_cppModel(x, base)

        # message("debug2")
        # message(cpp_model$za[[1]][1:20])
        # message(.Random.seed[1:5])
        ## generate different initial assignment for each chain
        if (x$it == 0) cpp_model$initAssignments()

        # message(cpp_model$za[[1]][1:20])



        for (j in seq((burn + iterations) %/% chunkProgress)) {
          cpp_model$iterate(chunkProgress, FALSE, computeLikelihood)
          # p(
            # message = sprintf("Elapsed time: %g", round(as.numeric(Sys.time() - start_time, units = "secs"), 0)),
            amount = chunkProgress * 1000
          # )
        }
        if ((burn + iterations) %% chunkProgress != 0) {
          cpp_model$iterate((burn + iterations) %% chunkProgress, FALSE, computeLikelihood)
          # p(
            # message = sprintf("Elapsed time: %g", round(as.numeric(Sys.time() - start_time, units = "secs"), 0)), ### TODO: test difftime(Sys.time(), t1, units = "sec")
            amount = ((burn + iterations) %% chunkProgress) * 1000
          # )
        }

        # message("in cpp:", head(cpp_model$za[[1]]))

        ### TODO: re-implement message for burned chain ?
        # p(message = sprintf("Burned chain %g", i), amount  = 1)
        # p(message = sprintf("Processed chain %g", i), amount = 2)
        # x <- utils::modifyList(x, sentopics:::extract_cppModel(cpp_model, base))
        tmp <- sentopics:::extract_cppModel(cpp_model, base)
        x[names(tmp)] <- tmp

        # message("in R:", head(x$za[[1]]))

        # message("theta1:", head(x$theta[, 1]))
        # message("theta2:", head(sentopics:::computeTheta(x, base)[, 1]))
        # message("debug3")
        # x <- sentopics:::generateWTSD(x, base = base)
        # message("debug3.5")
        # base$beta <- sentopics:::switchBeta(base$beta, x$T, x$S) #### only for current worker... not well optimized overall
        # message("debug4")
        # x$theta <- sentopics:::computeTheta(x, base)
        # x$pi <- sentopics:::computePi(x, base)
        # x$phi <- sentopics:::computePhi(x, base)
        # # message("debug5")
        # x$logLikelihood <- x$logLikelihoodW + x$logLikelihoodT + x$logLikelihoodS
        #
        #
        # # message(paste(ls(environment(), all.names = TRUE), collapse = "  "))
        # # # message(ls(...future.env, all.names = TRUE))
        # # message(paste0(sapply(mget(ls(environment(), all.names = TRUE)), object.size), collapse = " "))
        #
        x
        # sentopics:::extract_cppModel(cpp_model, names(base$tokens))
      })
    # })
    if (nCores > 1) {
      future::plan("sequential")
      parallel::stopCluster(cl)
    }
    class(chains) <- "multiChains"
    attr(chains, "nChains") <- nChains
    # base$beta <- switchBeta(base$beta, x$T, x$S)
    attr(chains, "base") <- base
    attr(chains, "containedClass") <- "sentopicmodel"

    chains
  } else {
    stop("Incorrect number of chains specified.")
  }
}

### TODO: allow chain selection with nChains ?
#' @export
grow.multiChains <- function(x, iterations = 100, nChains = NULL, nCores = 1, displayProgress = TRUE, computeLikelihood = TRUE, seed = NULL) {

  ## historical variable.. to be removed
  burn = 0

  ### TODO: automated chain selection
  nChains <- attr(x, "nChains")
  base <- attr(x, "base")
  containedClass <- attr(x, "containedClass")
  # if (containedClass == "JST") { ## need to switch alpha and gamma in base
  #   index <- which(names(base) %in% c("alpha", "gamma"))
  #   names(base)[index] <- names(base)[rev(index)]
  # }

  x <- unclass(x) ## unclass to prevent usage of `[[.multiChains`
  # base$beta <- switchBeta(base$beta, x[[1]]$T, x[[1]]$S) ## to 2D

  x <- lapply(x, as.sentopicmodel) ## return to sentopicmodel objects

  ## erase posterior to limit memory transfers
  for (i in seq_along(x)) {
    x[[i]][["L1post"]] <- x[[i]][["L2post"]] <- x[[i]][["phi"]] <- NULL
  }
  #
  #
  ### maybe not the most proper way to do this but...
  options("doFuture.foreach.export" = ".export")


  names <- names(x)[1:nChains]
  doFuture::registerDoFuture()
  if (nCores > 1) {
    cl <- parallel::makeCluster(nCores)
    future::plan("cluster", workers = cl)
    # if (!is.null(seed)) doRNG::registerDoRNG(seed * (x[[1]]$it + 1), FALSE)
  } else {
    future::plan("sequential")
    # if (!is.null(seed)) doRNG::registerDoRNG(seed * (x[[1]]$it + 1), FALSE)
  }
  ## determine how often is the progress bar refreshed ## note that a refresh rate too high can negatively affect performance
  chunkProgress <- min(max(((burn + iterations)*nChains/nCores) %/% 10, 10), 1000, iterations)

  if (!is.null(seed)) seed <- seed * (x[[1]]$it + 1)

  # message(seed)

  # if (displayProgress) progressr::handlers("progress") else progressr::handlers("void")
  # progressr::with_progress({
    start_time <- Sys.time()
    p <- progressr::progressor(steps = nChains * ((burn + iterations) * 1000 + 3))
    chains <- doRNG::`%dorng%`(foreach::foreach(x = x, i = 1:nChains, .packages = c("sentopics"), .export = c("burn", "iterations", "chunkProgress", "start_time", "base", "computeLikelihood"),
                                                .final = function(x) stats::setNames(x, paste0("chain", 1:nChains)), .options.RNG = seed),
                               {
                                 # x <- data.table::copy(x)
       # p(sprintf("Starting chain %d", i), class = "sticky", amount = 1)
       # x <- x[[i]]
       cpp_model <- sentopics:::rebuild_cppModel(x, base)

       # message(.Random.seed)

       for (j in seq((burn + iterations) %/% chunkProgress)) {
         cpp_model$iterate(chunkProgress, FALSE, computeLikelihood)
         # p(
         #   message = sprintf("Elapsed time: %g", round(as.numeric(Sys.time() - start_time, units = "secs"), 0)),
         #   amount = chunkProgress * 1000
         # )
       }
       if ((burn + iterations) %% chunkProgress != 0) {
         cpp_model$iterate((burn + iterations) %% chunkProgress, FALSE, computeLikelihood)
         # p(
         #   message = sprintf("Elapsed time: %g", round(as.numeric(Sys.time() - start_time, units = "secs"), 0)), ### TODO: test difftime(Sys.time(), t1, units = 'sec')
         #   amount = ((burn + iterations) %% chunkProgress) * 1000
         # )
       }

       ### TODO: re-implement message for burned chain ?
       # p(message = sprintf("Burned chain %g", i), amount  = 1)
       # p(message = sprintf("Processed chain %g", i), amount = 2)
       # x <- utils::modifyList(x, sentopics:::extract_cppModel(cpp_model, base))
       tmp <- sentopics:::extract_cppModel(cpp_model, base)
       x[names(tmp)] <- tmp
       # x <- sentopics:::generateWTSD(x, base = base)
       # base$beta <- sentopics:::switchBeta(base$beta, x$T, x$S) #### only for current worker... not well optimized overall
       # x$theta <- sentopics:::computeTheta(x, base)
       # x$pi <- sentopics:::computePi(x, base)
       # x$phi <- sentopics:::computePhi(x, base)
       # x$logLikelihood <- x$logLikelihoodW + x$logLikelihoodT + x$logLikelihoodS

       x
     })
  # })
  if (nCores > 1) {
    future::plan("sequential")
    parallel::stopCluster(cl)
  }
  class(chains) <- "multiChains"
  attr(chains, "nChains") <- nChains
  # base$beta <- switchBeta(base$beta, x[[1]]$T, x[[1]]$S) ## back to 3D
  attr(chains, "base") <- base
  attr(chains, "containedClass") <- containedClass
  names(chains) <- names

  chains
}

# melt --------------------------------------------------------------------

#' Melt for sentopicmodel object
#'
#' @author Olivier Delmarcelle
#'
#' @description ...
#'
#' @param data ...
#' @param ... ...
#' @param na.rm ...
#' @param value.name ...
#'
#' @return ...
#'
#' @export
melt <- function(data, ..., na.rm = FALSE, value.name = "value") {
  if (inherits(data, "sentopicmodel")) {
    UseMethod("melt", data)
  } else {
    data.table::melt(data, ..., na.rm = na.rm, value.name = value.name)
  }
}
### TODO: adjust output for LDA/JST/rJST
#' @export
melt.sentopicmodel <- function(data, ..., include_docvars = FALSE) {
  if (data$it <= 0) stop("Nothing to melt. Iterate the model with grow() first.")
  class <- class(data)[1]
  data <- as.sentopicmodel(data)
  id <- topic <- sentiment <- L1_prob <- L2_prob <- NULL # due to NSE notes in R CMD check

  L1stats <- data.table::as.data.table(data$L1post, sorted = FALSE, keep.rownames = ".id")
  # colnames(L1stats) <- c(".id", 1:(ncol(L1stats) - 1))
  L1_name <- names(dimnames(data$L1post))[2]
  L1stats <- melt(L1stats, id = ".id", variable.factor = TRUE,
                  variable.name = L1_name, value.name = "L1_prob")
  if (attr(data, "Sdim") == "L1") stopifnot(identical(levels(L1stats[[L1_name]]), levels(data$vocabulary$lexicon)))
  # L1stats$L1 <- as.numeric(L1stats$L1)

  if (class != "LDA") {
    L2stats <- data$L2post
    # dimnames(L2stats)[1:2] <- list(NULL, NULL)
    names(dimnames(L2stats))[3] <- ".id"
    L2_name <- names(dimnames(L2stats))[1]

    L2stats <- data.table::as.data.table(L2stats, sorted = FALSE, value.name = "L2_prob")
    for (i in 1:2) {
      L2stats[[i]] <- factor(L2stats[[i]])
    }
    if (attr(data, "Sdim") == "L1") stopifnot(identical(levels(L1stats[[L1_name]]), levels(L1stats[[L1_name]])))
    if (attr(data, "Sdim") == "L2") stopifnot(identical(levels(L2stats[[L2_name]]), levels(data$vocabulary$lexicon)))

    # colnames(L2stats) <- c("L2", "L1", "id", "prob")

    mixtureStats <- L2stats[L1stats, on = c(".id", L1_name)]
    mixtureStats[, prob := L2_prob * L1_prob]
    mixtureStats$`sentopic` <- interaction(mixtureStats[[L1_name]], mixtureStats[[L2_name]], sep = "_", lex.order = TRUE)
    data.table::setcolorder(mixtureStats, c(colnames(mixtureStats)[1:2], "sentopic"))

    ## quick integrity check
    if (!all(isTRUE(all.equal(sum(L2stats$L2_prob), data$L1 * length(data$tokens))),
             isTRUE(all.equal(sum(L1stats$L1_prob), length(data$tokens))),
             isTRUE(all.equal(sum(mixtureStats$prob), length(data$tokens))))) {
      stop("Issue found in the computation. Please verify that the input object is correct.")
    }
  } else {
    mixtureStats <- L1stats
    data.table::setnames(mixtureStats, "L1_prob", "prob")
    mixtureStats <- mixtureStats[, list(topic, .id, prob)]
  }

  # mixtureStats <- mixtureStats[, list(id, L1, L2, prob = prob * i.prob, l2prob = prob, l1prob = i.prob)]

  if (include_docvars) {
    docvars <- quanteda::docvars(data$tokens)
    docvars$.id <- names(data$tokens)
    mixtureStats <- merge(mixtureStats, docvars, sort = FALSE)
    data.table::setcolorder(mixtureStats, c(colnames(mixtureStats)[2:4], ".id"))
  }



  mixtureStats[]
}

# Accessors ---------------------------------------------------------------

## structure() for attributes ie structure(NextMethod(), class="foo")

#' @export
`[[.multiChains` <- function(x, i, ...) {

  base <- attr(x, "base")
  containedClass <- attr(x, "containedClass")

  # ## coerce to sentopicmodel to manipulate with core(x)
  # x <- lapply(unclass(x), as.sentopicmodel)

  x <- NextMethod()
  core(x) <- base

  fun <- get(paste0("as.", containedClass))
  fun(reorder_sentopicmodel(x))
}

##TODO: add test for this?
#' @export
`$.multiChains` <- function(x, name, ...) {

  if (!name %in% names(x)) {
    #### This wont work with "alpha" "gamma" because they are under sentopicmodel form
    x <- attr(x, "base")
    x <- NextMethod()
  } else {
    base <- attr(x, "base")
    containedClass <- attr(x, "containedClass")
    # ## coerce to sentopicmodel to manipulate with core(x)
    # x <- lapply(unclass(x), as.sentopicmodel)
    x <- NextMethod()
    core(x) <- base
    fun <- get(paste0("as.", containedClass))
    x <- fun(reorder_sentopicmodel(x))
  }

  x
}

#' @export
`[.multiChains` <- function(x, i, ...) {

  base <- attr(x, "base")
  nChains <- length(i)
  rng <- attr(x, "rng")[i]
  containedClass <- attr(x, "containedClass")
  x <- NextMethod()

  attr(x, "base") <- base
  attr(x, "nChains") <- nChains
  attr(x, "rng") <- rng
  attr(x, "containedClass") <- containedClass
  # TODO : check attributes

  class(x) <- "multiChains"

  x
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


