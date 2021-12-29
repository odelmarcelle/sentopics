
generateVocab <- function(nTopics = 5, nSentiments = 3, nWords = 5, nCommonWords = 2, betaDirichlet = 0, hierarchy = c("rJST", "JST")) {
  if (betaDirichlet <= 0) FLAG <- TRUE else FLAG <- FALSE

  hierarchy <- match.arg(hierarchy)
  if (hierarchy == "rJST") reversed <- TRUE else reversed <- FALSE

  vocab <- vector()
  for (i in 1:nTopics) {
    topicVocab <- rep(paste0("topic", i), nWords * nSentiments)
    for (j in 1:nSentiments) {
      # print((i-1)*nWords*nSentiments+(1:nWords)+(nWords*(j-1)))
      topicVocab[(1:nWords) + (nWords * (j - 1))] <- paste0(topicVocab[(1:nWords) + (nWords * (j - 1))], "sent", j, "w", 1:nWords)
    }
    vocab <- c(vocab, topicVocab)
  }
  vocab

  if (nCommonWords > 0) {
    topicVocab <- as.vector(sapply(1:nTopics, function(x) paste0("topic", x, "w", 1:nCommonWords)))

    vocab <- c(vocab, topicVocab)

    topicVocab <- as.vector(sapply(1:nSentiments, function(x) paste0("sentiment", x, "w", 1:nCommonWords)))
    vocab <- c(vocab, topicVocab)

  }

  betaa <- array(0, dim = c(length(vocab), nTopics, nSentiments))
  for (i in 1:nTopics) {
    for (j in 1:nSentiments) {
      # print(((j-1)*nWords):(j*nWords))
      # print(((i-1)*nWords*nSentiments) + ((j-1)*nWords+1):(j*nWords))
      if (FLAG) {
        betaa[ c( ((i - 1) * nWords * nSentiments) + ((j - 1) * nWords + 1):(j * nWords),
                  seq(nTopics * nSentiments * nWords + 1 + (i - 1) * nCommonWords, length.out = nCommonWords),
                  seq(nTopics * nSentiments * nWords + nTopics * nCommonWords + 1 + (j - 1) * nCommonWords, length.out = nCommonWords))
               , i, j] <- rep(1/(nWords + nCommonWords * 2), nWords + nCommonWords * 2)
      } else {
        betaa[ c( ((i - 1) * nWords * nSentiments) + ((j - 1) * nWords + 1):(j * nWords),
                  seq(nTopics * nSentiments * nWords + 1 + (i - 1) * nCommonWords, length.out = nCommonWords),
                  seq(nTopics * nSentiments * nWords + nTopics * nCommonWords + 1 + (j - 1) * nCommonWords, length.out = nCommonWords))
               , i, j] <- rdirichlet(1, rep(betaDirichlet, nWords + nCommonWords * 2))
      }
    }
    # betaa[(1:(nWords*nSentiments))+(nWords*nSentiments*(i-1)),i,] <- MCMCpack::rdirichlet(1,rep(1,nWords*nSentiments))
  }

  # nTopics*nSentiments

  dimnames(betaa) <- list(word = vocab, topics = paste0("topic", 1:nTopics), sent = paste0("sent", 1:nSentiments))
  colSums(betaa)

  rowSums(betaa, dims = 2) ## topic wise
  rowSums(aperm(betaa, c(1, 3, 2)), dims = 2) ## sentiment wise

  if (reversed) betaa <- aperm(betaa, c(1,3,2))

  betaa
}

generateDocuments <- function(vocab, nDocs = 100, L1prior = 1, L2prior = 5, nWords = 400, nClass = 1) {
  if (length(L1prior) == 1) {
    L1priorMatrix <- matrix(0, dim(vocab)[3], nClass)
    for (c in 1:nClass) {
      L1priorMatrix[, c] <- rep(L1prior, dim(vocab)[3])
    }
  } else if (length(L1prior) == nClass) {
    L1priorMatrix <- matrix(0, dim(vocab)[3], nClass)
    for (c in 1:nClass) {
      L1priorMatrix[, c] <- rep(L1prior[c], dim(vocab)[3])
    }
  } else if (length(L1prior) == nClass * dim(vocab)[3]) {
    L1priorMatrix <- L1prior
  } else {
    stop("Please provide a valid L1prior: either an unique value, an unique value per class, or a complete matrix T x C")
  }

  if (length(L2prior) == 1) {
    L2priorArray <- array(0, dim = c(dim(vocab)[3:2], nClass))
    for (c in 1:nClass) {
      L2priorArray[, , c] <- rep(L2prior, dim(vocab)[3] * dim(vocab)[2])
    }
  } else if (length(L2prior) == nClass) {
    L2priorArray <- array(0, dim = c(dim(vocab)[3:2], nClass))
    for (c in 1:nClass) {
      L2priorArray[, , c] <- rep(L2prior[c], dim(vocab)[3] * dim(vocab)[2])
    }
  } else if (length(L2prior) == nClass * dim(vocab)[3] * dim(vocab)[2]) {
    L2priorArray <- array(L2prior, dim = c(dim(vocab)[3:2], nClass))
  } else {
    stop("Please provide a valid L2prior: either an unique value, an unique value per class, or a complete array T x S x C")
  }

  docs <- vector(mode = "list", nDocs)
  docLengths <- ceiling(stats::rnorm(nDocs, nWords, sd = nWords/5)) ## Vary document length
  docLengths[docLengths < 1] <- 1
  classVector <- rep(0, nDocs)
  pi <- matrix(0, dim(vocab)[3], dim(vocab)[2])
  S <- dim(vocab)[2]
  V <- dimnames(vocab)[[1]]
  for (i in 1:nDocs) {
    c <- sample(1:nClass, 1)
    classVector[i] <- c

    theta <- rdirichlet(1, L1priorMatrix[, c])
    pi <- t(sapply(1:dim(vocab)[3], function(x) rdirichlet(1, L2priorArray[x, , c])))
    dim(pi) <- c(dim(vocab)[3:2]) ## otherwise error when S = 1
    # pi <- MCMCpack::rdirichlet(length(theta),L2prior)    ### error generation
    docs[[i]] <- vector("integer", docLengths[i])
    z <- sample.int(dim(vocab)[3], docLengths[i], prob = theta, replace = TRUE)
    for (j in 1:docLengths[i]) {
      # z <- sample(1:dim(vocab)[2], 1, prob = theta)
      zj <- z[j]
      l <- sample.int(S, 1L, prob = pi[zj, ], replace = TRUE)
      docs[[i]][j] <- sample(V, 1L, prob = vocab[, l, zj], replace = TRUE)
    }
  }
  docs <- quanteda::as.tokens(docs)
  # docs <- sapply(docs, paste0, collapse = " ")
  # Encoding(docs) <- "UTF-8"
  quanteda::docvars(docs, "class") <- classVector
  docs
}


generatePartialLexicon <- function(toks, Sindex = c(1,3)) {

  vocab <- data.table::data.table(word = c(attr(toks, "types")))
  lex <- vocab[grep(paste0("sentiment[",paste0(Sindex, collapse = ""),"]"), word)]
  lex$value <- 1
  lex[grep("sentiment1", word), "value" := -1][]
  if (2 %in% Sindex & 3 %in% Sindex) lex[grep("sentiment2", word), "value" := 0][]

  quanteda::dictionary(list("negative" = lex[value == -1, word],
                            "positive" = lex[value == 1, word]))
}


distToGenerative <- function(x, generativeVocabulary, plot = FALSE, full = FALSE, method = "invariantEuclidean") {
  if (!inherits(x, "multiChains")) stop("Only valid for multiChains objects.")
  if (nrow(x[[1]]$vocabulary) < nrow(generativeVocabulary)) stop("Not all words of the generative vocabulary are present in the generated text.")

  x[[length(x) + 1]] <- x[[1]]
  x[[length(x)]]$phi <- generativeVocabulary[order(match(rownames(generativeVocabulary),x$vocabulary$word)),,,drop = FALSE]

  attr(x, "nChains") <- length(x)
  d <- chainsDistances(x, method)
  if (plot) {
    coord <- stats::cmdscale(stats::as.dist(d))
    plot(coord[, 1], coord[, 2], type = "n", xlab = "Coordinate 1", ylab = "Coordinate 2", main = paste0("Distance to true vocabulary"))
    graphics::text(coord[-attr(x, "nChains"), 1], coord[-attr(x, "nChains"), 2], rownames(coord), cex = 0.8)

    graphics::points(coord[attr(x, "nChains"), 1], coord[attr(x, "nChains"), 2], cex = 2, pch = 1, lwd = 4, col = "gray55")
    graphics::abline(v = coord[attr(x, "nChains"), 1], h = coord[attr(x, "nChains"), 2], col = "gray75")
  } else {
    if (full) {
      d
    } else {
      d[nrow(d), -nrow(d)]
    }
  }
}

#' Generate vocabulary from list of words
#'
#' @param list list of words. First list level determine the topic. Second list
#'   level indicate the sentiment
#'
#' @return an array usable in `generateDocuments`
#'
#' @examples
#' sentopics:::vocabFromList(list(
#'   list(c("tasty", "delicious"), c("bad", "smelly")),
#'   list(c("surprising", "amazing"), c("boring", "annoying")),
#'   list("fearless", "coward")
#' ))
#' sentopics:::vocabFromList(lapply(1:3, function(i) c(paste0("topic",i,"w1"))))
#' @keywords internal
vocabFromList <- function(list) {

  index <- unique(unlist(list, use.names = FALSE))
  n <- length(index)
  S <- sum(sapply(list, is.list))
  if (any(sapply(list, is.list))) {
    S <- max(sapply(list, function(x) {
      ifelse(is.list(x), length(x), 1)
    }))
  } else S = 0
  if (S == 0) S <- 1
  K <- length(list)

  vocab <- array(0, dim = c(n, S, K))
  rownames(vocab) <- index
  if (S == 1) {
    for (i in seq_along(list)) {
      match <- index %in% list[[i]]
      vocab[match, 1, i] <- 1/sum(match)
    }
  } else {
    list <- unlist(list, recursive = FALSE, use.names = FALSE)
    dim(list) <- c(S, K)
    for (i in 1:K) {
      for (j in 1:S) {
        match <- index %in% list[[j, i]]
        vocab[match, j, i] <- 1/sum(match)
      }
    }
  }

  dimnames(vocab) <- list(word = index,
                          sent = paste0("sent", 1:S),
                          topics = paste0("topic", 1:K))

  vocab
}
