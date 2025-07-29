
### TODO:: reword file to work with L1/L2


## Segment a word set into pairs to evaluate. Only used in C_V.
segmentation <- function(x, to = c("one", "all", "set")) {

  x <- x$index
  switch(to,
         one = {
           segments <- apply(expand.grid(x, x), MARGIN = 1, as.list)
           segments[sapply(segments, function(x) Reduce(identical, x))] <- NULL
           segments
         },
         all = {
           segments <- as.list(1:length(x))
           for (i in 1:length(x)) {
             segments[[i]] <- list(x[i], x[-i])
           }
           segments
         },
         set = {
           segments <- as.list(1:length(x))
           for (i in 1:length(x)) {
             segments[[i]] <- list(x[i], x)
           }
           segments
         },
         stop("Method not implemented"))
}

## Create virtual documents defined by the window size.
virtualDocuments <- function(itoks, window = 10) {
  vD <- itoks
  if (window == "boolean") return(vD)
  if (!is.numeric(window)) stop("Incorrect window argument. Please enter a valid number or the character \"boolean\".")
  if (any(quanteda::ntoken(vD) < window)) {
    warning("Some documents are shorter than the sliding window. Boolean count will be applied for those document instead.")
    short <- vD[quanteda::ntoken(vD) < window]
    vD <- vD[quanteda::ntoken(vD) >= window]
  }
  vD <- quanteda::tokens_chunk(vD, size = window, overlap = window - 1, use_docvars = FALSE)
  vD <- quanteda::tokens_subset(vD, quanteda::ntoken(vD) == window)

  if (exists("short", inherits = FALSE)) vD <- vD + short
  vD
}


## Computation of NPMI without using epsilon smoothing
NPMI <- function(x, epsilon = 0){
  if (dim(x)[1] != dim(x)[2]) stop("Provide a square matrix of co-occurence probabilities")

  ## naming dimnames to match default behavior of which( arr.ind = TRUE)
  names(dimnames(x)) <- c("row", "col")
  ## apply smoothing if required
  x <- x + epsilon

  toMinusOne <- which(x == 0)
  toOne <- which(x == 1L)

  # divide by p(w1)
  NPMIs <- x / diag(x)
  # divide by p(w2)
  NPMIs <- t(NPMIs) / diag(x)

  NPMIs <- log(NPMIs) / -log(x)

  NPMIs[toMinusOne] <- -1
  NPMIs[toOne] <- 1

  if (any(is.na(NPMIs))) {
    stop("Unexpected NA. Need debug")
    # # Manually correcting NaN created by log(0)
    # index <- which(is.na(NPMIs), arr.ind = TRUE)
    # for (i in 1:nrow(index)) {
    #   if (x[index[i, "row"], index[i, "col"]] == 1) {         # if p(w1, w2) = 1
    #     NPMIs[index[i, "row"], index[i, "col"]] <- 1
    #   } else if (x[index[i, "row"], index[i, "row"]] == 0) {  # if p(w1) = 0
    #     NPMIs[index[i, "row"], index[i, "col"]] <- 0
    #   } else if (x[index[i, "col"], index[i, "col"]] == 0) {  # if p(w2) = 0
    #     NPMIs[index[i, "row"], index[i, "col"]] <- 0
    #   } else if (x[index[i, "row"], index[i, "col"]] == 0) {  # if p(w1, w2) = 0
    #     NPMIs[index[i, "row"], index[i, "col"]] <- -1
    #   } else {
    #     stop("Unknown error in NPMI computation")
    #   }
    # }
  }
  NPMIs
}


## C_NPMI as in "Exploring the space of topic coherence measures"
C_NPMI <- function(x, nWords = 10, window = 10, topics = 1:x$L1, sentiments = 1:x$L2, top_method = c("probability", "term-score", "loglinear", "inversed loglinear", "topic-score", "sentiment-score", "loglinear excluding shared"), NPMIs = NULL) {
  topic <- sentiment <- NULL # due to NSE notes in R CMD check
  top_method <- match.arg(top_method)

  if (is.null(NPMIs)) NPMIs <- computeNPMI(x$tokens, window)

  coherence <- matrix(0, length(topics), length(sentiments))
  dimnames(coherence) <- list(paste0("topic", topics), paste0("sentiment", sentiments))

  for (t in 1:length(topics)) {
    for (s in 1:length(sentiments)) {

      top <- top_words_dt(x, nWords, top_method)
      if (is.null(top$L2)) top$L2 <- 1
      top <- merge(top[L1 == topics[t] & L2 == sentiments[s]], x$vocabulary, by.x = "word", by.y = "word")
      sub <- NPMIs[as.character(top$index), as.character(top$index)]

      coherence[t,s] <- mean(sub[upper.tri(sub)])
    }
  }
  coherence
}


engine_CV <- function(NPMIs, top, weight = 1) {
  S <- segmentation(top, "set")
  similarity <- rep(0, length(S))
  b <- colSums(NPMIs[as.character(S[[1]][[2]]), , drop = FALSE])
  for (i in 1:length(S)) {
    a <- colSums(NPMIs[as.character(S[[i]][[1]]), , drop = FALSE])
    similarity[i] <- stats::as.dist(cosineSimilarity(cbind(a, b)))
  }
  if (length(weight) == 1) mean(similarity) else sum(similarity * weight)
}


## C_V as in "Exploring the space of topic coherence measures"
C_V <- function(x, nWords = 10, window = 110, topics = 1:x$L1, sentiments = 1:x$L2, top_method = c("probability", "term-score", "loglinear", "inversed loglinear", "topic-score", "sentiment-score", "loglinear excluding shared", "topics"), NPMIs = NULL) {
  topic <- sentiment <- NULL # due to NSE notes in R CMD check
  top_method <- match.arg(top_method)

  if (is.null(NPMIs)) NPMIs <- computeNPMI(x$tokens, window)
  coherence <- matrix(0, length(topics), length(sentiments))
  dimnames(coherence) <- list(paste0("topic", topics), paste0("sentiment", sentiments))

  tops <- merge(top_words_dt(x, nWords, top_method), x$vocabulary, by.x = "word", by.y = "word", sort = FALSE)


  for (t in 1:length(topics)) {
    for (s in 1:length(sentiments)) {
      top <- tops[L1 == topics[t] & L2 == sentiments[s]]
      coherence[t,s] <- engine_CV(NPMIs, top)
    }
  }
  coherence
}
