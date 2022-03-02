
## function extracting c++ model and transform it to a R object
extract_cppModel <- function(x, base) {

  rModel <- list()
  rModel$it <- x$it
  ## source alpha and gamma from x if updates are enabled
  if (base$L1cycle != 0L) rModel$L1prior <- base$L1prior <- x$L1prior
  if (base$L2cycle != 0L) rModel$L2prior <- base$L2prior <- x$L2prior

  if (rModel$it > 0) {
    rModel$L1post <- computeTheta(x, base)
    rModel$L2post <- computePi(x, base)
    rModel$phi <- computePhi(x, base)
  }

  rModel$logLikelihood <- x$logLikelihoodW + x$logLikelihoodL1 + x$logLikelihoodL2
  attr(rModel$logLikelihood, "components") <- list(
    logLikelihoodW = x$logLikelihoodW,
    logLikelihoodL1 = x$logLikelihoodL1,
    logLikelihoodL2 = x$logLikelihoodL2
  )

  rModel
}

## function rebuilding C++ model from an R object
rebuild_cppModel <- function(x, base) {
  ### TODO: base$cleaned is not protected after the call to rebuild_cppModel...
  ### create an issue since it is not copied to CPP

  ## source alpha and gamma from x if updates are enabled
  if (base$L1cycle != 0L) base$L1prior <- x$L1prior
  if (base$L2cycle != 0L) base$L2prior <- x$L2prior

  if (isTRUE(all.equal(stats::median(base$beta), 0))) stop("Unable to rebuild initBeta.")
  cppModel <- methods::new(cpp_sentopicmodel, attr(x, "reversed"))

    cppModel$rebuild(
      nrow(base$vocabulary), # x$V
      x$L1,
      x$L2,
      length(base$tokens), # x$D
      1L, # length(unique(quanteda::docvars(base$tokens, ".class"))), # x$C
      x$it,
      x$initLDA,
      x$smooth,
      x$za,
      #### clean tokens before rebuilding
      base$cleaned,
      as.integer(base$vocabulary$lexicon) - 1, ## shift index for C++
      base$L1prior,
      base$beta,
      base$L2prior,
      base$L1cycle,
      base$L2cycle,
      if (is.null(x$logLikelihood)) 0 else attr(x$logLikelihood, "components")$logLikelihoodW,
      if (is.null(x$logLikelihood)) 0 else attr(x$logLikelihood, "components")$logLikelihoodL1,
      if (is.null(x$logLikelihood)) 0 else attr(x$logLikelihood, "components")$logLikelihoodL2,
      # x$histAlpha,
      # x$histGamma,
      # x$lambdaFactor
      stats::median(base$beta) ### caution... will not work if too many lexicon
    )
  cppModel
}

## function performing integrity checks on count structures & assignments lists of a model
check_integrity <- function(x, detailed = FALSE, fast = TRUE) {
  x <- as.sentopicmodel(x)

  if (!identical(dim(x$L1prior), as.integer(c(x$L1, 1)))) stop("Internal error in dimensions of priors.")
  if (!identical(dim(x$L2prior), as.integer(c(x$L1 * x$L2, 1)))) stop("Internal error in dimensions of priors.")
  if (!identical(dim(x$beta), as.integer(c(x$L1 * x$L2, length(quanteda::types(x$tokens)))))) stop("Internal error in dimensions of priors.")

  tmp <- unlist(x$za, use.names = FALSE, recursive = FALSE)
  zaRange <- c(min(tmp), max(tmp))
  zaFlag <- zaRange[1] >= 1 && zaRange[2] <= (x$L1 * x$L2)

  ## can do twice faster using rcpp if ever needed..
  # //[[Rcpp::export(rng = false)]]
  # IntegerVector frange(const IntegerVector& x) {
  #   return(range(x));
  # }

  toksFlag <- identical(
    c(quanteda::types(x$tokens)),
    x$vocabulary$word
  )

  if (attr(x, "reversed")) {
    lexFlag <- all(
      all(x$beta[, is.na(x$vocabulary$lexicon)] != 0)
      ### Cannot rely on media value as merging models produces multiple values in beta
      # all(colSums(x$beta[, !is.na(x$vocabulary$lexicon)]) == stats::median(x$beta[x$beta > 0])*x$L1)
    )
  } else { ## TODO: improve this. Do some aperm on x$beta depending on the lexicon dimension?
    ######### Basically reversing in JST/reversedJST lexicon still refer to the same dimension. it's not reversed
    if (any(!is.na(x$vocabulary$lexicon))) {
      S <- levels(x$vocabulary$lexicon)
      lexFlag <- all(
        all(x$beta[, is.na(x$vocabulary$lexicon)] != 0),
        isTRUE(all.equal(unique(colSums(x$beta[, !is.na(x$vocabulary$lexicon), drop = FALSE])),
                         unique(x$beta[x$beta > 0])*x$L2)),
        ### this checks that no lexicon word is assigned to another L1 than designated
        sapply(seq_along(S), function(i) {
          all(rebuild_zw(x)[-seq((i - 1)*x$L2 + 1, i*x$L2), which(x$vocabulary$lexicon == S[i])] == 0)
        })
      )
    } else lexFlag <- TRUE
  }

  flags = mget(sapply(expression(zaFlag, toksFlag, lexFlag), paste0))

  if (x$it > 0) {
    if (!identical(dim(x$L2post), as.integer(c(x$L2, x$L1, length(x$tokens))))) stop("Internal error in dimensions of mixtures.")
    if (!identical(dim(x$L1post), as.integer(c(length(x$tokens), x$L1)))) stop("Internal error in dimensions of mixtures.")
    if (!identical(dim(x$phi), as.integer(c(length(quanteda::types(x$tokens)), x$L2, x$L1)))) stop("Internal error in dimensions of mixtures.")

    L2Flag <- all(abs(colSums(x$L2post) - 1) < sqrt(.Machine$double.eps))
    L1Flag <-  all(abs(rowSums(x$L1post) - 1) < sqrt(.Machine$double.eps))
    phiFlag <- all(abs(colSums(x$phi) - 1) < sqrt(.Machine$double.eps))

    flags = c(flags, mget(sapply(expression(L2Flag, L1Flag, phiFlag), paste0)))
  }

  if (detailed) {
    flags
  } else {
    all(unlist(flags, recursive = FALSE))
  }
}

reorder_sentopicmodel <- function(x) {
  x <- as.sentopicmodel(x)
  if (is.null(attr(x, "reversed"))) stop("Object corrupted, missing reversed attribute.")
  reversed <- attr(x, "reversed")
  Sdim <- attr(x, "Sdim")
  labels <- attr(x, "labels")
  if (!is.null(x$IGNORE_CHECK)) return(x)
  x <- x[c("tokens", "vocabulary", "L1", "L2", "L1prior", "L2prior", "beta",
           "it",
           "za",
           # "zw", "zd",
           "L1post", "L2post", "phi",
           "logLikelihood",
           "initLDA", "smooth", "L1cycle", "L2cycle")]
  x <- x[!sapply(x, is.null)]
  class(x) <- c("sentopicmodel", "cLDA")
  attr(x, "reversed") <- reversed
  attr(x, "Sdim") <- Sdim
  attr(x, "labels") <- labels
  if (!check_integrity(x)) stop("Internal error when reordering the sentopicmodel object.") # this has an impact on subsetting performance.. consider removing or optimizing
  x
}


core <- function(x) {
  # res <- x[c("tokens", "vocabulary", "L1prior", "beta", "L2prior", "L1cycle", "L2cycle")]
  res <- x[c("tokens", "vocabulary", "beta", "L1cycle", "L2cycle")]
  attr(res, "Sdim") <- attr(x, "Sdim")
  attr(res, "reversed") <- attr(x, "reversed")
  ## extract only if updates are disabled
  if (res$L1cycle == 0L) res$L1prior <- x$L1prior
  if (res$L2cycle == 0L) res$L2prior <- x$L2prior
  res$cleaned <- cleanPadding(x$tokens)
  res
}

`core<-` <- function(x, value) {
  if (is.null(value)) {
    if (x$L1cycle == 0L) x$L1prior <- NULL
    if (x$L2cycle == 0L) x$L2prior <- NULL
    x[c("tokens", "vocabulary", "beta", "L1cycle", "L2cycle")] <- NULL
  } else {
    if (value$L1cycle == 0) x$L1prior <- value$L1prior
    if (value$L2cycle == 0) x$L2prior <- value$L2prior
    x[c("tokens", "vocabulary", "beta", "L1cycle", "L2cycle")] <- value[c("tokens", "vocabulary", "beta", "L1cycle", "L2cycle")]
  }
  x
}

rebuild_zd <- function(x) {
  wrapper_cpp_rebuild_zd(x$za, x$L1 * x$L2)
}
rebuild_zw <- function(x, base = core(x), array = FALSE) {
  zw <- wrapper_cpp_rebuild_zw(cleanPadding(base$tokens), x$za, x$L1 * x$L2, nrow(base$vocabulary))
  if (array) array(zw, dim = c(x$L2, x$L1, nrow(base$vocabulary))) else zw
}





# Posterior computations --------------------------------------------------


## compute theta from a model output
computeTheta <- function(x, base = core(x)) {

  if (is.null(x$zd)) x$zd <- rebuild_zd(x)
  if (length(x$zd) == 0) x$zd <- rebuild_zd(x)
  L1 <- x$L1

  l1Sum_zd <- array(x$zd, dim = c(x$L2, L1, length(base$tokens)))
  l1Sum_zd <- colSums(l1Sum_zd)
  sum_d <- colSums(l1Sum_zd)

  perDocAlpha <- sapply(rep(1L, length(base$tokens)), function(d) base$L1prior[, d], simplify = TRUE)
  dim(perDocAlpha) <- c(L1, length(base$tokens))
  sumAlpha <- colSums(perDocAlpha)

  L1post <- l1Sum_zd + perDocAlpha
  sum_theta <- sum_d + sumAlpha
  for (d in 1:length(base$tokens)) {
    for (i in 1:L1) {
      L1post[i, d] <- L1post[i, d] / sum_theta[d]
    }
  }

  L1post <- t(L1post)
  dimnames(L1post) <- list(.id = names(base$tokens), L1 = labels_sentopicmodel(x, base)[["L1"]] )
  # dimnames(theta)[[2]] <- paste0("topic", 1:x$T)
  # dimnames(theta)[[1]] <- names(base$intTokens)

  L1post
}

## compute pi from a model output
computePi <- function(x, base = core(x)) {

  if (x$L2 == 1L) {
    L2post <- array(1, dim = c(1, x$L1, length(base$tokens)))
    dimnames(L2post) <- list(L2 = labels_sentopicmodel(x, base)[["L2"]], L1 = labels_sentopicmodel(x, base)[["L1"]], .id = names(base$tokens))
    return(L2post)
  }

  if (is.null(x$zd)) x$zd <- rebuild_zd(x)
  if (length(x$zd) == 0) x$zd <- rebuild_zd(x)
  L1 <- x$L1
  L2 <- x$L2

  l1Sum_zd <- array(x$zd, dim = c(L2, L1, length(base$tokens)))
  l1Sum_zd <- colSums(l1Sum_zd)

  perDocGamma <- sapply(rep(1L, length(base$tokens)), function(d) base$L2prior[, d], simplify = TRUE)
  l1Sum_perDocGamma <- array(perDocGamma, dim = c(L2, L1, length(base$tokens)))
  l1Sum_perDocGamma <- colSums(l1Sum_perDocGamma)

  L2post <- x$zd + perDocGamma
  sum_pi <- l1Sum_zd + l1Sum_perDocGamma

  for (d in 1:length(base$tokens)) {
    for (i in 1:L1) {
      L2post[1:L2 + (i - 1) * L2, d] <- L2post[1:L2 + (i - 1) * L2, d] / sum_pi[i, d]
    }
  }

  L2post <- array(L2post, dim = c(L2, L1, length(base$tokens)))
  dimnames(L2post) <- list(L2 = labels_sentopicmodel(x, base)[["L2"]], L1 = labels_sentopicmodel(x, base)[["L1"]], .id = names(base$tokens))
  # dimnames(pi)[[2]] <- paste0("topic", 1:x$T)
  # dimnames(pi)[[1]] <- paste0("sent", 1:x$S)
  # dimnames(pi)[[3]] <- names(base$intTokens)

  L2post
}


## compute phi from a model output
computePhi <- function(x, base = core(x)) {

  if (is.null(x$zw)) x$zw <- rebuild_zw(x, base)
  if (length(x$zw) == 0) x$zw <- rebuild_zw(x, base)

  phi <- x$zw + base$beta
  sum_phi <- rowSums(x$zw) + rowSums(base$beta)

  for (i in 1:nrow(phi)) {
    phi[i, ] <- phi[i, ] / sum_phi[i]
  }

  phi <- aperm(array(phi, dim = c(x$L2, x$L1, ncol(x$zw))), c(3,1,2))

  dimnames(phi) <- list(word = base$vocabulary$word, L2 = labels_sentopicmodel(x, base)[["L2"]], L1 = labels_sentopicmodel(x, base)[["L1"]])

  # dimnames(phi)[[1]] <- base$vocabulary$word
  # dimnames(phi)[[3]] <- paste0("topic", 1:x$T)
  # dimnames(phi)[[2]] <- paste0("sent", 1:x$S)

  ### TODO: find a way to simplify phi for LDA without breaking checks afterwards
  phi
}

# Chains distance functions -----------------------------------------------

## compute pairwise distance between MCMCs
hellingerDistances <- function(multiChains) {
  nChains <- attr(multiChains, "nChains")
  l_phi <- lapply(multiChains, function(x){
    matrix(x$phi, nrow(x$vocabulary), x$L1 * x$L2)
  })
  cb <- utils::combn(1:length(l_phi), 2, simplify = FALSE)
  distMatrix <- matrix(0, nChains, nChains)
  for (i in 1:length(cb)) {
    phi1 <- l_phi[[cb[[i]][1]]]
    phi2 <- l_phi[[cb[[i]][2]]]

    ## Hellinger transformation
    # tmp <- fields::rdist(sqrt(t(phi1)), sqrt(t(phi2))) / sqrt(2)
    tmp <- euclidean_cdist(sqrt(phi1), sqrt(phi2)) / sqrt(2) ## RECHECK
    d <- RcppHungarian::HungarianSolver(tmp)$cost

    distMatrix[cb[[i]][1], cb[[i]][2]] <- d
    distMatrix[cb[[i]][2], cb[[i]][1]] <- d
  }
  dimnames(distMatrix) <- list(1:nChains,1:nChains)
  distMatrix
}

## compute pairwise distance between MCMCs
euclideanDistances <- function(multiChains) {
  nChains <- attr(multiChains, "nChains")
  l_phi <- lapply(multiChains, function(x){
    matrix(x$phi, nrow(x$vocabulary), x$L1 * x$L2)
  })
  cb <- utils::combn(1:length(l_phi), 2, simplify = FALSE)
  distMatrix <- matrix(0, nChains, nChains)
  for (i in 1:length(cb)) {
    phi1 <- l_phi[[cb[[i]][1]]]
    phi2 <- l_phi[[cb[[i]][2]]]

    ## euclidean norm
    # tmp <- fields::rdist(t(phi1), t(phi2))
    tmp <- euclidean_cdist(phi1, phi2)
    d <- RcppHungarian::HungarianSolver(tmp)$cost

    distMatrix[cb[[i]][1], cb[[i]][2]] <- d
    distMatrix[cb[[i]][2], cb[[i]][1]] <- d
  }
  dimnames(distMatrix) <- list(1:nChains,1:nChains)
  distMatrix
}

## compute pairwise distance between MCMCs
stupidEuclideanDistances <- function(multiChains) {
  nChains <- attr(multiChains, "nChains")
  l_phi <- lapply(multiChains, function(x){
    matrix(x$phi, nrow(x$vocabulary), x$L1 * x$L2)
  })
  cb <- utils::combn(1:length(l_phi), 2, simplify = FALSE)
  distMatrix <- matrix(0, nChains, nChains)
  for (i in 1:length(cb)) {
    phi1 <- l_phi[[cb[[i]][1]]]
    phi2 <- l_phi[[cb[[i]][2]]]

    ## euclidean norm
    # tmp <- fields::rdist(t(phi1), t(phi2))
    tmp <- euclidean_cdist(phi1, phi2)
    d <- sum(diag(tmp))

    distMatrix[cb[[i]][1], cb[[i]][2]] <- d
    distMatrix[cb[[i]][2], cb[[i]][1]] <- d
  }
  dimnames(distMatrix) <- list(1:nChains,1:nChains)
  distMatrix
}



## compute pairwise distance between MCMCs
## This assumes that sentiments are correctly estimated
invariantEuclideanOptim <- function(multiChains, L1 = multiChains[[1]]$L1, L2 = multiChains[[1]]$L2) {

  # if (S == 1) stop("invariantEuclidan is undefined for non-JST models.")
  strict <- TRUE
  if (all(is.na(multiChains$vocabulary$lexicon)) && L2 > 1) {
    message("No lexicon detected, allowing permutations over sentiment labels.")
    strict <- FALSE
  }

  nChains <- attr(multiChains, "nChains")
  l_phi <- lapply(multiChains, function(x){
    matrix(x$phi, nrow(x$vocabulary), x$L1 * x$L2)
  })
  cb <- utils::combn(1:length(l_phi), 2, simplify = FALSE)
  distMatrix <- matrix(0, nChains, nChains)
  for (i in 1:length(cb)) {
    phi1 <- l_phi[[cb[[i]][1]]]
    phi2 <- l_phi[[cb[[i]][2]]]

    ## euclidean norm
    # tmp <- fields::rdist(t(phi1), t(phi2))
    tmp <- euclidean_cdist(phi1, phi2)

    ## Check only permutations of L1. Size: factorial(L1)
    ## Idea: L2 are fixed per L1. Reduce the size of the (L1*L2, L1*L2) matrix
    ## to a (L1, L1) matrix by summing the diagonal of each submatrix (L2, L2).
    ## Then, apply hungarian algorithm to find the minimum L1 pairs. L1 pairs
    ## are expanded to find the max difference between two topic-sentiment
    if (attr(unclass(multiChains)[[1]], "reversed") && strict) {

      tmp2 <- matrix(0, L1, L1)
      for (ii in 1:L1) {
        for (jj in 1:L1) {
          tmp2[ii, jj] <- sum(diag(tmp[1:L2 + (ii - 1)*L2, 1:L2 + (jj - 1)*L2, drop = FALSE]))
        }
      }
      # d <- RcppHungarian::HungarianSolver(tmp2)$cost
      pairs <- RcppHungarian::HungarianSolver(tmp2)$pairs
      # # d <- max(apply(pairs, 1, function(x) tmp2[x[1], x[2]]))

      expandPairs <- apply(pairs, c(2,1), expandIndex, S = L2)
      # mapply( function(i, j) tmp[i, j], expandPairs[, 1], expandPairs[, 2])
      if (L2 == 1L) {
        dim(expandPairs) <- c(1, dim(expandPairs))
      }
      # stopifnot(isTRUE(all.equal(  ## expandPairs simply breakdowns the total cost(distance)
      #   sum(apply(expandPairs, c(1,3), function(x) tmp[x[1], x[2]])),
      #   RcppHungarian::HungarianSolver(tmp2)$cost
      # )))
      d <- max(apply(expandPairs, c(1,3), function(x) tmp[x[1], x[2]]))

      ##TODO: safety, to be removed
      if (!identical(dim(expandPairs), as.integer(c(L2, 2L, L1)))) stop("Condition not fulfilled")
    }
    else if (!attr(unclass(multiChains)[[1]], "reversed") && strict) {

      tmp2 <- vector("list", L1)
      for (ii in 1:L1) {
        tmp2[[ii]] <- RcppHungarian::HungarianSolver(tmp[1:L2 + (ii - 1)*L2, 1:L2 + (ii - 1)*L2, drop = FALSE])$pairs + (ii - 1)*L2 ## Adding this so we get the real indices directly
      }
      ## topic-sentiment pairs
      matches <- do.call(rbind, tmp2)
      ## max distance among topic-sentiment pairs
      d <- max(tmp[matches])
    } else if (!strict) {
      tmp2 <- matrix(0, L1, L1)
      tmpPairs <- array(list(), dim = c(L1,L1))
      for (ii in 1:L1) {
        for (jj in 1:L1) {
          hungarian <- RcppHungarian::HungarianSolver(tmp[1:L2 + (ii - 1)*L2, 1:L2 + (jj - 1)*L2, drop = FALSE])
          tmp2[ii, jj] <- hungarian$cost
          ## correct indices directly
          hungarian$pairs[, 1] <- hungarian$pairs[, 1] + (ii - 1) * L2
          hungarian$pairs[, 2] <- hungarian$pairs[, 2] + (jj - 1) * L2
          tmpPairs[[ii, jj]] <- hungarian$pairs
        }
      }
      pairs <- RcppHungarian::HungarianSolver(tmp2)$pairs

      # tmp2[pairs]
      ## topic-sentiment pairs
      matches <- do.call(rbind, tmpPairs[pairs])
      ## max distance among topic-sentiment pairs
      d <- max(tmp[matches])
    }



    # tmp
    # m <- 4
    # tmp[1 + 0L:(m - 1L) * (dim(tmp)[1L] + 1)]
    #
    # diag(tmp)
    # diag(tmp[nrow(tmp):1, ])


    #



    distMatrix[cb[[i]][1], cb[[i]][2]] <- d
    distMatrix[cb[[i]][2], cb[[i]][1]] <- d
  }
  dimnames(distMatrix) <- list(1:nChains,1:nChains)
  distMatrix
}

expandIndex <- function(x, S) {
  seq( (x - 1)*S + 1, length.out = S)
}

cosineDistances <- function(multiChains) {
  nChains <- attr(multiChains, "nChains")
  l_phi <- lapply(multiChains, function(x){
    matrix(x$phi, nrow(x$vocabulary), x$L1 * x$L2)
  })
  cb <- utils::combn(1:length(l_phi), 2, simplify = FALSE)
  distMatrix <- matrix(0, nChains, nChains)
  for (i in 1:length(cb)) {
    phi1 <- l_phi[[cb[[i]][1]]]
    phi2 <- l_phi[[cb[[i]][2]]]

    tmp <- round(1 - cosineSimilarity(phi1, phi2), 15) ### resolves issue where 1 =/= 1 because numeric stuff
    d <- RcppHungarian::HungarianSolver(tmp)$cost

    distMatrix[cb[[i]][1], cb[[i]][2]] <- d
    distMatrix[cb[[i]][2], cb[[i]][1]] <- d
  }
  dimnames(distMatrix) <- list(1:nChains,1:nChains)
  distMatrix
}

## To keep?
## minimum matching euclidean distance
minMaxEuclidean <- function(multiChains) {
  nChains <- attr(multiChains, "nChains")
  l_phi <- lapply(multiChains, function(x){
    matrix(x$phi, nrow(x$vocabulary), x$L1 * x$L2)
  })
  cb <- utils::combn(1:length(l_phi), 2, simplify = FALSE)
  distMatrix <- matrix(0, nChains, nChains)
  for (i in 1:length(cb)) {
    phi1 <- l_phi[[cb[[i]][1]]]
    phi2 <- l_phi[[cb[[i]][2]]]

    ## euclidean norm
    # tmp <- fields::rdist(t(phi1), t(phi2))
    tmp <- euclidean_cdist(phi1, phi2)

    maxMin <- 1:nrow(tmp)
    for (j in 1:nrow(tmp)) {
      maxMin[j] <- min(tmp[, j])
    }
    max1 <- max(maxMin)
    maxMin <- 1:nrow(tmp)
    for (j in 1:nrow(tmp)) {
      maxMin[j] <- min(tmp[j, ])
    }
    max2 <- max(maxMin)

    d <- max(max1, max2)

    distMatrix[cb[[i]][1], cb[[i]][2]] <- d
    distMatrix[cb[[i]][2], cb[[i]][1]] <- d
  }
  dimnames(distMatrix) <- list(1:nChains,1:nChains)
  distMatrix
}


# Misc --------------------------------------------------------------------

# from MCMCpack::rdirichlet
rdirichlet <- function(n, alpha) {
  l <- length(alpha)
  x <- matrix(stats::rgamma(l * n, alpha), ncol = l, byrow = TRUE)
  sm <- x %*% rep(1, l)
  return(x/as.vector(sm))
}

cosineSimilarity <- function(x, y = NULL) {
  if (is.null(y)) y <- x
  cpp_cosineSimilarity(x, y)
}

cleanPadding <- function(x) {
  clean <- lapply(unclass(x), function(x) x[x > 0])
  c(clean)
}

spreadColor <- function(color, n, range = .1) {
  if (n == 1) return(color)
  range <- as.integer(range * 255)
  rgb <- grDevices::col2rgb(color)[, 1]
  upper <- rgb
  upper["green"] <- upper["green"] + range
  upper["red"] <- upper["red"] - range

  lower <- rgb
  lower["green"] <- lower["green"] - range
  lower["red"] <- lower["red"] + range

  ## limit to 0:255
  tmp <- rbind(lower, upper)
  tmp[tmp < 0] <- 0L
  tmp[tmp > 255] <- 255L

  tmp <- apply(tmp, 2, as.list)
  colRange <- do.call(mapply, c(FUN = grDevices::rgb, tmp, maxColorValue = 255))
  grDevices::colorRampPalette(colRange)(n)
}


missingSuggets <- function(packages) {
  Suggests <- sapply(packages, requireNamespace, quietly = TRUE)
  names(Suggests)[!Suggests]
}

# ## quick check
# col <- RColorBrewer::brewer.pal(3, "Set1")
# COLORS <- spreadColor(col[2], 3, range = .25)
# plot(1L, 1L, col = COLORS[2], pch = 19, cex = 5)
# points(1.2, 1L, col = COLORS[3], pch = 19, cex = 5)
# points(.8, 1L, col = COLORS[1], pch = 19, cex = 5)
# sapply(col, spreadColor, 3)


floor_date <- function(x, period = c("day", "month", "quarter", "year")) {
  period <- match.arg(period)
  if (period == "day") return(x)
  x <- as.POSIXlt(x)
  switch(period,
         week48 = {
           d <- as.integer(ceiling(date$mday / 7))
           x$mday <- ifelse(d == 5L, 4L, d) * 7L
         },
         month = x$mday <- rep_len(1, length(x)),
         quarter = {
           x$mday <- rep_len(1, length(x))
           x$mon <- x$mon %/% 3 * 3
         },
         year = {
           x$mday <- rep_len(1, length(x))
           x$mon <- rep_len(0, length(x))
         })
  as.Date(x)
}

# Alternate likelihood ----------------------------------------------------
multLikelihoodW <- function(x) {
  if (is.null(x$zw)) x$zw <- rebuild_zw(x)
  epsilon <- 10^-12
  logLik <- 0
  logLikMultinomial <- 0

  for (z in 1:(x$L1 * x$L2)) {
    numOne <- lgamma(sum(x$beta[z, ]))
    denomOne <- sum(lgamma(x$beta[z, ] + epsilon))

    numMultinomial <- lgamma(sum(x$zw[z, ]) + 1)

    numTwo <- 0
    denomMultinomial <- 0
    for (w in 1:nrow(x$vocabulary)) {
      numTwo <- numTwo + lgamma(x$zw[z, w] + x$beta[z, w] + epsilon)
      denomMultinomial <- denomMultinomial + lgamma(x$zw[z, w] + 1)
    }
    denomTwo <- lgamma(sum(x$zw[z, ]) + sum(x$beta[z, ]))

    tmp <- numOne + numTwo - denomOne - denomTwo
    logLik <- logLik + tmp
    logLikMultinomial <- logLikMultinomial + tmp + numMultinomial - denomMultinomial
  }

  stats::setNames(c(logLik, logLikMultinomial), c("logLik", "logLik2"))
}
multLikelihoodL1 <- function(x) {
  if (is.null(x$zd)) x$zd <- rebuild_zd(x)
  epsilon <- 10^-8
  logLik <- 0
  logLikMultinomial <- 0
  C <- rep(1L, length(x$tokens))
  l1Sum_zd <- array(x$zd, dim = c(x$L2, x$L1, length(x$tokens)))
  l1Sum_zd <- colSums(l1Sum_zd)

  for (d in 1:length(x$tokens)) {

    c <- C[d]
    # c <- attr(x$intTokens, "docvars")$class[d] #fast
    # c <- quanteda::docvars(x$intTokens)$class[d]
    # c <- quanteda::docvars(x$intTokens, "class")[d]

    numOne <- lgamma(sum(x$L1prior[, c]))
    denomOne <- sum(lgamma(x$L1prior[, c] + epsilon))

    numMultinomial <- lgamma(sum(x$zd[, d]) + 1)

    numTwo <- 0
    denomMultinomial <- 0
    for (t in 1:x$L1) {
      numTwo <- numTwo + lgamma(sum(l1Sum_zd[t, d]) + x$L1prior[t, c] + epsilon)
      denomMultinomial <- denomMultinomial + lgamma(sum(l1Sum_zd[t, d]) + 1)
    }
    denomTwo <- lgamma(sum(x$zd[, d]) + sum( x$L1prior[, c]))

    tmp <- numOne + numTwo - denomOne - denomTwo
    logLik <- logLik + tmp
    logLikMultinomial <- logLikMultinomial + tmp + numMultinomial - denomMultinomial
  }
  stats::setNames(c(logLik, logLikMultinomial), c("logLik", "logLik2"))
}
multLikelihoodL2 <- function(x) {
  if (is.null(x$zd)) x$zd <- rebuild_zd(x)
  epsilon <- 10^-12
  logLik <- 0
  logLikMultinomial <- 0
  C <- rep(1L, length(x$tokens))
  l1Sum_zd <- array(x$zd, dim = c(x$L2, x$L1, length(x$tokens)))
  l1Sum_zd <- colSums(l1Sum_zd)
  l1Sum_L2prior <- colSums(array(x$L2prior, dim = c(x$L2, x$L1, length(unique(C)))))


  for (d in 1:length(x$tokens)) {

    # c <- quanteda::docvars(x$intTokens, "class")[d]
    # c <- attr(x$intTokens, "docvars")$class[d] #fast
    c <- C[d]

    for (t in 1:x$L1) {
      numOne <- lgamma(l1Sum_L2prior[t, c])
      denomOne <- sum(lgamma(x$L2prior[1:x$L2 + (t - 1) * x$L2, c] + epsilon))

      numMultinomial <- lgamma(l1Sum_zd[t, d] + 1)

      numTwo <- 0
      denomMultinomial <- 0
      for (s in 1:x$L2) {
        z <- (t - 1) * x$L2 + s
        numTwo <- numTwo + lgamma(x$zd[z, d] + x$L2prior[z, c] + epsilon)
        denomMultinomial <- denomMultinomial + lgamma(x$zd[z, d] + 1)
      }
      denomTwo <- lgamma(l1Sum_zd[t, d] + l1Sum_L2prior[t, c])

      tmp <- numOne + numTwo - denomOne - denomTwo
      logLik <- logLik + tmp
      logLikMultinomial <- logLikMultinomial + tmp + numMultinomial - denomMultinomial
    }

  }
  stats::setNames(c(logLik, logLikMultinomial), c("logLik", "logLik2"))
}
multLikelihood <- function(x) {
  x <- as.sentopicmodel(x)
  logLik <- rbind(multLikelihoodW(x),
            multLikelihoodL1(x),
            multLikelihoodL2(x))
  logLik <- rbind(colSums(logLik), logLik)
  rownames(logLik) <- c("WTS", "W", "L1", "L2")
  logLik
}








# Stemming ----------------------------------------------------------------

recompileVocabulary <- function(x) {
  class <- class(x)[1]
  x <- as.sentopicmodel(x)
  if (x$it > 0) warning("The model will be reset before recompiling the vocabulary")
  x$it <- 0
  x$logLikelihood <- NULL
  x$phi <- x$L2post <- x$L1post <- NULL

  cpp_model <- rebuild_cppModel(x, core(x))
  cpp_model$initBetaLex(stats::median(x$beta))
  cpp_model$initAssignments()
  # x <- utils::modifyList(x, extract_cppModel(cpp, core(x)))
  tmp <- extract_cppModel(cpp_model, core(x))
  x[names(tmp)] <- tmp

  x$beta <- cpp_model$beta


  fun <- get(paste0("as.", class))
  fun(reorder_sentopicmodel(x))
}



# Re-definition to avoid depencies ----------------------------------------

# This code is directly copied from the tidytext package to avoid unnecessary
# dependencies. The sole usage of this function is to order words correctly in
# `topWords()`.
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  if (!is.list(within)) {
    within <- list(within)
  }
  
  new_x <- do.call(paste, c(list(x, sep = sep), within))
  stats::reorder(new_x, by, FUN = fun)
}
scale_x_reordered <- function(..., labels = reorder_func) {
  ggplot2::scale_x_discrete(labels = labels, ...)
}
reorder_func <- function(x, sep = "___") {
  reg <- paste0(sep, ".+$")
  gsub(reg, "", x)
}


# Experimental ---------------------------------------------------------

makeVocabulary <- function(toks, dictionary, S) {
  
  ## CMD checks
  word <- NULL

  ## compound valence shifter if presents
  valence <- dictionary[grepl("valence_", names(dictionary), fixed = TRUE)]
  lexicon <- dictionary[!grepl("valence_", names(dictionary), fixed = TRUE)]

  if (length(valence) > 0) {

    ## TODO: is it best to rely on tokens_replace or better to use gsub on types
    ## and recompile if needed?

    ## append __INTERNAL__VALENCE to valence word to ease the use of
    ## tokens_compound
    ### WARNING! : this will lowercase existing words. Maybe not ideal.
    toks <- quanteda::tokens_replace(
      toks,
      valence$valence_negator,
      paste0(valence$valence_negator, "__INTERNAL__VALENCE"),
      valuetype = "fixed", case_insensitive = TRUE)

    toks <- quanteda::tokens_replace(
      toks,
      unique(unlist(lexicon, use.names = FALSE)),
      paste0("LEXICON__INTERNAL__", unique(unlist(lexicon, use.names = FALSE))),
      valuetype = "fixed", case_insensitive = TRUE)


    ## start compounding valence amplifiers first.
    if (!is.null(valence$valence_amplifier)) {
      toks <- quanteda::tokens_replace(
        toks,
        valence$valence_amplifier,
        paste0("AMPLIFIER__INTERNAL__", valence$valence_amplifier, "__INTERNAL__AMPLIFIER"),
        valuetype = "fixed", case_insensitive = TRUE)

      ## compound the sequence VALENCE-AMPLIFIER-LEXICON
      ## TODO: slow, check if this can be made faster
      toks <- quanteda::tokens_compound(
        toks,
        quanteda::phrase("*__VALENCE AMPLIFIER__INTERNAL__*__INTERNAL__AMPLIFIER LEXICON__*"),
        window = 0,
        concatenator = "__", join = FALSE,
        valuetype = "glob")

      ## replace non-compounded amplifier to their initial state
      toks <- quanteda::tokens_replace(
        toks,
        paste0("AMPLIFIER__INTERNAL__", valence$valence_amplifier, "__INTERNAL__AMPLIFIER"),
        valence$valence_amplifier,
        valuetype = "fixed", case_insensitive = FALSE)

    }

    ## compound the sequence VALENCE-LEXICON
    toks <- quanteda::tokens_compound(toks, quanteda::phrase("*__VALENCE LEXICON__*") ,
                                   window = 0,
                                   concatenator = "__", join = FALSE,
                                   valuetype = "glob")

    ## replace non-compounded valence and lexicon word to their initial state
    toks <- quanteda::tokens_replace(
      toks,
      paste0(valence$valence_negator, "__INTERNAL__VALENCE"),
      valence$valence_negator,
      valuetype = "fixed", case_insensitive = FALSE)
    toks <- quanteda::tokens_replace(
      toks,
      paste0("LEXICON__INTERNAL__", unique(unlist(lexicon, use.names = FALSE))),
      unique(unlist(lexicon, use.names = FALSE)),
      valuetype = "fixed", case_insensitive = FALSE)

    ## record words that were compounded
    compounds <- gsub("__INTERNAL__.*?__INTERNAL__", "_",
                      quanteda::types(toks)[grepl("__INTERNAL__.*__INTERNAL__", quanteda::types(toks))],
                      perl = TRUE)

    ## remove the un-needed __INTERNAL__
    toks <- quanteda::tokens_replace(
      toks,
      quanteda::types(toks)[grepl("__INTERNAL__.*__INTERNAL__", quanteda::types(toks))],
      sub(".*?__INTERNAL__.*__INTERNAL__", "#NEGATOR#__",
          quanteda::types(toks)[grepl("__INTERNAL__.*__INTERNAL__", quanteda::types(toks))],
          perl = TRUE),
      valuetype = "fixed", case_insensitive = FALSE)

  } ## end of if(length(valence) > 0)

  ## Creation of vocabulary
  vocabulary <- data.table::data.table(index = 1:length(attr(toks, "types")), word = c(attr(toks, "types")))

  ## Lexicon preparation
  if (is.null(lexicon)) {
    # message("No lexicon found.")
    vocabulary$lexicon <- factor(rep(NA, nrow(vocabulary)), levels = paste0("sent", 1:S))
  } else if (quanteda::is.dictionary(lexicon)) {

    ## Checking lexicon dimension -- reordering as [negative-neutral-positive]
    if (S != length(lexicon)) {
      if (S == 2) { ## attempts to coerce to two sentiments

        lexicon <- lexicon[tolower(names(lexicon)) %in% c("negative", "positive")]
        ## TODO: manage polarised dictionary3
        if (length(lexicon) != 2) stop("Please provide a dictionary with negative and positive categories.")
      } else if (S == 3) { ## attempts to coerce to negative-neutral-positive

        lexicon <- lexicon[tolower(names(lexicon)) %in% c("negative", "positive", "neutral")]
        if (!all(c("neutral") %in% names(lexicon))) {
          lexicon <- quanteda::dictionary(c(unclass(lexicon), list(neutral = character())))
          lexicon <- lexicon[c("negative", "neutral", "positive")] ### reorder and put neutral in the middle
        }
        if (length(lexicon) != 3) stop("Please provide a dictionary with negative and positive categories.")
      } else {
        stop("The number of dictionary entries should match the number of sentiments.")
      }
    }

    ## Match token types to lexicon entries

    # old and non robust
    # matches <- quanteda:::pattern2list(dict, quanteda::types(x), "glob", TRUE)
    # matches <- matches[sapply(matches, length) == 1]
    # matches <- split(unlist(matches, use.names = FALSE), names(matches))
    # matches <- utils::modifyList(sapply(names(dict), function(x) NULL), matches)

    # corrected
    # matches <- quanteda::object2id(dict, quanteda::types(x), "glob", TRUE)
    # tmp <- unlist(sapply(seq_along(matches), function(i) rep(names(matches[i]), length(matches[[i]]))),
    #               use.names = FALSE)
    # matches <- unlist(matches, use.names = FALSE)
    # matches <- split(matches, tmp)

    ### shorter but more hacky
    matches <- sapply(lexicon, function(i) unlist(quanteda::pattern2id(i, quanteda::types(toks), "glob", TRUE),
                                               use.names = FALSE), simplify = FALSE)

    if (length(matches) != S) stop("Dictionary error.")

    types_lex <- rep(NA, length(quanteda::types(toks)))
    for (i in seq_along(matches)) {
      types_lex[matches[[i]]] <- i
    }
    tmp <- unlist(matches, use.names = FALSE)
    types_lex[unique(tmp[duplicated(tmp)])] <- NA  ### set words in different categories to NA
    types_lex <- factor(types_lex, seq_along(matches), names(matches))

    ## Feed vocabulary with matched types
    vocabulary$lexicon <- types_lex

    ## Special matching for valence-lexicon pairs
    if (length(valence) > 0) {
      ### make matches for valence compounds
      matches <- sapply(lexicon, function(i) unlist(quanteda::pattern2id(
        i, vocabulary[grepl("#NEGATOR#__", word), sub("#NEGATOR#__", "", word)],
        "glob", TRUE),
        use.names = FALSE), simplify = FALSE)

      ## reverse negative-positive index
      names <- names(matches)
      i <- which(names(matches) %in% c("positive", "negative"))
      names(matches)[i] <- names(matches)[rev(i)]
      matches <- matches[names]

      types_val <- as.numeric(vocabulary[grepl("#NEGATOR#__", word), lexicon])
      for (i in seq_along(matches)) {
        types_val[matches[[i]]] <- i
      }
      ## feed valence-lexicon pairs to vocabulary
      vocabulary[grepl("#NEGATOR#__", word), lexicon := types_val]

      attr(vocabulary, "compounds") <- compounds
    }
  } else {
    stop("Incorrect lexicon object.")
  }
  list(vocabulary = vocabulary[], toks = toks)
}



### TODO: check
getTexts <- function(x, topic, sentiment, n = 3, collapsed = TRUE) {
  
  # CMD check
  prob <- .id <- NULL
  
  ## avoid conflict with column names
  if (attr(x, "reversed")) {
    stopifnot(topic %in% create_labels(x, flat = FALSE)[["L1"]] & sentiment %in% create_labels(x, flat = FALSE)[["L2"]])
    ..L1 <- topic
    ..L2 <- sentiment
  } else {
    stopifnot(topic %in% create_labels(x, flat = FALSE)[["L2"]] & sentiment %in% create_labels(x, flat = FALSE)[["L1"]])
    ..L2 <- topic
    ..L1 <- sentiment
  }
  x <- as.sentopicmodel(x)
  # names <- names(head(sort(x$pi[sentiment, topic, ], decreasing = TRUE), n))
  names <- utils::head(melt(x)[L1 == ..L1 & L2 == ..L2][order(-prob), .id], n)
  toks <- x$tokens[names]
  toks <- quanteda::tokens_remove(toks, "")
  if (collapsed) lapply(toks, paste0, collapse = " ")
  else toks
}


computeFcm <- function(x, window = 10) {
  collocationsV <- quanteda::dfm(sentopics:::virtualDocuments(x, window), tolower = FALSE)
  ### FOR QUANTEDA 3.2+: now dfm() reorder the types in the order in which they
  ### appear. This is not the case for a tokens object following
  ### tokens_compound(), creating a mismatch. Solution: enforce the order of the
  ### dfm using the following line.
  collocationsV <- collocationsV[, quanteda::types(x)]
  collocationsV <- quanteda::dfm_remove(collocationsV, "") ## remove padding
  stopifnot(identical(colnames(collocationsV), c(quanteda::types(x))))
  colnames(collocationsV) <- seq_len(ncol(collocationsV))
  # collocationsV <- quanteda::as.fcm(sign(quanteda::t(collocationsV)) %*%  sign(collocationsV))
  collocationsV <- quanteda::as.fcm(sign(quanteda::t(collocationsV)) %*%  sign(collocationsV) / nrow(collocationsV))
}


computeNPMI <- function(x, window) {
  NPMI(as.matrix(computeFcm(x, window), epsilon = 0))
}

create_labels <- function(x, class, flat = TRUE) {
  if (missing(class)) class <- base::class(x)[1]
  x <- as.sentopicmodel(x)
  if (!is.null(attr(x, "labels"))) {
    res <- attr(x, "labels")
    empty_lab <- !c("L1", "L2") %in% names(res)
    empty_lab <- empty_lab & (unlist(x[c("L1", "L2")]) > 1)
    if (any(empty_lab)) {
      res2 <- switch(class,
                    "sentopicmodel" = labels_sentopicmodel(x),
                    "JST" = labels_JST(x),
                    "rJST" = labels_rJST(x),
                    "LDA" = labels_LDA(x),
                    stop("Error creating labels.")
      )
      res <- c(res, res2[empty_lab])
    } 
  } else {
    res <- switch(class,
           "sentopicmodel" = labels_sentopicmodel(x),
           "JST" = labels_JST(x),
           "rJST" = labels_rJST(x),
           "LDA" = labels_LDA(x),
           stop("Error creating labels.")
    )
  }
  if (flat & all(!is.null(res$L1), !is.null(res$L2)) ) {
    c(sapply(res$L1, paste0, "_", res$L2))
  } else if (flat) {
    unlist(res, use.names = FALSE)
  } else {
    res
  }
}

labels_sentopicmodel <- function(x, base = x) {
  labs <- list(L1 = paste0("l1-", 1:x$L1),
               L2 = paste0("l2-", 1:x$L2))
  labs[[attr(base, "Sdim")]] <- levels(base$vocabulary$lexicon)
  labs
}


labels_JST <- function(x) {
  labs <- list(L1 = levels(x$vocabulary$lexicon),
               L2 = paste0("topic", 1:x$L2))
}

labels_rJST <- function(x) {
  labs <- list(L1 = paste0("topic", 1:x$L1),
               L2 = levels(x$vocabulary$lexicon))
}

labels_LDA <- function(x) {
  list(L1 = c(paste0("topic", seq(x$L1))))
}

