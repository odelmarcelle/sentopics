// TODO : if model is perfectly functional, try to disable Armadillo checks
// #define ARMA_NO_DEBUG

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <cmath>
#include <type_traits>
#include <vector>
#include "model.h"

// un-comment the following line to enable debug messages
// #define DEBUG
#ifdef DEBUG
#define DEBUG_MSG(str) do { Rcout << str << std::endl; } while ( false )
#else
#define DEBUG_MSG(str) do { } while ( false )
#endif

// #define DEBUG2
#ifdef DEBUG2
#define DEBUG_MSG2(str) do { Rcout << str << std::endl; } while ( false )
#else
#define DEBUG_MSG2(str) do { } while ( false )
#endif

// [[Rcpp::depends(RcppArmadillo,RcppProgress)]]

using namespace Rcpp;
using namespace arma;

namespace {

// R integer vectors are exposed as int*, but the model views them as
// arma::uword*.  This is only safe while Armadillo uses 32-bit indices.
static_assert(
  sizeof(int) == sizeof(arma::uword),
  "sentopics requires arma::uword to have the same width as R's int"
);

bool validDirichletParameters(const std::vector<double>& parameters) {
  for (const double parameter : parameters) {
    if (!std::isfinite(parameter) || parameter <= 0.0) {
      return false;
    }
  }
  return true;
}

class RIndexGuard {
public:
  explicit RIndexGuard(
    std::vector<std::unique_ptr<arma::uvec>>& indices_
  ) : primary(indices_), secondary(nullptr) {
    decrement(primary);
  }

  RIndexGuard(
    std::vector<std::unique_ptr<arma::uvec>>& tokens_,
    std::vector<std::unique_ptr<arma::uvec>>& assignments_
  ) : primary(tokens_), secondary(&assignments_) {
    if (primary.size() != secondary->size()) {
      Rcpp::stop("Internal error: token and assignment dimensions differ.");
    }

    decrement(primary);
    decrement(*secondary);
  }

  ~RIndexGuard() noexcept {
    increment(primary);
    if (secondary != nullptr) {
      increment(*secondary);
    }
  }

  RIndexGuard(const RIndexGuard&) = delete;
  RIndexGuard& operator=(const RIndexGuard&) = delete;

private:
  static void decrement(
    std::vector<std::unique_ptr<arma::uvec>>& indices
  ) noexcept {
    for (arma::uword d = 0; d < indices.size(); ++d) {
      *indices[d] -= 1;
    }
  }

  static void increment(
    std::vector<std::unique_ptr<arma::uvec>>& indices
  ) noexcept {
    for (arma::uword d = 0; d < indices.size(); ++d) {
      *indices[d] += 1;
    }
  }

  std::vector<std::unique_ptr<arma::uvec>>& primary;
  std::vector<std::unique_ptr<arma::uvec>>* secondary;
};

}  // namespace

// constructor default values
void model::set_default_values(bool reverse_) {
  reverse = reverse_;

  V = 1;
  L1 = 1;
  L2 = 1;
  D = 1;
  it = 0;

  alphaCycle = 0;
  gammaCycle = 0;

  logLikelihoodW = vec();
  logLikelihoodL1 = vec();
  logLikelihoodL2 = vec();

}

// initialize the model
void model::init(SEXP intTokens_,
                 SEXP za_,
                 uword V_,
                 uword L1_,
                 uword L2_,
                 ivec lexicon_,
                 mat& alpha_,
                 double beta_,
                 mat& gamma_,
                 uword alphaCycle_,
                 uword gammaCycle_
) {
  V = V_;
  L1 = L1_;
  L2 = L2_;
  D = LENGTH(intTokens_);
  C = 1;
  it = 0;

  intTokens.resize(D);
  za.resize(D);
  for (R_xlen_t i = 0; i < XLENGTH(intTokens_); i++) {
    int len = LENGTH(VECTOR_ELT(intTokens_, i));
    intTokens[i] =  std::unique_ptr<arma::uvec>(new arma::uvec( reinterpret_cast<arma::uword*>(INTEGER(VECTOR_ELT(intTokens_, i))), len, false, true)); // ok because clean in models.R is not out of scope
    za[i] =  std::unique_ptr<arma::uvec>(new arma::uvec( reinterpret_cast<arma::uword*>(INTEGER(VECTOR_ELT(za_, i))), len, false, true));
  }

  lexicon = lexicon_;
  this->initBeta = beta_;
  alpha = alpha_;
  gamma = gamma_;

  alphaCycle = alphaCycle_;
  gammaCycle = gammaCycle_;

  // initialize beta
  initBetaLex(this->initBeta);

  // initialize topic assignments, sentiment assignments and count structures
  initAssignments();
}


void model::initBetaLex(double initBeta_) {

  beta = arma::mat(L1*L2, V);
  beta.fill(initBeta_);

  for (uword w = 0; w < V; w++){

    // provided lexicon is an R object containing values of either 0:L2 or NA
    // in the IntegerVector type, NA is stored as -2147483648, the smallest integer value
    // hence, NAs are identified by simply testing -2147483648
    // WARNING : any operation on the vector such as += 1 will break this behavior
    if(lexicon(w) != INT_MIN){

      if(reverse){
        for (uword z = 0; z < L1*L2; z++){
          if( z % L2 != (uword) lexicon(w)){
            beta(z, w) = 0;
          }
        }
      } else {
        for (uword z = 0; z < L1*L2; z++){
          if( z / L2 != (uword) lexicon(w)){
            beta(z, w) = 0;
          }
        }
      }
    }
  }
}


// initialize topic/sentiments assignments and count structures
void model::initAssignments() {
  RNGScope rngScope;
  RIndexGuard indexGuard(intTokens, za);

  uvec zSet = regspace<uvec>(0, (L1*L2)-1);

  DEBUG_MSG("Zset : " << trans(zSet));
  DEBUG_MSG("First RNG draws... : " << trans(Rcpp::RcppArmadillo::sample(zSet, 10, true)));

  for (uword d = 0; d < D; d++){

    DEBUG_MSG("Starting document " << d << "...");

    uword W = intTokens[d]->size();

    DEBUG_MSG("za[d] before draw: " << trans(*za[d]) );

    *za[d] = Rcpp::RcppArmadillo::sample(zSet, W, true);
    DEBUG_MSG("za[d] after draw: " << trans(*za[d]) );
    for (uword w = 0; w < W; w++){
      if ( lexicon(intTokens[d]->at(w)) != INT_MIN ) {
        za[d]->at(w) = as_scalar(Rcpp::RcppArmadillo::sample(zSet, 1, true, beta.col(intTokens[d]->at(w)) ));
      }
    }
  }
}

// rebuild the model from R input
void model::rebuild(uword V_,
                    uword L1_,
                    uword L2_,
                    uword D_,
                    uword C_,
                    uword it_,
                    SEXP za_,
                    SEXP intTokens_,
                    ivec& lexicon_,
                    mat& alpha_,
                    mat& beta_,
                    mat& gamma_,
                    uword alphaCycle_,
                    uword gammaCycle_,
                    vec& logLikelihoodW_,
                    vec& logLikelihoodL1_,
                    vec& logLikelihoodL2_,
                    double initBeta_
) {

  if (static_cast<R_xlen_t>(D_) != XLENGTH(intTokens_)) {
    Rcpp::stop("The supplied document count does not match intTokens_.");
  }
  if (static_cast<R_xlen_t>(D_) != XLENGTH(za_)) {
    Rcpp::stop("The supplied document count does not match za_.");
  }
  for (R_xlen_t i = 0; i < XLENGTH(intTokens_); ++i) {
    if (XLENGTH(VECTOR_ELT(intTokens_, i)) != XLENGTH(VECTOR_ELT(za_, i))) {
      Rcpp::stop(
        "The token and assignment vectors must have the same length for every document."
      );
    }
  }

  // imported variables
  V = V_;
  L1 = L1_;
  L2 = L2_;
  D = D_;
  C = C_;
  it = it_;
  initBeta = initBeta_;
  intTokens.resize(D_);
  za.resize(D_);
  for (R_xlen_t i = 0; i < XLENGTH(intTokens_); i++) {

    int len = LENGTH(VECTOR_ELT(intTokens_, i));
    intTokens[i] =  std::unique_ptr<arma::uvec>(new arma::uvec( reinterpret_cast<arma::uword*>(INTEGER(VECTOR_ELT(intTokens_, i))), len, false, true)); // "cleaned" need to remains in "base". If not, "cleaned" gets out of scope and the shared memory is no longer protected from re-allocation, which can cause the memory to become corrupted
    za[i] =  std::unique_ptr<arma::uvec>(new arma::uvec( reinterpret_cast<arma::uword*>(INTEGER(VECTOR_ELT(za_, i))), len, false, true));
  }
  lexicon = lexicon_;
  alpha = alpha_;
  beta = beta_;
  gamma = gamma_;
  alphaCycle = alphaCycle_;
  gammaCycle = gammaCycle_;
  logLikelihoodW = logLikelihoodW_;
  logLikelihoodL1 = logLikelihoodL1_;
  logLikelihoodL2 = logLikelihoodL2_;
}


// [[Rcpp::export(rng = false)]]
arma::imat wrapper_cpp_rebuild_zw(SEXP intTokens_, SEXP za_, arma::uword Z, arma::uword V) {

  int D = LENGTH(intTokens_);
  std::vector<std::unique_ptr<arma::uvec>> intTokens(D);
  std::vector<std::unique_ptr<arma::uvec>> za(D);
  for (R_xlen_t i = 0; i < XLENGTH(intTokens_); i++) {
    int len = LENGTH(VECTOR_ELT(intTokens_, i));
    intTokens[i] =  std::unique_ptr<arma::uvec>(new arma::uvec( reinterpret_cast<arma::uword*>(INTEGER(VECTOR_ELT(intTokens_, i))), len, false, true));
    za[i] =  std::unique_ptr<arma::uvec>(new arma::uvec( reinterpret_cast<arma::uword*>(INTEGER(VECTOR_ELT(za_, i))), len, false, true));
  }
  RIndexGuard r_index_guard(intTokens, za);
  return cpp_rebuild_zw(intTokens, za, Z, V);
}

arma::imat cpp_rebuild_zw(std::vector<std::unique_ptr<arma::uvec>>& intTokens, std::vector<std::unique_ptr<arma::uvec>>& za, arma::uword Z, arma::uword V) {
  arma::imat zw = imat(Z, V, fill::zeros);
  uword D = intTokens.size();
  DEBUG_MSG2("Rebuilding zw");
  for (uword d = 0; d < D; d++){
    uword W = intTokens[d]->size();
    for (uword w = 0; w < W; w++){
      zw(za[d]->at(w), intTokens[d]->at(w)) += 1;
    }
  }
  DEBUG_MSG2("zw rebuilt (" +  std::to_string(zw.n_rows) + " x " + std::to_string(zw.n_cols) + ")");
  return(zw);
}

arma::imat cpp_rebuild_l1w(std::vector<std::unique_ptr<arma::uvec>>& intTokens, std::vector<std::unique_ptr<arma::uvec>>& za, arma::uword Z, arma::uword V, arma::uword L2) {
  arma::imat zw = imat(Z/L2, V, fill::zeros);
  uword D = intTokens.size();
  DEBUG_MSG2("Rebuilding l1w");
  for (uword d = 0; d < D; d++){
    uword W = intTokens[d]->size();
    for (uword w = 0; w < W; w++){
      zw(za[d]->at(w) / L2, intTokens[d]->at(w)) += 1;
    }
  }
  DEBUG_MSG2("l1w rebuilt (" +  std::to_string(zw.n_rows) + " x " + std::to_string(zw.n_cols) + ")");
  return(zw);
}

// [[Rcpp::export(rng = false)]]
arma::imat wrapper_cpp_rebuild_zd(SEXP za_, arma::uword Z) {
  int D = LENGTH(za_);
  std::vector<std::unique_ptr<arma::uvec>> za(D);
  for (R_xlen_t i = 0; i < XLENGTH(za_); i++) {
    int len = LENGTH(VECTOR_ELT(za_, i));
    za[i] =  std::unique_ptr<arma::uvec>(new arma::uvec( reinterpret_cast<arma::uword*>(INTEGER(VECTOR_ELT(za_, i))), len, false, true));
  }
  RIndexGuard r_index_guard(za);
  return cpp_rebuild_zd(za, Z);
}

arma::imat cpp_rebuild_zd(std::vector<std::unique_ptr<arma::uvec>>& za, arma::uword Z) {
  uword D = za.size();
  arma::imat zd = imat(Z, D, fill::zeros);
  DEBUG_MSG2("Rebuilding zd");
  for (uword d = 0; d < D; d++){
    for (const auto& word : *za[d]){ // iterator is slightly more efficient
      zd(word, d) += 1;
    }
  }
  DEBUG_MSG2("zd rebuilt (" +  std::to_string(zd.n_rows) + " x " + std::to_string(zd.n_cols) + ")");;
  return(zd);
}


arma::imat cpp_rebuild_l1d(std::vector<std::unique_ptr<arma::uvec>>& za, arma::uword Z, arma::uword L2) {
  uword D = za.size();
  arma::imat zd = imat(Z/L2, D, fill::zeros);
  DEBUG_MSG2("Rebuilding l1d");
  for (uword d = 0; d < D; d++){
    for (const auto& word : *za[d]){ // iterator is slightly more efficient
      zd(word / L2, d) += 1;
    }
  }
  DEBUG_MSG2("l1d rebuilt (" +  std::to_string(zd.n_rows) + " x " + std::to_string(zd.n_cols) + ")");
  return(zd);
}


void model::iterate(uword iterations, bool displayProgress, bool computeLikelihood) {
  RNGScope rngScope;

  // add "iterations" elements to the likelihood storage vectors
  const uword likelihoodEnd = it + iterations;
  logLikelihoodW.resize(likelihoodEnd);
  logLikelihoodL1.resize(likelihoodEnd);
  logLikelihoodL2.resize(likelihoodEnd);
  if (iterations > 0) {
    logLikelihoodW.subvec(it, likelihoodEnd - 1).fill(NA_REAL);
    logLikelihoodL1.subvec(it, likelihoodEnd - 1).fill(NA_REAL);
    logLikelihoodL2.subvec(it, likelihoodEnd - 1).fill(NA_REAL);
  }

  // RcppProgress tracking object
  Progress p(iterations, displayProgress);

  // Temporarily shift the R-backed vectors to zero-based indexing. The guard
  // restores them on normal return, interruption, and exception unwinding.
  RIndexGuard indexGuard(intTokens, za);

  DEBUG_MSG2("Branching to loops");


  // branch to correct loop
  int LDAruns = 0;
  ///////////////////////////////////////////////////
  if ( (L2 == 1) && (reverse == true) ) {LDAruns = iterations;} //branch to LDA if no sentiment and not JST model
  //////////////////////////////////////////////////
  DEBUG_MSG2("LDA runs : "<< LDAruns);
  DEBUG_MSG2("non-LDA runs : "<< (iterations - LDAruns));

  if (LDAruns > 0) { iterateLDA(0, LDAruns, computeLikelihood, p); DEBUG_MSG2("LDA performed");}

  if ((iterations - LDAruns) > 0) { iteratel2(LDAruns, iterations - LDAruns, computeLikelihood, p); DEBUG_MSG2("non-LDA performed");}

  // An interrupted run may complete fewer iterations than requested. Remove
  // the unexecuted tail rather than exposing phantom likelihood entries.
  logLikelihoodW.resize(it);
  logLikelihoodL1.resize(it);
  logLikelihoodL2.resize(it);
}

void model::iterateLDA(uword start, uword iterations, bool computeLikelihood, Progress& p) {

  // re-initialize sum and count structures
  sumBeta = sum(beta, 1); // still needed for likelihood
  if (computeLikelihood) { cacheBetaTerms(); }

  sumAlpha = trans(sum(alpha, 0));
  sumGamma = mat(L1, C);
  for (uword t = 0; t < L1; t++){sumGamma.row(t) = sum(gamma.rows(0 + t*L2, L2-1 + t*L2), 0);}
  DEBUG_MSG2("Re-initialized sum structures");

  l1d = cpp_rebuild_l1d(za, L1*L2, L2);
  l1w = cpp_rebuild_l1w(intTokens, za, L1*L2, V, L2);
  count_l1 = sum(l1w, 1);
  count_d = trans(sum(l1d, 0));
  DEBUG_MSG2("Re-initialized count structures");

  // initialize prob vectors
  l1Probs = vec(L1);
  CACHEDl1Probs = vec(L1);

  // iterateLDA() is only dispatched when L2 == 1. Work directly with the
  // underlying column-major storage to avoid checked Armadillo access in the
  // per-token, per-topic hot loop.
  auto* const l1dPtr = l1d.memptr();
  auto* const l1wPtr = l1w.memptr();
  auto* const countL1Ptr = count_l1.memptr();
  const double* const alphaPtr = alpha.memptr();
  const double* const betaPtr = beta.memptr();
  const double* const sumBetaPtr = sumBeta.memptr();
  double* const cachedL1ProbsPtr = CACHEDl1Probs.memptr();
  double* const l1ProbsPtr = l1Probs.memptr();

  // LDA() initializes beta to a single value, but converted or merged models
  // can carry a non-uniform matrix. Detect the common case once so its hot
  // loop does not need to stream a beta column for every token.
  const double scalarBeta = betaPtr[0];
  bool hasScalarBeta = true;
  for (uword i = 1; i < beta.n_elem; ++i) {
    if (betaPtr[i] != scalarBeta) {
      hasScalarBeta = false;
      break;
    }
  }
  const double scalarSumBeta = sumBetaPtr[0];

  const auto sampleDocument = [&](uword d, auto scalarBetaTag) {
    constexpr bool useScalarBeta = decltype(scalarBetaTag)::value;

    auto* const documentCounts = l1dPtr + d * L1;

    DEBUG_MSG("Probabilities caching...");

    // caching ps_td and pt_d for document
    for (uword t = 0; t < L1; t++){
      const double betaSum = useScalarBeta ? scalarSumBeta : sumBetaPtr[t];
      cachedL1ProbsPtr[t] = (documentCounts[t] + alphaPtr[t]) /
        (countL1Ptr[t] + betaSum);
    }
    DEBUG_MSG("Cached probs for document: " << d);
    DEBUG_MSG(trans(CACHEDl1Probs));

    const uword size = intTokens[d]->size();
    const uword* const words = intTokens[d]->memptr();
    uword* const assignments = za[d]->memptr();

    // per word sampling
    for (uword w = 0; w < size; w++){
      DEBUG_MSG("Word/document index: [" << d << "]-" << w);
      const uword word = words[w];
      const uword oldTopic = assignments[w];
      auto* const wordCounts = l1wPtr + word * L1;
      const double* const wordBeta = betaPtr + word * L1;
      const double oldCachedProb = cachedL1ProbsPtr[oldTopic];

      DEBUG_MSG("Vocabulary index : (" << word << ")");

      // Remove the current assignment from the leave-one-out counts.
      --documentCounts[oldTopic];
      --wordCounts[oldTopic];
      --countL1Ptr[oldTopic];
      const double oldTopicBetaSum = useScalarBeta ?
        scalarSumBeta : sumBetaPtr[oldTopic];
      cachedL1ProbsPtr[oldTopic] =
        (documentCounts[oldTopic] + alphaPtr[oldTopic]) /
        (countL1Ptr[oldTopic] + oldTopicBetaSum);

      // Build the cumulative conditional distribution over topics.
      double cumulative = 0.0;
      for (uword t = 0; t < L1; ++t) {
        const double betaValue = useScalarBeta ? scalarBeta : wordBeta[t];
        cumulative += cachedL1ProbsPtr[t] * (wordCounts[t] + betaValue);
        l1ProbsPtr[t] = cumulative;
      }

      const double draw = unif_rand() * cumulative;
      uword newTopic = oldTopic;

      for (uword t = 0; t < L1; ++t) {
        if (l1ProbsPtr[t] > draw) {
          newTopic = t;
          break;
        }
      }

      assignments[w] = newTopic;
      ++documentCounts[newTopic];
      ++wordCounts[newTopic];
      ++countL1Ptr[newTopic];

      if (newTopic == oldTopic) {
        // All counts returned to their pre-sampling values, so the cached
        // probability is already available without a second division.
        cachedL1ProbsPtr[oldTopic] = oldCachedProb;
      } else {
        const double newTopicBetaSum = useScalarBeta ?
          scalarSumBeta : sumBetaPtr[newTopic];
        cachedL1ProbsPtr[newTopic] =
          (documentCounts[newTopic] + alphaPtr[newTopic]) /
          (countL1Ptr[newTopic] + newTopicBetaSum);
      }

      DEBUG_MSG("Word re-sampled");
    }
  };

  for (uword i = start; i < (iterations + start); i++){

    if (Progress::check_abort()) {
      Rcpp::Rcout << "Process aborted at iteration " << i << std::endl;
      return;
    }

    DEBUG_MSG("it = " << it << " . Starting sampling()\n");
    for (uword d = 0; d < D; d++){
      DEBUG_MSG("Starting document: " << d);

      if (hasScalarBeta) {
        sampleDocument(d, std::true_type{});
      } else {
        sampleDocument(d, std::false_type{});
      }

      DEBUG_MSG("Ended document: " << d);
    }

    // updating alphas every alphaCycle
    if (alphaCycle > 0) {
      if ((it + 1) % alphaCycle == 0) {
        updateAlpha();
      }
    }


    if (computeLikelihood) {

      // functions are based on zd, zw, count_z and l1d. Need to update since sampling does not
      zd = cpp_rebuild_zd(za, L1*L2);
      zw = cpp_rebuild_zw(intTokens, za, L1*L2, V);
      count_z = sum(zd, 1);

      // computing likelihoods
      DEBUG_MSG("Likelihood W");
      logLikelihoodW(it) = computeLogLikelihoodW();
      DEBUG_MSG("Likelihood L1");
      logLikelihoodL1(it) = computeLogLikelihoodL1();
      DEBUG_MSG("Likelihood L2");
      logLikelihoodL2(it) = computeLogLikelihoodL2();
    }

    // increment internal counter & RcppProgress object
    ++it;
    p.increment();
  }
}



void model::iteratel2(uword start, uword iterations, bool computeLikelihood, Progress& p) {


  // re-initialize sum and count structures
  sumBeta = sum(beta, 1);
  if (computeLikelihood) { cacheBetaTerms(); }
  sumAlpha = trans(sum(alpha, 0));
  sumGamma = mat(L1, C);
  for (uword t = 0; t < L1; t++){sumGamma.row(t) = sum(gamma.rows(0 + t*L2, L2-1 + t*L2), 0);}
  DEBUG_MSG2("Re-initialized sum structures");

  zw = cpp_rebuild_zw(intTokens, za, L1*L2, V);
  zd = cpp_rebuild_zd(za, L1*L2);
  count_z = sum(zd, 1);
  count_d = trans(sum(zd, 0));

  l1d = imat(L1, D);
  for(uword t = 0; t < L1; t++){l1d.row(t) = sum(zd.rows(0 + t*L2, L2-1 + t*L2), 0);}

  DEBUG_MSG2("Re-initialized count structures");

  // initialize prob vectors
  zProbs = vec(L1*L2);
  CACHEDzProbs = vec(L1*L2);


  for (uword i = start; i < (iterations + start); i++){

    if (Progress::check_abort()) {
      Rcpp::Rcout << "Process aborted at iteration " << i << std::endl;
      return;
    }

    DEBUG_MSG("it = " << it << " . Starting sampling()\n");
    for (uword d = 0; d < D; d++){
      DEBUG_MSG("Starting document: " << d);

      // store class number of current document
      uword c = 0;

      DEBUG_MSG("Probabilities caching...");

      // caching ps_td and pt_d for document
      for (uword t = 0; t < L1; t++){
        refreshCachedZProbs(t, c, d);
      }

      DEBUG_MSG("Cached probs for document: " << d);
      DEBUG_MSG(trans(CACHEDzProbs));

      uword size = intTokens[d]->size();
      // per word sampling
      for (uword w = 0; w < size; w++){
        DEBUG_MSG("Word/document index: [" << d << "]-" << w);
        DEBUG_MSG("Vocabulary index : (" << intTokens[d]->at(w) << ")");
        sampling(intTokens[d]->at(w), za[d]->at(w), c, d);
        DEBUG_MSG("Word re-sampled");
      }

      DEBUG_MSG("Ended document: " << d);
    }

    // updating alphas every alphaCycle
    if (alphaCycle > 0) {
      if ((it + 1) % alphaCycle == 0) {
        updateAlpha();
      }
    }

    if (gammaCycle > 0) {
      if ((it + 1) % gammaCycle == 0) {
        updateGamma();
      }
    }


    if (computeLikelihood) {
      DEBUG_MSG("Likelihood W");
      logLikelihoodW(it) = computeLogLikelihoodW();
      DEBUG_MSG("Likelihood L1");
      logLikelihoodL1(it) = computeLogLikelihoodL1();
      DEBUG_MSG("Likelihood L2");
      logLikelihoodL2(it) = computeLogLikelihoodL2();
    }


    // increment internal counter & RcppProgress object
    ++it;
    p.increment();
  }
}


// refresh cached probabilities sharing the same first-layer label
void model::refreshCachedZProbs(uword l1, uword c, uword d) {

  const double l1Prob = (l1d(l1, d) + alpha(l1, c)) /
    (l1d(l1, d) + sumGamma(l1, c));

  for (uword s = 0; s < L2; s++) {
    const uword z = l1 * L2 + s;
    CACHEDzProbs(z) = l1Prob *
      (zd(z, d) + gamma(z, c)) / (count_z(z) + sumBeta(z));
  }
}


// sample a new topic and sentiment for a word
void model::sampling(const uword& word, uword& zLeave, uword c, uword d)  {

  // uniform random draw
  double u;

  uword l1 = zLeave / L2;
  // uword l2 = zLeave % L2;

  // // remove current word from count structures
  zw(zLeave, word) -= 1;
  zd(zLeave, d) -= 1;
  l1d(l1, d) -=1;
  count_z(zLeave) -= 1;
  // count_d(d) -= 1; // doesn't change following assignment anyway


  // Removing the word changes the first-layer factor shared by all L2 entries
  // below l1, as well as the second-layer factor for zLeave.
  refreshCachedZProbs(l1, c, d);

  DEBUG_MSG("Cached probs before sampling");
  DEBUG_MSG(trans(CACHEDzProbs));


  zProbs(0) = CACHEDzProbs(0) * (zw(0, word) + beta(0, word));
  for (uword z = 1; z < L1*L2; z++) {
    zProbs(z) = zProbs(z-1) + CACHEDzProbs(z) * (zw(z, word) + beta(z, word));
  }

  DEBUG_MSG("Cumulated probabilities");
  DEBUG_MSG(trans(zProbs));


  // sampling a new topic and sentiment

  DEBUG_MSG("Pre sample");
  DEBUG_MSG("z: " << zLeave);

  u  = unif_rand() * zProbs(L2 * L1 - 1);
  for (uword z = 0; z < L1*L2 ; z ++) {
    if(zProbs(z) >  u ) {
      zLeave = z;
      break;
    }
  }

  DEBUG_MSG("Post sample");
  DEBUG_MSG("z: " << zLeave);


  l1 = zLeave / L2;

  // update count structures
  zw(zLeave, word) += 1;
  zd(zLeave, d) += 1;
  l1d(l1, d) +=1;
  count_z(zLeave) += 1;
  // count_d(d) += 1; // doesn't change following assignment anyway

  DEBUG_MSG("Caching probabilities");

  // Adding the word likewise changes every cached entry below the sampled l1.
  refreshCachedZProbs(l1, c, d);

  DEBUG_MSG("Cached probs after sampling");
  DEBUG_MSG(trans(CACHEDzProbs));
}


// estimation of the parameter alpha
void model::updateAlpha() {

  DEBUG_MSG("Updating alpha");
  bool discarded_update = false;

  if (D == 0) {
    return;
  }

  uvec index;  // temporary index of document belonging in a given class
  imat sub_td;  // subset of tsd for a given class
  uword sub_D;   // number of document belonging to a given class

  for (uword c = 0; c < C; c++){

    DEBUG_MSG("Recomputing alpha for class: " << c);

    // identify documents belonging to a given class
    index = linspace<uvec>(0, D-1);
    DEBUG_MSG("Index:");
    DEBUG_MSG(trans(index));

    sub_D = index.size();;
    sub_td = l1d.cols(index);

    DEBUG_MSG("Recomputing alpha for class: " << c);
    // preparing input for polya_fit_simple()
    // storage owns the rows; data provides the int** view expected by polya_fit_simple()
    // alpha_temp replicates alpha for a given c
    std::vector<std::vector<int>> storage(L1, std::vector<int>(sub_D));
    std::vector<int*> data(L1);
    std::vector<double> alpha_temp(L1);
    for (uword t = 0; t < L1; t++) {

      alpha_temp[t] = alpha(t, c);

      for (uword d = 0; d < sub_D; d++) {
        storage[t][d] = sub_td(t, d);
      }
      data[t] = storage[t].data();
    }

    DEBUG_MSG("Current alpha:");
    DEBUG_MSG(trans(alpha.col(c)));
    // Rcout << alpha << "\n";

    // estimate a new alpha_temp based on matrix data
    polya_fit_simple(data.data(), alpha_temp.data(), L1, sub_D);
    DEBUG_MSG("New alpha:");
    DEBUG_MSG(arma::rowvec(alpha_temp.data(), L1, false, false));

    // Do not copy invalid optimizer output into the model. A class or topic
    // with no observations can make the fixed-point update undefined; in
    // that case retain the previous valid hyperparameters.
    if (validDirichletParameters(alpha_temp)) {
      for (uword t = 0; t < L1; t++){
        alpha(t, c) = alpha_temp[t];
      }
    } else {
      discarded_update = true;
    }
    sumAlpha(c) = sum(alpha.col(c));
  }

  if (discarded_update) {
    Rcpp::warning(
      "Invalid alpha hyperparameter update discarded; retaining previous values."
    );
  }
}



// estimation of the parameter gamma
void model::updateGamma() {

  bool discarded_update = false;

  if (D == 0) {
    return;
  }

  uvec index;    // temporary index of document belonging in a given class
  imat sub_tsd;  // subset of tsd for a given class
  uword sub_D;     // number of document belonging to a given class

  for (uword c = 0; c < C; c++){

    // identify documents belonging to a given class
    index = linspace<uvec>(0, D-1);
    sub_D = index.size();
    sub_tsd = zd.cols(index);

    // preparing input for polya_fit_simple()
    // storage owns the rows; data provides the int** view expected by polya_fit_simple()
    // gamma_temp replicates gamma for a given c and t

    // initializing data and gamma_temp sizes
    std::vector<std::vector<int>> storage(L2, std::vector<int>(sub_D, 0));
    std::vector<int*> data(L2);
    std::vector<double> gamma_temp(L2, 0.0);
    for (uword s = 0; s < L2; s++) {
      data[s] = storage[s].data();
    }

    // looping on t as a vector gamma is estimated once per t
    for (uword t = 0; t<L1; t++){

      // populating gamma_temp and data for a given t
      for (uword s = 0; s < L2; s++) {
        gamma_temp[s] = gamma(t*L2 + s, c);
        for (uword d = 0; d < sub_D; d++) {
          storage[s][d] = sub_tsd(t*L2 + s, d);
        }
      }

      // estimate a new gamma_temp based on matrix data
      polya_fit_simple(data.data(), gamma_temp.data(), L2, sub_D);

      // Do not copy invalid optimizer output into the model. A topic with no
      // observations makes the fixed-point update undefined; retain its
      // previous valid gamma values instead.
      if (validDirichletParameters(gamma_temp)) {
        for (uword s = 0; s < L2; s++) {
          gamma(t*L2 + s, c) = gamma_temp[s];
        }
      } else {
        discarded_update = true;
      }
    }
  }
  for (uword t = 0; t < L1; t++){sumGamma.row(t) = sum(gamma.rows(0 + t*L2, L2-1 + t*L2), 0);}

  if (discarded_update) {
    Rcpp::warning(
      "Invalid gamma hyperparameter update discarded; retaining previous values."
    );
  }
}

void model::cacheBetaTerms() {
  const double epsilon = 0.000000001;
  lgammaBeta = lgamma(beta + epsilon);
  lgammaSumBeta = lgamma(sumBeta + epsilon);
  sumLgammaBeta = sum(lgammaBeta, 1);
}

// compute component p(w|t,s) of the likelihood
double model::computeLogLikelihoodW() {

  const double epsilon = 0.000000001; // a very small epsilon is added to avoid lgamma(0)
  const uword Z = L1 * L2;

  double logLik = 0; // initialize the likelihood

  const auto* zwPtr = zw.memptr();
  const double* betaPtr = beta.memptr();
  const double* lgammaBetaPtr = lgammaBeta.memptr();
  vec numTwo(Z, fill::zeros);
  double* const numTwoPtr = numTwo.memptr();

  // Armadillo stores matrices column-major. Traverse each word column
  // contiguously while retaining word-order accumulation for every z.
  for (uword w = 0; w < V; w++) {
    for (uword z = 0; z < Z; z++) {
      const auto count = *zwPtr++;
      const double betaValue = *betaPtr++;
      numTwoPtr[z] += (count == 0) ?
        *lgammaBetaPtr :
        lgamma(count + betaValue + epsilon);
      ++lgammaBetaPtr;
    }
  }

  const double* const lgammaSumBetaPtr = lgammaSumBeta.memptr();
  const double* const sumLgammaBetaPtr = sumLgammaBeta.memptr();
  const auto* const countZPtr = count_z.memptr();
  const double* const sumBetaPtr = sumBeta.memptr();
  for (uword z = 0; z < Z; z++) {
    logLik += lgammaSumBetaPtr[z] + numTwoPtr[z] - sumLgammaBetaPtr[z] -
      lgamma(countZPtr[z] + sumBetaPtr[z]);
  }


  return(logLik);
}



// compute component p(s|t) of the likelihood
double model::computeLogLikelihoodL2() {

  const double epsilon = 0.000000001; // a very small epsilon is added to avoid lgamma(0)

  double logLik = 0; // initialize the likelihood

  // computation is done in four subcomponents. two variables for the numerator
  // and two for the denominator
  double numOne;
  double numTwo;
  double denomOne;
  double denomTwo;

  // There is currently a single document class, so c is constant throughout.
  const uword c = 0;

  // These terms vary by first-layer label, but not by document.
  vec lgammaGamma = lgamma(gamma.col(c) + epsilon);
  vec numOneByL1(L1);
  vec denomOneByL1(L1);
  for (uword t = 0; t < L1; t++) {
    numOneByL1(t) = lgamma(sumGamma(t, c));
    denomOneByL1(t) = sum(lgammaGamma.rows(t*L2, L2-1 + t*L2));
  }

  const auto* zdPtr = zd.memptr();
  const auto* l1dPtr = l1d.memptr();
  const double* const gammaPtr = gamma.memptr();
  const double* const lgammaGammaPtr = lgammaGamma.memptr();
  const double* const sumGammaPtr = sumGamma.memptr();

  for (uword d = 0; d < D; d++) {

    uword z = 0;
    for (uword t = 0; t < L1; t++) {

      numOne = numOneByL1(t);
      denomOne = denomOneByL1(t);

      numTwo = 0;
      for (uword s = 0; s < L2; s++) {
        const auto count = *zdPtr++;
        numTwo += (count == 0) ?
          lgammaGammaPtr[z] :
          lgamma(count + gammaPtr[z] + epsilon);
        ++z;
      }
      denomTwo = lgamma(*l1dPtr++ + sumGammaPtr[t]);

      // increments the log-likelihood with the result of the four subcomponents
      logLik += numOne + numTwo - denomOne - denomTwo;
    }
  }
  return(logLik);
}

// compute component p(t) of the likelihood
double model::computeLogLikelihoodL1() {

  const double epsilon = 0.000000001; // a very small epsilon is added to avoid lgamma(0)

  double logLik = 0; // initialize the likelihood

  // computation is done in four subcomponents. two variables for the numerator
  // and two for the denominator
  double numOne;
  double numTwo;
  double denomOne;
  double denomTwo;

  // There is currently a single document class, so c is constant throughout.
  const uword c = 0;

  // These terms do not depend on the document.
  vec lgammaAlpha = lgamma(alpha.col(c) + epsilon);
  const double numOneConst = lgamma(sumAlpha(c));
  const double denomOneConst = sum(lgammaAlpha);
  const auto* l1dPtr = l1d.memptr();
  const double* const alphaPtr = alpha.memptr();
  const double* const lgammaAlphaPtr = lgammaAlpha.memptr();
  const auto* const countDPtr = count_d.memptr();

  for (uword d = 0; d < D; d++) {

    numOne = numOneConst;
    denomOne = denomOneConst;

    numTwo = 0;
    for (uword t = 0; t < L1; t++) {
      const auto count = *l1dPtr++;
      numTwo += (count == 0) ?
        lgammaAlphaPtr[t] :
        lgamma(count + alphaPtr[t] + epsilon);
    }
    denomTwo = lgamma(countDPtr[d] + sumAlpha(c));

    // increments the log-likelihood with the result of the four subcomponents
    logLik += numOne + numTwo - denomOne - denomTwo;
  }
  return(logLik);
}

SEXP model::get_za() {
  SEXP tmp = PROTECT(Rf_allocVector(VECSXP, za.size()));
  for (uword d = 0; d < za.size(); d++) {
    SEXP value = PROTECT(wrap(arma::conv_to<arma::ivec>::from(*(za[d]))));
    SET_VECTOR_ELT(tmp, d, value);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return tmp;
}


SEXP model::get_intTokens() {
  SEXP tmp = PROTECT(Rf_allocVector(VECSXP, intTokens.size()));
  for (uword d = 0; d < intTokens.size(); d++) {
    SEXP value = PROTECT(wrap(arma::conv_to<arma::ivec>::from(*(intTokens[d]))));
    SET_VECTOR_ELT(tmp, d, value);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return tmp;
}
