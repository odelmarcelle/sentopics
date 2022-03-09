// TODO : if model is perfectly functional, try to disable Armadillo checks
// #define ARMA_NO_DEBUG

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
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

// constructor default values
void model::set_default_values(bool reversed_) {
  reversed = reversed_;

  V = 1;
  L1 = 1;
  L2 = 1;
  D = 1;
  it = 0;
  initLDA = 0;

  smooth = false;
  smooth_factor = 0;

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
                 uword gammaCycle_,
                 uword initLDA_,
                 uword smooth_
) {
  V = V_;
  L1 = L1_;
  L2 = L2_;
  D = LENGTH(intTokens_);
  C = 1;
  it = 0;
  initLDA = initLDA_;
  smooth = smooth_;

  intTokens.resize(D);
  za.resize(D);
  for (R_xlen_t i = 0; i < XLENGTH(intTokens_); i++) {
    int len = LENGTH(VECTOR_ELT(intTokens_, i));
    intTokens[i] =  std::unique_ptr<arma::uvec>(new arma::uvec( reinterpret_cast<arma::uword*>(INTEGER(VECTOR_ELT(intTokens_, i))), len, false, true)); // ok because clean in models.R is not out of scope
    za[i] =  std::unique_ptr<arma::uvec>(new arma::uvec( reinterpret_cast<arma::uword*>(INTEGER(VECTOR_ELT(za_, i))), len, false, true));
  }

  lexicon = lexicon_;
  double initBeta = beta_;;
  alpha = alpha_;
  gamma = gamma_;

  alphaCycle = alphaCycle_;
  gammaCycle = gammaCycle_;

  // initialize beta
  initBetaLex(initBeta);

  // initialize topic assignments, sentiment assignments and count structures
  initAssignments();
}


void model::initBetaLex(double initBeta) {

  beta = arma::mat(L1*L2, V);
  beta.fill(initBeta);

  for (uword w = 0; w < V; w++){

    // provided lexicon is an R object containing values of either 0:L2 or NA
    // in the IntegerVector type, NA is stored as -2147483648, the smallest integer value
    // hence, NAs are identified by simply testing -2147483648
    // WARNING : any operation on the vector such as += 1 will break this behavior
    if(lexicon(w) != INT_MIN){

      if(reversed){
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

  uvec zSet = regspace<uvec>(0, (L1*L2)-1);

  DEBUG_MSG("Zset : " << trans(zSet));
  DEBUG_MSG("First RNG draws... : " << trans(Rcpp::RcppArmadillo::sample(zSet, 10, true)));

  for (uword d = 0; d < D; d++){

    DEBUG_MSG("Starting document " << d << "...");
    *intTokens[d] -=1;

    uword W = intTokens[d]->size();

    DEBUG_MSG("za[d] before draw: " << trans(*za[d]) );

    *za[d] = Rcpp::RcppArmadillo::sample(zSet, W, true);
    DEBUG_MSG("za[d] after draw: " << trans(*za[d]) );
    for (uword w = 0; w < W; w++){
      if ( lexicon(intTokens[d]->at(w)) != INT_MIN ) {
        za[d]->at(w) = as_scalar(Rcpp::RcppArmadillo::sample(zSet, 1, true, beta.col(intTokens[d]->at(w)) ));
      }
    }

    // reverse indexing correction
    *za[d] += 1;
    *intTokens[d] += 1;
  }
}

// rebuild the model from R input
void model::rebuild(uword V_,
                    uword L1_,
                    uword L2_,
                    uword D_,
                    uword C_,
                    uword it_,
                    uword initLDA_,
                    uword smooth_,
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

  // imported variables
  V = V_;
  L1 = L1_;
  L2 = L2_;
  D = D_;
  C = C_;
  it = it_;
  initLDA = initLDA_;
  smooth = smooth_;
  if (smooth + initLDA < it) {smooth = false;}
  initBeta = initBeta_;
  intTokens.resize(LENGTH(intTokens_));
  za.resize(LENGTH(za_));
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
    *intTokens[i] -= 1;
    *za[i] -= 1;
  }
  arma::imat zw = cpp_rebuild_zw(intTokens, za, Z, V);
  // reverse indexing correction for R object
  for (R_xlen_t i = 0; i < XLENGTH(intTokens_); i++) {
    *intTokens[i] += 1;
    *za[i] += 1;
  }
  return(zw);
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
    *za[i] -= 1;
  }
  arma::imat zd = cpp_rebuild_zd(za, Z);
  // reverse indexing correction for R object
  for (R_xlen_t i = 0; i < XLENGTH(za_); i++) {
    *za[i] += 1;
  }
  return(zd);
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
  logLikelihoodW.resize(it + iterations);
  logLikelihoodL1.resize(it + iterations);
  logLikelihoodL2.resize(it + iterations);

  // RcppProgress tracking object
  Progress p(iterations, displayProgress);

  // correct indexing
  for (uword d = 0; d < D; d++){
    *intTokens[d] -= 1;
    *za[d] -= 1;
  }

  DEBUG_MSG2("Branching to loops");


  // branch to correct loop
  int LDAruns = std::min( std::max( (int) (initLDA - it), 0), (int) iterations );
  ///////////////////////////////////////////////////
  if ( (L2 == 1) & (reversed == true) ) {LDAruns = iterations;} //branch to LDA if no sentiment and not JST model
  //////////////////////////////////////////////////
  DEBUG_MSG2("LDA runs : "<< LDAruns);
  DEBUG_MSG2("non-LDA runs : "<< (iterations - LDAruns));

  if (LDAruns > 0) { iterateLDA(0, LDAruns, computeLikelihood, p); DEBUG_MSG2("LDA performed");}

  if ((iterations - LDAruns) > 0) { iteratel2(LDAruns, iterations - LDAruns, computeLikelihood, p); DEBUG_MSG2("non-LDA performed");}

  // reverse indexing correction
  for (uword d = 0; d < D; d++){
    *intTokens[d] += 1;
    *za[d] += 1;
  }


  // classVector = classVector + 1; // reversing index correction
}

void model::iterateLDA(uword start, uword iterations, bool computeLikelihood, Progress& p) {

  // re-initialize sum and count structures
  sumBeta = sum(beta, 1); // still needed for likelihood
  // beta need special treatment to reach L1 x V dimension
  L1beta = mat(L1, V);
  for (uword t = 0; t < L1; t++){L1beta.row(t) = sum(beta.rows(0 + t*L2, L2-1 + t*L2), 0);}
  L1sumBeta = sum(L1beta, 1);

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

  for (uword i = start; i < (iterations + start); i++){

    if (Progress::check_abort()) {
      Rcpp::Rcout << "Process aborted at iteration " << i << std::endl;
      return;
    }

    if (smooth) {
      if (smooth < it) {
        smooth_factor = 0;
      } else {
        smooth_factor = pow(0.01, ((double) it) / (double) smooth);
      }
    }
    DEBUG_MSG("it = " << it << " . Starting sampling()\n");
    for (uword d = 0; d < D; d++){
      DEBUG_MSG("Starting document: " << d);

      // store class number of current document
      uword c = 0;

      DEBUG_MSG("Probabilities caching...");

      // caching ps_td and pt_d for document
      for (uword t = 0; t < L1; t++){
        CACHEDl1Probs(t) = (l1d(t, d) + alpha(t, c)) / (count_l1(t) + L1sumBeta(t));
      }
      DEBUG_MSG("Cached probs for document: " << d);
      DEBUG_MSG(trans(CACHEDl1Probs));

      uword size = intTokens[d]->size();
      // per word sampling
      for (uword w = 0; w < size; w++){
        DEBUG_MSG("Word/document index: [" << d << "]-" << w);
        DEBUG_MSG("Vocabulary index : (" << intTokens[d]->at(w) << ")");
        samplingLDA(intTokens[d]->at(w), za[d]->at(w), c, d);
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

    if (smooth) {
      if (smooth + initLDA < it) {
        smooth = false;
      } else {
        smooth_factor = pow(0.01, ((double) it - (double) initLDA) / (double) smooth);
      }
    }

    DEBUG_MSG("it = " << it << " . Starting sampling()\n");
    for (uword d = 0; d < D; d++){
      DEBUG_MSG("Starting document: " << d);

      // store class number of current document
      uword c = 0;

      DEBUG_MSG("Probabilities caching...");

      double tmpL1;
      double tmpL2;

      // caching ps_td and pt_d for document
      for (uword t = 0; t < L1; t++){
        tmpL1 = (l1d(t, d) + alpha(t, c)) / (l1d(t, d) + sumGamma(t, c));
        for (uword s = 0; s < L2; s++) {
          tmpL2 = (zd(t*L2 + s, d) + gamma(t*L2 + s, c)) / (count_z(t*L2 + s) + sumBeta(t*L2 + s));
          CACHEDzProbs(t*L2 + s) = tmpL1 * tmpL2;
        }
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

    if ( (it - initLDA - smooth + 1) > 0 ) {
      if (gammaCycle > 0) {
        if ((it + 1) % gammaCycle == 0) {
          updateGamma();
        }
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


  // Computation of stProbs taking advantage of cached results. Non-word dependent element access are pre-computed for the document
  // Instead of recomputing these element once for each topic/sentiment, only the element of CACHEDstProbs corresponding to the
  // current topic/sentiment of the word is updated. After the re-sampling, CACHEDstProbs is updated for the sampled topic/sentiment
  CACHEDzProbs(zLeave) = (l1d(l1, d) + alpha(l1, c)) / (l1d(l1, d) + sumGamma(l1, c)) *
    (zd(zLeave, d) + gamma(zLeave, c)) / (count_z(zLeave) + sumBeta(zLeave));

  DEBUG_MSG("Cached probs before sampling");
  DEBUG_MSG(trans(CACHEDzProbs));

  if (smooth) {
    if (initLDA == 0) {
      double noise;

      for (uword z = 0; z < L1*L2; z++) {
        zProbs(z) = CACHEDzProbs(z) * (zw(z, word) + beta(z, word));
      }
      DEBUG_MSG("Probs array before smoothing");
      DEBUG_MSG(zProbs);


      if (lexicon(word) == INT_MIN) {
        noise = accu(zProbs) / (L1*L2);
        for (uword z = 0; z < L1*L2; z++) {
          zProbs(z) = (1 - smooth_factor) * zProbs(z) + smooth_factor * noise;
        }
      } else {
        // this is different for rJST and JST
        if (reversed) {
          // rJST version
          uword lex = lexicon(word);
          noise = accu(zProbs) / L1;
          for (uword tmpL1 = 0; tmpL1 < L1; tmpL1++) {
            zProbs(tmpL1 * L2 + lex) = (1 - smooth_factor) * zProbs(tmpL1 * L2 + lex) + smooth_factor * noise;
          }
        }
        else {
          // JST version: probabilities are left intact as lexicon words
          // cannot change L1 anyway
        }
      }



      DEBUG_MSG("Probs array after smoothing");
      DEBUG_MSG(trans(zProbs));

      for (uword z = 1; z < L1*L2; z++) {
        zProbs(z) += zProbs(z-1);
      }
      DEBUG_MSG("Cumulated probabilities");
      DEBUG_MSG(trans(zProbs));
    } else {

      for (uword z = 0; z < L1*L2; z++) {
        zProbs(z) = CACHEDzProbs(z) * (zw(z, word) + beta(z, word));
      }
      DEBUG_MSG("Probs array before smoothing");
      DEBUG_MSG(trans(zProbs));

      if (lexicon(word) == INT_MIN) {
        arma::vec noise2 = arma::vec(L1);

        for (uword tmpL1 = 0; tmpL1 < L1; tmpL1++) {
          noise2(tmpL1) = accu(zProbs(arma::span( 0 + L2*tmpL1, L2-1 + L2*tmpL1 ))) / L2;
        }
        for (uword z = 0; z < L1*L2; z++) {
          zProbs(z) = (1 - smooth_factor) * zProbs(z) + smooth_factor * noise2(z / L2);
        }
      }

      DEBUG_MSG("Probs array after smoothing");
      DEBUG_MSG(trans(zProbs));

      for (uword z = 1; z < L1*L2; z++) {
        zProbs(z) += zProbs(z-1);
      }

      DEBUG_MSG("Cumulated probabilities");
      DEBUG_MSG(trans(zProbs));
    }
  } else {

    zProbs(0) = CACHEDzProbs(0) * (zw(0, word) + beta(0, word));
    for (uword z = 1; z < L1*L2; z++) {
      zProbs(z) = zProbs(z-1) + CACHEDzProbs(z) * (zw(z, word) + beta(z, word));
    }

    DEBUG_MSG("Cumulated probabilities");
    DEBUG_MSG(trans(zProbs));
  }

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

  CACHEDzProbs(zLeave) = (l1d(l1, d) + alpha(l1, c)) / (l1d(l1, d) + sumGamma(l1, c)) *
    (zd(zLeave, d) + gamma(zLeave, c)) / (count_z(zLeave) + sumBeta(zLeave));

  DEBUG_MSG("Cached probs after sampling");
  DEBUG_MSG(trans(CACHEDzProbs));
}


// sample a new topic and sentiment for a word
void model::samplingLDA(const uword& word, uword& zLeave, uword c, uword d)  {

  // uniform random draw
  double u;

  uword l1 = zLeave / L2;
  uword l2 = zLeave % L2;

  DEBUG_MSG("l1w before sampling: " << trans(l1w.col(word)));
  DEBUG_MSG(size(L1beta));
  DEBUG_MSG("l1Beta for current word: " << trans(L1beta.col(word)));

  // update count structures
  l1d(l1, d) -= 1;
  l1w(l1, word) -= 1;
  count_l1(l1) -= 1;

  CACHEDl1Probs(l1) = (l1d(l1, d) + alpha(l1, c)) / (count_l1(l1) + L1sumBeta(l1));

  DEBUG_MSG("Cached probs before sampling");
  DEBUG_MSG(trans(CACHEDl1Probs));

  DEBUG_MSG("count_l1");
  DEBUG_MSG(trans(count_l1));



  if (smooth_factor > 0) {

    for (uword t = 0; t < L1; t++) {
      l1Probs(t) = CACHEDl1Probs(t) * (l1w(t, word) + L1beta(t, word));
    }
    DEBUG_MSG("Probs before smoothing");
    DEBUG_MSG(trans(l1Probs));

    double noise;
    if (reversed | (lexicon(word) == INT_MIN)) {
      noise = accu(l1Probs) / L1;
      for (uword t = 0; t < L1; t++) {
        l1Probs(t) = (1 - smooth_factor) * l1Probs(t) + smooth_factor * noise;
      }
    } else {
      // lexicon words are not able to change L1 anyway in JST, no smooth applied
    }


    DEBUG_MSG("Probs array after smoothing");
    DEBUG_MSG(trans(l1Probs));

    for (uword t = 1; t < L1; t++) {
      l1Probs(t) += l1Probs(t-1);
    }
    DEBUG_MSG("Cumulated probabilities");
    DEBUG_MSG(trans(l1Probs));

  } else {

    l1Probs(0) = CACHEDl1Probs(0) * (l1w(0, word) + L1beta(0, word));
    for (uword t = 1; t < L1; t++) {
      l1Probs(t) = l1Probs(t-1) + CACHEDl1Probs(t) * (l1w(t, word) + L1beta(t, word));
    }
    DEBUG_MSG("Cumulated probabilities");
    DEBUG_MSG(trans(l1Probs));
  }


  // sampling a new topic and sentiment

  DEBUG_MSG("Pre sample");
  DEBUG_MSG("z: " << zLeave);
  DEBUG_MSG("l1: " << l1);

  u  = unif_rand() * l1Probs(L1 - 1);
  for (uword t = 0; t < L1 ; t++) {
    if(l1Probs(t) >  u ) {
      DEBUG_MSG("Sampled t: " << t);
      zLeave = t*L2 + l2;
      break;
    }
  }

  DEBUG_MSG("Post sample");
  DEBUG_MSG("z: " << zLeave);


  l1 = zLeave / L2;

  DEBUG_MSG("l1: " << l1);

  // update count structures
  l1d(l1, d) += 1;
  // count_d(d) += 1;  // doesn't change following assignment anyway
  l1w(l1, word) += 1;
  count_l1(l1) += 1;

  DEBUG_MSG("l1w after sampling: " << trans(l1w.col(word)));


  CACHEDl1Probs(l1) = (l1d(l1, d) + alpha(l1, c)) / (count_l1(l1) + L1sumBeta(l1));

  DEBUG_MSG("Cached probs after sampling");
  DEBUG_MSG(trans(CACHEDl1Probs));
}




// estimation of the parameter alpha
void model::updateAlpha() {

  DEBUG_MSG("Updating alpha");

  uvec index;  // temporary index of document belonging in a given class
  imat sub_td;  // subset of tsd for a given class
  uword sub_D;   // number of document belonging to a given class

  for (uword c = 0; c < C; c++){

    DEBUG_MSG("Recomputing alpha for class: " << c);

    // identify documents belonging to a given class
    // index = find(classVector == c);
    index = linspace<uvec>(0, D-1);
    DEBUG_MSG("Index:");
    DEBUG_MSG(trans(index));

    sub_D = index.size();;
    sub_td = l1d.cols(index);

    DEBUG_MSG("Recomputing alpha for class: " << c);
    // preparing input for polya_fit_simple()
    // data is an int** replicating sub_td
    // alpha_temp replicates alpha for a given c
    int ** data;
    double * alpha_temp;
    data = new int*[L1];
    alpha_temp = new double[L1];
    for (uword t = 0; t < L1; t++) {

      alpha_temp[t] = alpha(t, c);

      data[t] = new int[sub_D];
      for (uword d = 0; d < sub_D; d++) {
        data[t][d] = sub_td(t, d);
      }
    }

    DEBUG_MSG("Current alpha:");
    DEBUG_MSG(trans(alpha.col(c)));
    // Rcout << alpha << "\n";

    // estimate a new alpha_temp based on matrix data
    polya_fit_simple(data, alpha_temp, L1, sub_D);
    DEBUG_MSG("New alpha:");
    DEBUG_MSG(arma::rowvec(alpha_temp, L1, false, false));

    // update alpha with the returned alpha_temp
    for (uword t = 0; t < L1; t++){
      alpha(t, c) = alpha_temp[t];
    }
    sumAlpha(c) = sum(alpha.col(c));
  }
}



// estimation of the parameter gamma
void model::updateGamma() {

  uvec index;    // temporary index of document belonging in a given class
  imat sub_tsd;  // subset of tsd for a given class
  uword sub_D;     // number of document belonging to a given class

  for (uword c = 0; c < C; c++){

    // identify documents belonging to a given class
    // index = find(classVector == c);
    index = linspace<uvec>(0, D-1);
    sub_D = index.size();
    sub_tsd = zd.cols(index);

    // preparing input for polya_fit_simple()
    // data is an int** replicating sub_tsd for a given t
    // gamma_temp replicates gamma for a given c and t

    // initializing data and gamma_temp sizes
    int ** data;
    double * gamma_temp;
    data = new int*[L2];
    gamma_temp = new double[L2];
    for (uword s = 0; s < L2; s++) {
      data[s] = new int[sub_D];
      gamma_temp[s] = 0;
      for (uword d = 0; d < sub_D; d++) {
        data[s][d] = 0;
      }
    }

    // looping on t as a vector gamma is estimated once per t
    for (uword t = 0; t<L1; t++){

      // populating gamma_temp and data for a given t
      for (uword s = 0; s < L2; s++) {
        gamma_temp[s] = gamma(t*L2 + s, c);
        for (uword d = 0; d < sub_D; d++) {
          data[s][d] = sub_tsd(t*L2 + s, d);
        }
      }

      // estimate a new gamma_temp based on matrix data
      polya_fit_simple(data, gamma_temp, L2, sub_D);

      // update gamma with the returned gamma_temp
      for (uword s = 0; s < L2; s++) {
        gamma(t*L2 + s, c) = gamma_temp[s];
      }
    }
  }
  for (uword t = 0; t < L1; t++){sumGamma.row(t) = sum(gamma.rows(0 + t*L2, L2-1 + t*L2), 0);}
}

// compute component p(w|t,s) of the likelihood
double model::computeLogLikelihoodW() {

  double epsilon = 0.000000001; // a very small epsilon is added to avoid lgamma(0)

  double logLik = 0; // initialize the likelihood

  // computation is done in four subcomponents. two variables for the numerator
  // and two for the denominator
  double numOne;
  double numTwo;
  double denomOne;
  double denomTwo;

  for (uword z = 0; z < L1*L2; z++) {

    numOne = lgamma(sumBeta(z));
    denomOne = sum(lgamma(beta.row(z) + epsilon));
    numTwo = 0;
    for (uword w = 0; w < V; w++) {
      numTwo += lgamma(zw(z, w) + beta(z, w) + epsilon);
    }
    denomTwo = lgamma(count_z(z) + sumBeta(z));

    // increments the log-likelihood with the result of the four subcomponents
    logLik += numOne + numTwo - denomOne - denomTwo;
  }


  return(logLik);
}



// compute component p(s|t) of the likelihood
double model::computeLogLikelihoodL2() {

  double epsilon = 0.000000001; // a very small epsilon is added to avoid lgamma(0)

  double logLik = 0; // initialize the likelihood

  // computation is done in four subcomponents. two variables for the numerator
  // and two for the denominator
  double numOne;
  double numTwo;
  double denomOne;
  double denomTwo;

  uword c;

  for (uword d = 0; d < D; d++) {

    // c = classVector(d);
    c = 0;

    for (uword t = 0; t < L1; t++) {

      numOne = lgamma(sumGamma(t, c));
      denomOne = sum(lgamma(vectorise(gamma.rows(0 + t*L2, L2-1 + t*L2)) + epsilon));

      numTwo = 0;
      for (uword s = 0; s < L2; s++) {
        numTwo += lgamma(zd(t*L2+s, d) + gamma(t*L2+s, c) + epsilon);
      }
      denomTwo = lgamma(l1d(t, d) + sumGamma(t, c));

      // increments the log-likelihood with the result of the four subcomponents
      logLik += numOne + numTwo - denomOne - denomTwo;
    }
  }
  return(logLik);
}

// compute component p(t) of the likelihood
double model::computeLogLikelihoodL1() {

  double epsilon = 0.000000001; // a very small epsilon is added to avoid lgamma(0)

  double logLik = 0; // initialize the likelihood

  // computation is done in four subcomponents. two variables for the numerator
  // and two for the denominator
  double numOne;
  double numTwo;
  double denomOne;
  double denomTwo;

  uword c;

  for (uword d = 0; d < D; d++) {

    // c = classVector(d);
    c = 0;

    numOne = lgamma(sumAlpha(c));
    denomOne =  sum(lgamma(alpha.col(c) + epsilon));

    numTwo = 0;
    for (uword t = 0; t < L1; t++) {
      numTwo += lgamma(l1d(t, d) + alpha(t, c)+ epsilon);
    }
    denomTwo = lgamma(count_d(d) + sumAlpha(c));

    // increments the log-likelihood with the result of the four subcomponents
    logLik += numOne + numTwo - denomOne - denomTwo;
  }
  return(logLik);
}

SEXP model::get_za() {
  SEXP tmp = PROTECT(Rf_allocVector(VECSXP, za.size()));
  for (uword d = 0; d < D; d++) {
    SET_VECTOR_ELT(tmp, d,  wrap( arma::conv_to<arma::ivec>::from(*(za[d]))));
  }
  UNPROTECT(1);
  return tmp;
}


SEXP model::get_intTokens() {
  SEXP tmp = PROTECT(Rf_allocVector(VECSXP, intTokens.size()));
  for (uword d = 0; d < D; d++) {
    SET_VECTOR_ELT(tmp, d,  wrap( arma::conv_to<arma::ivec>::from(*(intTokens[d]))));
  }
  UNPROTECT(1);
  return tmp;
}

