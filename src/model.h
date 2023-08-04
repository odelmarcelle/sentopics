#ifndef	MODEL_H
#define	MODEL_H

#include <memory>  // Fixes failed installation on linux gcc
#include <RcppArmadillo.h>
#include <progress.hpp>       // import progress bar functionality
#include <progress_bar.hpp>   // import progress bar functionality
#include "polya_fit_simple.h"  // estimates Dirichlet parameter based on data

using namespace Rcpp;
using namespace arma;

//ivec would be faster when called from R, as this avoid copying
arma::imat cpp_rebuild_zw(std::vector<std::unique_ptr<arma::uvec>>& intTokens, std::vector<std::unique_ptr<arma::uvec>>& za, arma::uword Z, arma::uword V);
arma::imat cpp_rebuild_zd(std::vector<std::unique_ptr<arma::uvec>>& za, arma::uword Z);

arma::imat wrapper_cpp_rebuild_zw(SEXP intTokens_, SEXP za_, arma::uword Z, arma::uword V);
arma::imat wrapper_cpp_rebuild_zd(SEXP za_, arma::uword Z);

class model {
public:
  bool reversed; // true means topic > sentiment (rJST), false means sentiment > topic (JST)

  uword V;  // vocabulary size
  uword L1;  // number of topics
  uword L2;  // number of sentiments
  uword D;  // number of documents in the corpus
  uword C;  // number of document classes

  uword it;          // internal iteration counter

  double initBeta;   // value of beta provided at initialization

  mat alpha;      // matrix T x C storing the parameter alpha for each class
  vec sumAlpha;   // vector of size C containing the sum of alphas for each class

  mat gamma;     // matrix (T x S) x C storing the parameter gamma for each class & topic
  mat sumGamma;   // matrix T x C containing the sum of gammas for each class & topic

  mat beta;      // matrix S x (T x S) storing the parameter beta for each topic & sentiment
  vec sumBeta;    // matrix T x S containing the sum of betas for each topic & sentiment

  mat L1beta;      // cube V x T x S storing the parameter beta for each topic & sentiment
  vec L1sumBeta;    // matrix T x S containing the sum of betas for each topic & sentiment

  uword alphaCycle;  // indicates at which frequency is estimated the Dirichlet parameter alpha
  uword gammaCycle;  // indicates at which frequency is estimated the Dirichlet parameter gamma

  arma::imat zw;
  arma::imat l1w; //summed up to dimension l1
  arma::imat zd;
  arma::imat l1d; //summed up to dimension l1
  std::vector<std::unique_ptr<arma::uvec>> za; // List of topic assignments
  arma::vec zProbs;
  arma::vec CACHEDzProbs;
  arma::ivec count_z;
  arma::ivec count_l1;

  arma::ivec count_d; // vector of size D containing the number of words in each document

  std::vector<std::unique_ptr<arma::uvec>> intTokens;  // List containing the corpus to analyze, where words are represented by their index in the vocabulary

  arma::ivec lexicon;  // vector of size V importing lexicon prior information. Expected values from 1 to S. Values of -2147483648 indicate no information

  arma::vec logLikelihoodW;  // vector storing the likelihood p(w|l1,l2) at each iteration
  arma::vec logLikelihoodL1;  // vector storing the likelihood p(l1) at each iteration
  arma::vec logLikelihoodL2;  // vector storing the likelihood p(l2|l1) at each iteration

  arma::vec l1Probs;       // for trueLDA
  arma::vec CACHEDl1Probs; // for trueLDA


  // class constructor
  model() {
    set_default_values(true);
  }
  model(bool reversed_) {
    set_default_values(reversed_);
  }

  // initial empty object
  void set_default_values(bool reversed_);


  // initialize the model
  void init(SEXP intTokens_,
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
            );

  void initBetaLex(double initBeta);        // initialize beta with lexicon information
  void initAssignments();   // randomly assign topic and sentiment to all words


  // model inference
  void iterate(uword iterations, bool displayProgress, bool computeLikelihood);  // perform n complete iterations of Gibbs sampling
  void iterateLDA(uword start, uword iterations, bool computeLikelihood, Progress& p);
  void iteratel2(uword start, uword iterations, bool computeLikelihood, Progress& p);

  void sampling(const uword& word, uword& zLeave, uword c, uword d); // sampling of a single word's topic
  void samplingLDA(const uword& word, uword& zLeave, uword c, uword d); // sampling of a single word's topic corrected

  void updateAlpha(); // update the value of the Dirichlet parameter alpha based on count matrices
  void updateGamma(); // update the value of the Dirichlet parameter gamma based on count matrices

  double computeLogLikelihoodW(); // compute the likelihood of p(w|l1,l2)
  double computeLogLikelihoodL1(); // compute the likelihood of p(l1)
  double computeLogLikelihoodL2(); // compute the likelihood of p(l2|l1)

  // function to rebuild the C++ model from an R object
  void rebuild(uword V_,
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
               );

  SEXP get_za();
  SEXP get_intTokens();

};

// Rcpp modules exposition
RCPP_MODULE(model_module) {
  class_<model>( "cpp_sentopicmodel" )
  .constructor()
  .constructor<bool>()
  .field( "reversed", &model::reversed)
  .field( "V", &model::V)
  .field( "L1", &model::L1)
  .field( "L2", &model::L2)
  .field( "D", &model::D)
  .field( "C", &model::C)
  .field( "it", &model::it)
  .field( "zd", &model::zd)
  .field( "l1d", &model::l1d)
  .field( "count_z", &model::count_z)
  .field( "zw", &model::zw)
  .field( "l1w", &model::l1w)
  .property("za", &model::get_za)
  .property("intTokens", &model::get_intTokens)
  .field( "lexicon", &model::lexicon)
  .field( "initBeta", &model::initBeta)
  .field( "L1prior", &model::alpha)
  .field( "L2prior", &model::gamma)
  .field( "beta", &model::beta)
  .field( "alphaCycle", &model::alphaCycle)
  .field( "gammaCycle", &model::gammaCycle)
  .field( "logLikelihoodW", &model::logLikelihoodW)
  .field( "logLikelihoodL1", &model::logLikelihoodL1)
  .field( "logLikelihoodL2", &model::logLikelihoodL2)
  .method( "init", &model::init)
  .method( "initBetaLex", &model::initBetaLex)
  .method( "initAssignments", &model::initAssignments)
  .method( "rebuild", &model::rebuild)
  .method( "iterate", &model::iterate)
  ;
}

#endif
