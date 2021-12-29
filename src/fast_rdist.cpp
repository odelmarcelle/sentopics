#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// Inspired from 'rdist' package but modified for more efficiency in computing
// euclidean distance. Compare pairs of vectors in each matrix columns. Columns
// are more efficient since R (and Armadillo) are column-major ordered.
//[[Rcpp::export(rng = false)]]
NumericMatrix euclidean_cdist(NumericMatrix A, NumericMatrix B) {
  int m = A.ncol(), n = B.ncol(), k = A.nrow();

  arma::mat Ar = arma::mat(A.begin(), k, m, false);
  arma::mat Br = arma::mat(B.begin(), k, n, false);
  arma::mat C(m, n);

  for (int i = 0; i < m; ++i){
    arma::mat Arow = Ar.col(i);
    for (int j = 0; j < n; ++j){
      C(i, j) = sum(square(Arow - Br.col(j)));
    }
  }
  return wrap(sqrt(C));
}

//[[Rcpp::export(rng = false)]]
NumericMatrix cpp_cosineSimilarity(NumericMatrix A, NumericMatrix B) {
  int m = A.ncol(), n = B.ncol(), k = A.nrow();

  arma::mat Ar = arma::mat(A.begin(), k, m, false);
  arma::mat Br = arma::mat(B.begin(), k, n, false);

  arma::mat C = (Ar.t() * Br) / ( trans(sqrt(sum(square(Ar), 0))) * sqrt(sum(square(Br), 0)) );

  return wrap(C);
}

/*** R
nrow <- 10^4
ncol <- 25
X <- matrix(rnorm(nrow * ncol), nrow, ncol)
Y <- matrix(rnorm(nrow * ncol), nrow, ncol)
tX <- t(X)
tY <- t(Y)

## Previous cosine similarity
cosineSimilarity <- function(x, y = NULL) {
  if (is.null(y)) y <- x
  t(x) %*% as.matrix(y) / (sqrt(colSums(x^2) %*% t(colSums(y^2))))
}

microbenchmark::microbenchmark(
  fields = fields <- fields::rdist(tX, tY),
  pdist = pdist <- pdist::pdist(tX, tY),
  rdist = rdist <- rdist::cdist(tX, tY),
  custom = custom <- euclidean_cdist(X, Y),
  cos = cos1 <- cosineSimilarity(X, Y),
  cppCos = cos2 <- cpp_cosineSimilarity(X, Y)
)

all.equal(fields, custom)
all.equal(as.matrix(pdist), custom)
all.equal(rdist, custom)
all.equal(cos1, cos2)

*/
