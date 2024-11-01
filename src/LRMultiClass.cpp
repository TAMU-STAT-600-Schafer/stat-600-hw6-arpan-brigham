// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// write objective function:
// [[Rcpp::export]]
double obj(const arma::uvec& y, const arma::mat& beta, const double& lambda,
           const arma::mat& pk, const int& n){
  double neg_sum_y = 0;
  for(int i = 0; i < n; i++){
    neg_sum_y = neg_sum_y - log(pk(n, y[i]));
  }
  double objective = neg_sum_y + (lambda) * arma::accu(beta % beta)/2;
  
  return(objective);
}

// write function to find pk given X and beta:
// [[Rcpp::export]]
arma::mat calc_pk_c(const arma::mat& X, const arma::mat& beta){
  // get exp(Xbeta)
  arma::mat exp_Xb = exp(X * beta);
  // get row sums of exp(Xbeta):
  int nrow = exp_Xb.n_rows;
  int ncol = exp_Xb.n_cols;
  arma::vec rowsums(nrow);
  for(int n = 0; n < nrow; n++){
    rowsums[n] = arma::sum(exp_Xb.row(n));
  }
  
  // get output pk matrix:
  arma::mat pk(nrow, ncol);
  for(int i = 0; i < nrow; i++){
    for(int j = 0; j < ncol; j++){
      pk(i, j) = exp_Xb(i, j)/rowsums[i];
    }
  }
  
  return(pk);
}

// For simplicity, no test data, only training data, and no error calculation.
// X - n x p data matrix
// y - n length vector of classes, from 0 to K-1
// numIter - number of iterations, default 50
// eta - damping parameter, default 0.1
// lambda - ridge parameter, default 1
// beta_init - p x K matrix of starting beta values (always supplied in right format)
// [[Rcpp::export]]
Rcpp::List LRMultiClass_c(const arma::mat& X, const arma::uvec& y, const arma::mat& beta_init,
                               int numIter = 50, double eta = 0.1, double lambda = 1){
    // All input is assumed to be correct
    
    // Initialize some parameters
    int K = max(y) + 1; // number of classes
    int p = X.n_cols;
    int n = X.n_rows;
    arma::mat beta = beta_init; // to store betas and be able to change them if needed
    arma::vec objective(numIter + 1); // to store objective values
    
    // Initialize anything else that you may need
    arma::mat pk = calc_pk_c(X, beta);
    arma::vec w(n);
    arma::mat hessian(n,p);
    arma::colvec gradient(p);
    arma::mat lambda_I(p,p);
    lambda_I.eye();
    lambda_I = lambda_I *lambda;
    arma::colvec y_k(n);
    arma::colvec diff(n);
    objective[0] = obj(y, beta, lambda, pk, n);
    
    // Newton's method cycle - implement the update EXACTLY numIter iterations
    for (int i = 1; i < numIter + 1; i++) {
      // Update Beta:
      for (int k = 0; k < K; k++) {
        y_k = arma::zeros<arma::colvec>(p);
        arma::uvec ind = arma::find(y == k);
        y_k(ind) = arma::ones<arma::colvec>(ind.size());
        diff = pk.col(k) - y_k;
        gradient = X.t() * diff + lambda * beta.col(k);
        w = pk.col(k) % (1 - pk.col(k));
        hessian = X.t() * (w % X) + lambda_I;
        beta.col(k) = beta.col(k) - eta * solve(hessian, gradient);
      }
      // Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
      pk = calc_pk_c(X, beta);
      objective[i] = obj(y, beta, lambda, pk, n);
    }
    
    // Create named list with betas and objective values
    return Rcpp::List::create(Rcpp::Named("beta") = beta,
                              Rcpp::Named("objective") = objective);
}
