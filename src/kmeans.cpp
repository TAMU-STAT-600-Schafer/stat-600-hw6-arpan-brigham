// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::uvec MyKmeans_c(const arma::mat& X, int K,
                            const arma::mat& M, int numIter = 100){
    // All input is assumed to be correct
    int n = X.n_rows;
    //int p = X.n_cols;
    // Initialize some parameters
    
    arma::uvec Y(n); // to store cluster assignments
    
    // Initialize any additional parameters if needed
    arma::mat group_mat = arma::zeros<arma::mat>(n,K);
    
    // For loop with kmeans algorithm
    for (int iter = 0 ; iter < numIter; iter++){
      arma::mat dist = 2 * (M * X.t()) -arma::repmat(arma::sum(arma::square(M) , 1), 1, n);
      Y = arma::index_min(dist, 0);
      arma::uvec unique_clusters = arma::unique(Y);  // Extract unique cluster assignments
      if (unique_clusters.n_elem < static_cast<size_t>(K)) {
        Rcpp::stop("Choose K correctly");
      }
    group_mat.zeros();
    for(size_t i=0; i < n; i++){
      group_mat(i,Y(i) )= 1;
    }
    
    arma::rowvec group_sum = arma::sum(group_mat, 0);
    arma::mat M_new = (group_mat.t() * X);
    M_new.each_row() /= group_sum;
    if (arma::approx_equal(M, M_new, "absdiff", 1e-6)) {
      break;
    } else {
      arma::mat M = M_new;
    }
    }
    // Returns the vector of cluster assignments
    return(Y);
}

