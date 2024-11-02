#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::uvec MyKmeans_c(const arma::mat& X, int K,
                      const arma::mat& M, int numIter = 100){
  int n = X.n_rows;
  //int p = X.n_cols;
  
  arma::mat M_previous = M;  // Initialize M_previous with provided centroids
  arma::uvec Y(n);  // Cluster assignment vector for each data point
  arma::mat M_next = M;  // Initialize M_next with a copy of M
  
  for (int iter = 0; iter < numIter; iter++) {
    // Calculate row sums of X^2 and M^2
    arma::colvec X_squared = arma::sum(arma::square(X), 1);
    arma::colvec M_squared = arma::sum(arma::square(M_previous), 1);
    
    // Calculate the cross-term and the distance matrix
    arma::mat cross_term = 2 * X * M_previous.t();
    arma::mat X_distance = arma::repmat(X_squared, 1, K) + arma::repmat(M_squared.t(), n, 1) - cross_term;
    
    // Find minimum distance cluster for each point
    Y = arma::index_min(X_distance, 1);
    
    // Check if any cluster has no assigned points
    for (int i = 0; i < K; i++) {
      arma::uvec indices = arma::find(Y == i);  // Find points assigned to cluster i
      if (indices.is_empty()) {
        Rcpp::stop("Empty cluster encountered. Consider changing the initialization of M.");
      } else {
        // Update cluster centroid M_next[i,] with mean of points in the cluster
        M_next.row(i) = arma::mean(X.rows(indices), 0);
      }
    }
    
    // Check for convergence: if M has not changed, break
    if (arma::approx_equal(M_next, M_previous, "absdiff", 1e-6)) {
      break;
    } else {
      M_previous = M_next;  // Update M_previous to the current M_next
    }
  }
  
  // Return the vector of assignments
  return Y;
}
