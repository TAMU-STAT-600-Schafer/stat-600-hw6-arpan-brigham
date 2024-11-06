#' Kmeans C++ implementation
#' 
#' @param X - n by p matrix containing n data points to cluster
#' @param K - integer specifying number of clusters
#' @param M - (optional) K by p matrix of cluster centers
#' @param numIter - number of maximal iterations for the algorithm, the default value is 100 
#'
#' @return Y - a vector of length n that contains class assignments (0:(K-1)).
#' @export
#'
#' @examples
#' # Give example
#' set.seed(234) # Set Seed
#' n <- 100
#' # Set cluster means:
#' mean1 <- 1
#' mean2 <- 5
#' mean3 <- 10
#' # Simulate data:
#' X <- matrix(c(rnorm(n, mean1), rnorm(n, mean2), rnorm(n, mean3)), ncol = 1)
#' K <- 3
#' # Run Function:
#' MyKmeans(X, K)
MyKmeans <- function(X, K, M = NULL, numIter = 100){
  
  # Check to make sure X & K are appropriately provided.
  if (!is.matrix(X) & !is.data.frame(X)) {
    stop(paste("X must be a n by p matrix"))
  }
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  n = nrow(X) # number of rows in X
  
  # Check whether M is NULL or not. If NULL, initialize based on K random points from X. If not NULL, check for compatibility with X dimensions.
  if(is.null(M)==TRUE){
    Index<-sample(1:n,K,replace = FALSE)
    M<-X[Index,]
  }else
  {
    if(dim(M)[2]!=dim(X)[2] || dim(M)[1]!=K)
    {
      stop("M is not suitable")
    }
  }
  #print(M)
  M<-as.matrix(M)
  
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = MyKmeans_c(X, K, M, numIter)
  
  # Return the class assignments
  return(Y)
}