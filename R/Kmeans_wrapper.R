#' Title
#'
#' @param X 
#' @param K 
#' @param M 
#' @param numIter 
#'
#' @return Explain return
#' @export
#'
#' @examples
#' # Give example
MyKmeans <- function(X, K, M = NULL, numIter = 100){
  
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