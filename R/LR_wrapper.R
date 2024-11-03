
#' Title
#'
#' @param X is a n x p matrix, where p is the number of covariates
#' @param y is a vector of length n
#' @param numIter number of iterations to run
#' @param eta 
#' @param lambda 
#' @param beta_init starting value for the beta matrix. Must be pxk where k is the number of groups.
#'
#' @return out
#' @export
#'
#' @examples
#' # Give example
LRMultiClass <- function(X, y, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  
  # Compatibility checks from HW3 and initialization of beta_init
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if (unique(X[, 1]) != 1) {
    stop(paste("First column of X is not 1s"))
  }
  if (unique(Xt[, 1]) != 1) {
    stop(paste("First column of Xt is not 1s"))
  }
  # Check for compatibility of dimensions between X and Y
  n <- length(y)
  if (nrow(X) != n) {
    stop(
      paste(
        "Number of observations in y aren't compatible with number of observations in X"
      )
    )
  }
  # Check for compatibility of dimensions between Xt and Yt
  nt <- length(yt)
  if (nrow(Xt) != nt) {
    stop(
      paste(
        "Number of observations in yt aren't compatible with number of observations in Xt"
      )
    )
  }
  # Check for compatibility of dimensions between X and Xt
  p <- ncol(X)
  if (p != ncol(Xt)) {
    stop(paste("Number of columns in X don't match Xt"))
  }
  # Check eta is positive
  if (eta <= 0) {
    stop(paste("Eta isn't positive"))
  }
  # Check lambda is non-negative
  if (lambda < 0) {
    stop(paste("Lambda is negative"))
  }
  
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  K <- length(unique(y))
  if (is.null(beta_init)) {
    beta_init <- matrix(0, nrow = p, ncol = K)
  } else{
    if (p != nrow(beta_init)) {
      stop(paste(
        "wrong number of rows in beta_init. Should be ",
        p,
        " has ",
        nrow(beta_init)
      ))
    }
    if (K != ncol(beta_init)) {
      stop(paste(
        "wrong number of columns in beta_init. Should be ",
        K,
        " has ",
        ncol(beta_init)
      ))
    }
  }
  
  # Call C++ LRMultiClass_c function to implement the algorithm
  out = LRMultiClass_c(X, y, numIter, eta, lambda, beta_init)
  
  # Return the class assignments
  return(out)
}