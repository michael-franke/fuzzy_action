# Auxiliary functions


Normalize <- function(v){
  # Computes the probability normalization of vector v.
  #
  # Args:
  #   v: A vector of non-negative numbers, the sum of which must be positive.
  #   
  # Returns:
  #   A probability distribution vector normalizing v.

  # Error handling
  if (any(v < 0)) {
    stop("All elements must be non-negative!")
  }
  v.sum <- sum(v)
  if (v.sum <= 0) {
    stop("The sum must be positive!")
  }

  return(v / v.sum)
} 


Softmax <- function(utils, lambda){
  # Computes sub-optimal choice distribution among options according to 
  # the utilities.
  #
  # Args:
  #   utils: A vector of utilities
  #   lambda: A real number as the degree of rationality
  #   
  # Returns:
  #   A probability distribution vector v of the same length as utils, 
  #   with v \propto exp(lambda*utils).

  v <- exp(lambda * utils)
  # Normalization
  return(Normalize(v))
}

