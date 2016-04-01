
source("auxiliary.R")

# Discrete cases

RSASpeakerSaturatedDiscrete <- function(prior, lambda, cost){
  # Computes RSA's production probability p(u|d, \theta)
  # 
  # Args:
  #   prior: A probability distribution vector as the prior of degrees. 
  #   lambda: degree of rationality
  #   cost: cost of the positive form
  #
  # Returns: 
  #   A vector p(u|d, \theta), which is of the same length (or add1) as prior, 
  #   showing the production probability for each threshold \theta
  #   (note that p(u|d, \theta) is constant, as long as d>\theta).
  #   
  # Error handling
  if (sum(prior) != 1) {
    warning("Prior must sum up to 1!")
  }
  
  n <- length(prior)
  
  # Note that we need "<" version CDFs
  lcdf <- c(0,cumsum(prior))[1:n]
  
  # 
  sigma.theta <- 1/ (1+ (1 - lcdf)^lambda  * exp(lambda * cost))

  return(sigma.theta)
}

RSASpeakerAverageDiscrete <- function(prior, lambda, cost){
  # Computes RSA's averaged production probability p(u|d) = E[p(u|d, \theta)]
  # (assuming the prior for \theta is uniform)
  # 
  # Args:
  #   prior: A probability distribution vector as the prior of degrees. 
  #   lambda: degree of rationality
  #   cost: cost of the positive form
  #
  # Returns: 
  #   A vector p(u|d), which is of the same length (or add1) as prior and 
  #   is the averaged production probability predicted by RSA
  #   (Note that for those \theta>d, p(u|d,\theta)=0, and \theta is assumed
  #   to be uniform, so the mean is just the cummulative sum divided by length)
  #   
  # Error handling

  n <- length(prior)

  return(cumsum(RSASpeakerSaturatedDiscrete(prior, lambda, cost)) / n)
}

RSAListenerDiscrete <- function(prior, lambda, cost){
  # Computes RSA's interpretation probability p(d|u) \propto p(d)p(u|d)
  # Note that p(u|d) is the averaged speaker "in the listener's mind"
  # 
  # Args:
  #   prior: A probability distribution vector as the prior of degrees. 
  #   lambda: degree of rationality
  #   cost: cost of the positive form
  #
  # Returns: 
  #   A vector p(d|u), which is of the same length as prior and 
  #   is the posterior probability of degree predicted by RSA
  #   
  # Error handling
  
  n <- length(prior)
  
  return(Normalize(prior * RSASpeakerAverageDiscrete(prior, lambda, cost)))
}

RSAThetaDiscrete <- function(prior, lambda, cost){
  # Computes RSA's posterior p(\theta) \propto (1-lcdf(\theta))p(u|d, \theta)
  # 
  # Args:
  #   prior: A probability distribution vector as the prior of degrees. 
  #   lambda: degree of rationality
  #   cost: cost of the positive form
  #
  # Returns: 
  #   A vector p(\theta), which is of the same length as prior and 
  #   is the posterior probability of degree predicted by RSA
  #   
  # Error handling
  if (sum(prior) != 1) {
    warning("Prior must sum up to 1!")
  }
  
  n <- length(prior)
  
  # Note that we need "<" version CDFs
  lcdf <- c(0,cumsum(prior))[1:n]

  return(Normalize( (1 - lcdf) * 
                    RSASpeakerSaturatedDiscrete(prior, lambda, cost)))
}

RSASpeakerSemanticDiscete = function(prior, lambda, cost){
  out = cumsum(RSAThetaDiscrete(prior,lambda, cost))
  n = length(prior)
  out[n] = (1+out[n-1])/2
  return(out)
}

RSASpeakerPragmaticDiscete = function(prior, lambda, cost){
  n = length(prior)
  r2 = RSAListenerDiscrete(prior,lambda, cost)
  # out = sapply(1:n, function(i) r2[i] / (prior[i] + r2[i]) )
  out = r2 / (prior + r2)
  return(out)
}