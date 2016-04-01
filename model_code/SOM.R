# The Speaker-Oriented Model (SOM) for various prior distributions

# The semantics of the positive form of a gradable adjective u 
# is defined as: x is u iff x's degree d >= \theta (threshold).
# The choice ">=" is arguably more intuitive, but technically it means 
# we should interpret \Phi(\theta) as \phi(h<\theta) which is not standard.
# Fortunately, since we mostly use continuous probability density functions,
# it does not make any difference. 

# The crucial formulas are

# \Pr(\theta) \propto exp(\lambda * U(\theta)),
# U(\theta) =  \Phi(\theta)/(1-\Phi(\theta)) * \int^\infty_\theta\phi^2(h)\ud h
#            - c * (1 - \Phi(\theta)),
# where \lambda is the degree of rationality,
# c is the cost of the positive form,
# \phi is the density function of the prior (probability in discrete cases),
# and \Phi is the cummulative distribution function (CDF).
#
# \Pr(\theta) is the prior of the threshold, derived by soft-maximizing  
# the utility defined above, which is the expected communicative success rate
# minus the cost
#
# We tend to use distributions with nice closure properties to analytically 
# derive \phi^2(h).
#
# The production rule is 
# \sigma(u | h_0) = \int^{h_0}_{-\infty} \Pr(h) \ud h \enspace.


source("auxiliary.R")

# Discrete cases

ExpectedSuccessDiscrete <- function(prior, add1=FALSE){
  # Computes expected success -- ES(\theta) for discrete priors. 
  # (TODO: rewrite SOM interface to clean up the code)
  # Args:
  #   prior: A probability distribution vector as the prior of degrees. 
  #   add1: whether to add another, unobserved degree that is even greater 
  #         than the current largest. This is to make sure that the current
  #         largest degree don't get prob 1.
  #
  # Returns: 
  #   A vector ES(\theta), which is of the same length (or add1) as prior and 
  #   is the expected success for each threshold \theta
  #   
  # Error handling
#   if (sum(prior) != 1) {
#     warning("Prior must sum up to 1!")
#   }
  
  n <- length(prior)
  
  # Note that we need "<" version CDFs
  lcdf <- c(0,cumsum(prior))[1:n]
  # The integral of squared prior
  square.prior <- prior^2
  square.integral <- sum(square.prior) - c(0,cumsum(square.prior))[1:n]
  
  expected.success <- lcdf / (1 - lcdf) * square.integral
  if (add1) {
    expected.success[n+1] <- 0
  }
  return(expected.success)
}
  
SOMThetaDiscrete <- function(prior, lambda, cost, add1=FALSE, 
                             nmlz=FALSE, rescale=FALSE){
  # Computes \Pr(\theta) for discrete priors. 
  # 
  # Args:
  #   prior: A probability distribution vector as the prior of degrees. 
  #   lambda: A real number, as the degree of rationality.
  #   cost: A real number, as the cost of the positive form.
  #   add1: whether to add another, unobserved degree that is even greater 
  #         than the current largest. This is to make sure that the current
  #         largest degree don't get prob 1.
  #   nmlz: whether the utilities should be normalized by multiplying the 
  #         number of points in the prior 
  #         (TODO: not really a neat solution, by default it is turned off.)
  # 
  #   rescale: the utilities will be rescaled by multiplying a constant 
  #         so that the maximal utility is always 1.
  #         This is another strategy to get a more homogeneous lambda. 
  #
  # Returns: 
  #   A vector \Pr(\theta), which is of the same length (or add1) as prior and 
  #   is a probability distribution over thresholds
  #   

  # Error handling
#   if (sum(prior) != 1) {
#     warning("Prior must sum up to 1!")
#   }

  n <- length(prior)

  # Note that we need "<" version CDFs
  lcdf <- c(0,cumsum(prior))[1:n]
  # The integral of squared prior
  square.prior <- prior^2
  square.integral <- sum(square.prior) - c(0,cumsum(square.prior))[1:n]

  # TODO(Ciyang):
  # The utility is actually multiplied by n because we need to regularize 
  # a bit in order for the softmax to work robustly. 
  # The point is that if the vector is too long, each entry has very low prob
  # and utilities become not so different from each other and the soft-max 
  # will need a very large lambda to work. 
  # Admittedly this is not elegant, but afterall what we need is some way to 
  # softmax and the standard one need not be the only reasonable choice. 
  # In any case it would be nice to see whether there is a better way out.
  
  utils <- lcdf / (1 - lcdf) * square.integral - cost * (1 - lcdf)
  if (nmlz){
    if (add1) {
      utils[n+1] <- 0
      utils <- (n+1) * utils
    }
    else {
      utils <- n * utils
    }
  }
  else {
    if (add1) {
      utils[n+1] <- 0
    }
    else {
    }
  }
  
  if (rescale){
    utils <- utils / max(utils)
  }
  return(Softmax(utils, lambda))
}

SOMSpeakerDiscrete <- function(prior, lambda, cost, add1=FALSE, rescale=FALSE){
  # Args:
  #   prior: A probability distribution vector as the prior of degrees.
  #   lambda: A real number, as the degree of rationality.
  #   cost: A real number, as the cost of the positive form.
  #   add1: whether to add another, unobserved degree that is even greater 
  #         than the current largest. This is to make sure that the current
  #         largest degree don't get prob 1.
  #
  # Returns: 
  #   A vector \sigma(u|d), which is of the same length as prior and 
  #   is the probability of using the positive form for each corresponding 
  #   degree in the prior

  n <- length(prior)

  if (add1) {
    return(cumsum(SOMThetaDiscrete(prior, lambda, cost, add1, rescale))[1:n])
  }
  else {
    return(cumsum(SOMThetaDiscrete(prior, lambda, cost, add1, rescale)))
  }
}

SOMSpeakerPredictive <- function(lambda, cost, N.vec, add1=TRUE){
  # generate 

  prob.vec <- SOMSpeakerDiscrete(Normalize(N.vec), lambda, cost, add1)
  return(mapply(rbinom, N.vec, prob.vec, MoreArgs=list(n=1)))
}

SOMListenerDiscrete <- function(prior, lambda, cost, 
                                add1=FALSE, rescale=FALSE){
  # Computes \sigma(d|u) for each degree (u=pos)
  # Args:
  #   prior: A probability distribution vector as the prior of degrees.
  #   lambda: A real number, as the degree of rationality.
  #   cost: A real number, as the cost of the positive form.
  #   add1: whether to add another, unobserved degree that is even greater 
  #         than the current largest. This is to make sure that the current
  #         largest degree don't get prob 1.
  #
  # Returns: 
  #   A vector \sigma(u|d), which is of the same length as prior and 
  #   is the probability of using the positive form for each corresponding 
  #   degree in the prior
  
  n <- length(prior)
  
  return(Normalize(prior * SOMSpeakerDiscrete(prior, lambda, cost, 
                                              add1, rescale)))
}
