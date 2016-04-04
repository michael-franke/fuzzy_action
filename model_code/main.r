#############################################
## predictions about fuzzy language use
##
#### 1. take a prior
#### 2. retrieve "meaning" by SOM or RSA
#### 3. create a game from that by assuming
####    some semantics for the composite
####    expressions
#### 4. run the game with RSA, RD or IQR
#############################################

setwd("~/Desktop/borderline-contradictions/model_code")
library(dplyr)
source('solution_concepts.r')
source('SOM.R')
source('RSA.R')

#############################################
## helper functions
#############################################

discretize = function(n, densityF){
  # densityF is a function that returns cumulative densities
  # output are probability masses for numerals >= 0
  n = round(n) # input must be a vector of integers
  prob = sapply(n, function(x) ifelse(x >= 0 , densityF(x + 0.5) - densityF(x-0.5), 0)) /  (1 - densityF(-0.5))
  return(prob/sum(prob))
}

rescale = function(x) {
  (x - min(x)) / (max(x) - min(x))
}

get_prior = function(ns, style = "flat") {
  known_styles = c("flat", "normal", "horns")
  if (style == "flat") {
    prior = rep(1/ns, ns)  
  }
  if (style == "normal") {
    prior = discretize(n = states, densityF = function(x) pnorm(x, mean = max_states/2, sd = max_states/10))  
  }
  if (style == "horns") {
    prior = discretize(n = states, densityF = function(x) pbeta(x/max_states, 0.5,0.5))  
  }
  if (! style %in% known_styles) {
    warning(paste0("Unknown priors style. Use one from: ", paste0(known_styles, collapse = ", ")))
  }
  return(prior)
}

get_base_semantics = function(prior, lambda = 5, cost = 0, style = "OTM") {
  known_styles = c("OTM", "RSA")
  if (style == "OTM") {
    P = SOMSpeakerDiscrete(prior = prior, lambda = lambda, cost = cost, rescale = TRUE)
    Q = rev(SOMSpeakerDiscrete(prior = rev(prior), lambda = lambda, cost = cost, rescale = TRUE))
  }
  if (style == "RSA") {
    P = RSASpeakerPragmaticDiscete(prior = prior, lambda = lambda, cost = cost)
    Q = rev(RSASpeakerPragmaticDiscete(prior = rev(prior), lambda = lambda, cost = cost))
  }
  if (! style %in% known_styles) {
    warning(paste0("Unknown style for base semantics. Use one from: ", paste0(known_styles, collapse = ", ")))
  }
  base_semantics = list(P = P, Q = Q)
  return(base_semantics)
}

get_compositional_semantics = function(base_semantics, style = "fuzzy_classic") {
  known_styles = c("fuzzy_classic", "fuzzy_weird", "Dan's mythical magic mix")
  P = base_semantics$P
  Q = base_semantics$Q
  ns = length(P)
  semantics = matrix(0, ncol = ns, nrow = 9)
  rownames(semantics) = c("Q", 
                          "not-P", 
                          "neither P nor Q", 
                          "not-Q", 
                          "P",
                          "neither Q not not-Q", 
                          "neither P nor not-P",
                          "Q and not-Q", 
                          "P and not-P")
  if (style == "fuzzy_classic") {
    semantics[1,] = Q # Q
    semantics[2,] = 1-P 
    semantics[3,] = sapply(1:ns, function(x) min(1-P[x], 1-Q[x]) )
    semantics[4,] = 1-Q
    semantics[5,] = P
    semantics[6,] = sapply(1:ns, function(x) min(1-Q[x], Q[x]) ) # neither Q nor not Q
    semantics[7,] = sapply(1:ns, function(x) min(1-P[x], P[x]) ) # neither P nor not P
    semantics[8,] = sapply(1:ns, function(x) min(1-Q[x], Q[x]) ) # Q and not Q
    semantics[9,] = sapply(1:ns, function(x) min(1-P[x], P[x]) ) # P and not P
  }
  if (style == "fuzzy_weird") {
    semantics[1,] = Q 
      # Q
    semantics[2,] = 1-P 
      # not P
    semantics[3,] = sapply(1:ns, function(x) min(1-P[x], 1-Q[x]) ) 
      # not P and not Q
    semantics[4,] = 1-Q
      # not Q
    semantics[5,] = P
      # P
    semantics[6,] = sapply(1:ns, function(x) min(1-Q[x], Q[x])/.5 ) 
      # neither Q nor not Q
    semantics[7,] = sapply(1:ns, function(x) min(1-P[x], P[x])/.5 ) 
      # neither P nor not P
    semantics[8,] = sapply(1:ns, function(x) min(1-Q[x], Q[x])/.5 ) 
      # Q and not Q
    semantics[9,] = sapply(1:ns, function(x) min(1-P[x], P[x])/.5 ) 
      # P and not P
  }
  if (style == "Dan's mythical magic mix") {
    semantics[1,] = Q 
    # Q
    semantics[2,] = 1-P 
    # not P
    semantics[3,] = sapply(1:ns, function(x) (1-P[x]) * (1-Q[x])) 
    # not P and not Q
    semantics[4,] = 1-Q
    # not Q
    semantics[5,] = P
    # P
    semantics[6,] = sapply(1:ns, function(x) Q[x] * (1-Q[x])) 
    # neither Q nor not Q
    semantics[7,] = sapply(1:ns, function(x) P[x] * (1-P[x]))  
    # neither P nor not P
    semantics[8,] = sapply(1:ns, function(x) Q[x] * (1-Q[x])) 
    # Q and not Q
    semantics[9,] = sapply(1:ns, function(x) P[x] * (1-P[x])) 
    # P and not P
  }
  if (! style %in% known_styles) {
    warning(paste0("Unknown style for compositional semantics. Use one from: ", paste0(known_styles, collapse = ", ")))
  }
  return(semantics)
}

get_utils = function(ns, style = "Lewis", alpha_Nosofsky = 1, alpha_quadratic = 1) {
  known_styles = c("Lewis", "Nosofsky", "quadratic_loss_rescaled")
  utils = matrix(0, nrow = ns, ncol = ns)
  for (i in 1:ns) {
    for (j in 1:ns) {
      if (style == "Lewis") {
        utils[i,j] = ifelse(i == j, 1, 0)  
      }
      if (style == "Nosofsky") {
        utils[i,j] = exp(-alpha_Nosofsky*(states[i] - states[j])^2)
      }
      if (style == "quadratic_loss_rescaled") {
        utils[i,j] = (states[i] - states[j])^(2*alpha_quadratic)
      }
      
    }
  }
  if (style == "quadratic_loss_rescaled") {
    utils = 1 - rescale(utils)
  }
  if (! style %in% known_styles) {
    warning(paste0("Unknown style for compositional semantics. Use one from: ", paste0(known_styles, collapse = ", ")))
  }
  return(utils)
}

plot_sender = function(mat) {
  plot_data = as.data.frame(mat) %>% melt() %>% mutate(x = rep(0:(nrow(mat)-1), times = ncol(mat))) %>%
    rename(message = variable)
  ggplot(plot_data, aes(x = x, y = value, color = message)) + geom_point() + geom_line()
}

plot_receiver = function(mat) {
  plot_data = as.data.frame(t(mat)) %>% melt() %>% mutate(x = rep(0:(ncol(mat)-1), times = nrow(mat))) %>%
    rename(message = variable)
  ggplot(plot_data, aes(x = x, y = value, color = message)) + geom_point() + geom_line()
}

get_rank_order = function(solution) {
  rec = solution$rec
  # for each message: get the "prototype" interpretation,
  max_state = sapply(1: nrow(rec), function(m) which.max(rec[m,]))
  # then rank messages according to that
  rank_order = rank(max_state)
  names(rank_order) = rownames(rec)
  return(rank_order)
}

#############################################
## create the game
#############################################

max_states = 11
ns = max_states + 1
states = seq(0,max_states, length.out = max_states+1)

prior = get_prior(ns, style = "horns")

base_semantics = get_base_semantics(prior, lambda = 1, cost = 0.5, style = "OTM")

semantics = get_compositional_semantics(base_semantics, style = "fuzzy_classic")
# restrict the semantics to exclude borderline contradictions
semantics = semantics[1:7,]

# utils = get_utils(ns, "Lewis")
utils = get_utils(ns, "Nosofsky")
# utils = get_utils(ns, "quadratic_loss_rescaled")


fuzzy_action_game = create_game(
  states = paste("t_", 0:(ns-1), collapse = NULL, sep = ""),
  messages = rownames(semantics),
  utils = utils,
  semantics  = semantics
  # message_preferences = c(1, .3, 0.1, .3, 1, 0.03, 0.03)
)


#############################################
## solve the game
#############################################

# solution = apply_RD(fuzzy_action_game, iterations = 105)
solution = apply_RSA(fuzzy_action_game, depth = 10, lambda = 5)
# solution = apply_IBR(fuzzy_action_game, depth = 10)
# solution = apply_IQR(fuzzy_action_game, depth = 10, lambda = 15)

#############################################
## visualization
#############################################

show(plot_sender(solution$sen))
show(plot_receiver(solution$rec))
show(sort(get_rank_order(solution)))

