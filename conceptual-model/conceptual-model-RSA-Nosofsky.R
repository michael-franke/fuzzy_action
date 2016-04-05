#
# Conceptual model for borderline contradictions. 
# Just semantics: We assume some distribution and then apply
# the definitions.
#

Normalize = function(v) {
  sm = sum(v)
  if (length(v) == 0) warning("Attempt to normalize empty vector")
  else if (sm <= 0) warning("Normalize(): Vector sum must be positive")
  else v/sm
}
vector.min = function(v1, v2) {
  ln = length(v1)
  if (ln != length(v2)) warning('unequal vector lenghts in vector.min()')
  else sapply(1:ln, function(i) min(v1[i], v2[i]))
}
decum = function(v) {
  out = rep(NA, length(v))
  out[1] = v[1]
  for (i in 2:length(v)) {
    out[i] = v[i] - v[i-1]
  }
  return(out)
}

maximum = 100
grid.size = 101
tall.mu = .55 * maximum
short.mu = .45 * maximum
both.sd = .1 * maximum

grid = seq(from=0, to=maximum, length.out=grid.size)

# normal prior:
tall.theta = Normalize(sapply(grid, FUN=function(x) dnorm(x, tall.mu, both.sd)))
short.theta = Normalize(sapply(grid, FUN=function(x) dnorm(x, short.mu, both.sd)))

# horns:
#tall.theta = Normalize(sapply(grid, FUN=function(x) x^8))
#short.theta = rev(tall.theta)


tall.cumulative = cumsum(tall.theta)
short.cumulative = rev(tall.cumulative)

not.tall.cumulative = 1 - tall.cumulative
not.short.cumulative = 1 - short.cumulative

neither.prob = not.tall.cumulative * not.short.cumulative
neither.fuzzy = vector.min(not.tall.cumulative, not.short.cumulative)

tall.fuzzy.classic.BC.cumulative = vector.min(tall.cumulative, not.tall.cumulative)
tall.fuzzy.rescaled.BC.cumulative = tall.fuzzy.classic.BC.cumulative / max(tall.fuzzy.classic.BC.cumulative)

short.fuzzy.classic.BC.cumulative = vector.min(short.cumulative, not.short.cumulative)
short.fuzzy.rescaled.BC.cumulative = short.fuzzy.classic.BC.cumulative / max(short.fuzzy.classic.BC.cumulative)

tall.mult.sampling.BC.cumulative = tall.cumulative * not.tall.cumulative
short.mult.sampling.BC.cumulative = short.cumulative * not.short.cumulative

# plot fuzzy classic vs fuzzy rescaled
plot(grid, tall.cumulative, col='blue', type='l', ylim=c(0,1))
lines(grid, short.cumulative, col='blue')
lines(grid, tall.fuzzy.classic.BC.cumulative, col='red')
lines(grid, short.fuzzy.classic.BC.cumulative, col='red')
lines(grid, tall.fuzzy.rescaled.BC.cumulative, col='green')
lines(grid, short.fuzzy.rescaled.BC.cumulative, col='green')
legend(x=.75, y=.7, c('Truthiness', 'Fuzzy-classic BC', 'Fuzzy-rescaled BC'), col=c('blue', 'red', 'green'), text.col=c('blue', 'red', 'green'), cex=.74, lty=1)

# plot fuzzy rescaled vs multiple sampling
plot(grid, tall.cumulative, col='blue', type='l', ylim=c(0,1), main='Modified fuzzy semantics vs. probabilistic model')
lines(grid, short.cumulative, col='blue')
lines(grid, tall.mult.sampling.BC.cumulative, col='red')
lines(grid, short.mult.sampling.BC.cumulative, col='red')
lines(grid, tall.fuzzy.rescaled.BC.cumulative, col='green')
lines(grid, short.fuzzy.rescaled.BC.cumulative, col='green')
legend(x=.75, y=.7, c('Truthiness', 'Mult-sampling BC', 'Fuzzy-rescaled BC'), col=c('blue', 'red', 'green'), text.col=c('blue', 'red', 'green'), cex=.74, lty=1)

# Next: plug into a pragmatic model ...

# choice point: does the improbability of the multiple-sampling being true affect the speaker probabilities? or do we just let costs do the work of sharpening this utterance?
degrees = grid
degree.mu = .5 * maximum
degree.sd = .15 * maximum
degree.prior = Normalize(sapply(degrees, function(x) dnorm(x, degree.mu, degree.sd)))
#degree.prior = Normalize(sapply(degrees, function(x) dbeta(x, .5, .5)))
lambda = 30
cost.param = .01

#utterances = c('tall', 'short', 'nada', 'not tall', 'not short', 'tall and not tall', 'short and not short', 'neither tall nor short')
utterances = c('tall', 'short', 'not tall', 'not short', 'tall and not tall', 'short and not short', 'neither tall nor short')
cost = function(u) cost.param * switch(u, 
                                       'tall' = 2,
                                       'short' = 2,
                                       'not tall' = 3,
                                       'not short' = 3,
                                       'tall and not tall' = 4,
                                       'short and not short' = 4,
                                       'neither tall nor short' = 4#,
                                      # 'nada' = 0
)
vacuous = rep(1, grid.size)
listener = function(semantic.theory) {
  sapply(utterances, function(u) {
    print(u)
    # truthiness is the likelihood function
    truthiness = sapply(degrees, FUN=function(d) {
      if (u == 'tall and not tall') {
        if (semantic.theory == 'fuzzy-classic') {
          sem = tall.fuzzy.classic.BC.cumulative
        } else if (semantic.theory == 'fuzzy-rescaled') {
          sem = tall.fuzzy.rescaled.BC.cumulative
        } else if (semantic.theory == 'mult-sampling') {
          sem = tall.mult.sampling.BC.cumulative
        } else warning('unknown semantic theory in function semantics()')
      } else if (u == 'short and not short') {
        if (semantic.theory == 'fuzzy-classic') {
          sem = short.fuzzy.classic.BC.cumulative
        } else if (semantic.theory == 'fuzzy-rescaled') {
          sem = short.fuzzy.rescaled.BC.cumulative
        } else if (semantic.theory == 'mult-sampling') {
          sem = short.mult.sampling.BC.cumulative
        } else warning('unknown semantic theory in function semantics()')
      } else if (u == 'neither tall nor short') {
        if (semantic.theory == 'mult-sampling') {
          sem = neither.prob
        } else {
          sem = neither.fuzzy
        }
      } else {
        # stuff shared among all theories
        sem = switch(u,
                   'tall' = tall.cumulative,
                   'short' = short.cumulative,
                   'not tall' = not.tall.cumulative,
                   'not short' = not.short.cumulative#,
                  # 'nada' = vacuous
        )
      }
      return(sem[which(degrees==d)])
    })
    return(Normalize(degree.prior * truthiness))
  })
}

listener.fuzzy.classic = listener('fuzzy-classic')
listener.fuzzy.rescaled = listener('fuzzy-rescaled')
listener.mult.sampling = listener('mult-sampling')

# P_S1(u | d) \propto exp(lambda * U(u))
#                   = exp(lambda * (P_L0(d | u) * cost(u))) 

speaker.prefs = function(d, semantic.theory) {
  listener.probs = switch(semantic.theory,
                  'fuzzy-classic' = listener.fuzzy.classic,
                'fuzzy-rescaled' = listener.fuzzy.rescaled,
                'mult-sampling' = listener.mult.sampling
                )
  listener.based.prefs = listener.probs[which(degrees == d),]
    # pick out the row where the relevant listener probs are
  utility.by.degree = sapply(degrees, function(x) -((x - d)^2))
  utt.pref = function(u) {
    interp.probs = listener.probs[,which(utterances==u)]
    expected.utility = sum(interp.probs * utility.by.degree) - cost(u)
    return(exp(lambda * expected.utility))
  }
  utt.probs = Normalize(sapply(utterances, utt.pref))
  return(utt.probs)
}

# taking speaker prefs to be a model of intuitive 'appropriateness':
theory.prefs = function(theory.name) {
  p = sapply(degrees, function(d) speaker.prefs(d, theory.name))
  rownames(p) = utterances
  colnames(p) = paste('d=', round(degrees,2), sep='')
  return(p)
}
fuzzy.classic.prefs = theory.prefs('fuzzy-classic')
fuzzy.rescaled.prefs = theory.prefs('fuzzy-rescaled')
mult.sampling.prefs = theory.prefs('mult-sampling')

cols = rainbow(length(utterances))
plot.utt.prefs = function(theory.name) {
  prefs = switch(theory.name, 
                 'fuzzy-classic' = fuzzy.classic.prefs,
                 'fuzzy-rescaled' = fuzzy.rescaled.prefs,
                 'mult-sampling' = mult.sampling.prefs
                 )
  plot(degrees, prefs[1,], col=cols[1], type='l', xlim=c(0,maximum), ylim=c(min(prefs),max(prefs)), main=paste('Speaker prefs:', theory.name))
  for (i in 2:length(utterances)) {
    lines(degrees, prefs[i,], col=cols[i])
  }
  legend('topright', utterances, text.col=cols, col=colss, lty=1, cex=.4)
}

par(mfrow=c(2,1))
#plot.utt.prefs('fuzzy-classic')
#plot.utt.prefs('fuzzy-rescaled')
plot.utt.prefs('mult-sampling')

plot.interp = function(theory.name) {
  prefs = switch(theory.name, 
                 'fuzzy-classic' = t(listener.fuzzy.classic),
                 'fuzzy-rescaled' = t(listener.fuzzy.rescaled),
                 'mult-sampling' = t(listener.mult.sampling)
  )
  plot(degrees, prefs[1,], col=cols[1], type='l', xlim=c(0,maximum), ylim=c(min(prefs), max(prefs)), main=paste('Interp:', theory.name))
  for (i in 2:length(utterances)) {
    lines(degrees, prefs[i,], col=cols[i])
  }
  legend('topright', utterances, text.col=cols, col=cols, lty=1, cex=.4)
}

#par(mfrow=c(1,1))
#plot.interp('fuzzy-classic')
#plot.interp('fuzzy-rescaled')
plot.interp('mult-sampling')

# implementation of metalinguistic negation for not P and not not-P?

