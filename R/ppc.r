library(ggplot2)
library(grid)

source('../R/bayes_survival.r')

# look at posteriors
sims <- extract(fit1, permuted = TRUE)
pint <- sims$beta[, 1]
psize <- sims$beta[, 2]
paff <- sims$beta[, 3]
pocc <- sims$beta[, 4]
phab <- sims$beta[, 5]
shape <- sims$alpha

pint <- cbind(data.frame(val = rep('intercept', length(pint))), sim = pintercept)
psize <- cbind(data.frame(val = rep('beta_{size}', length(psize))), sim = psize)
paff <- cbind(data.frame(val = rep('beta_{affinity}', length(paff))), sim = paff)
pocc <- cbind(data.frame(val = rep('beta_{occurrence}', length(pocc))), sim = pocc)
phab <- cbind(data.frame(val = rep('beta_{habitat}', length(phab))), sim = phab)

posts <- rbind(pint, psize, paff, pocc, phab)

# graphs
theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')
theme_update(axis.text = element_text(size = 27),
             axis.title = element_text(size = 30),
             legend.text = element_text(size = 25),
             legend.title = element_text(size = 26),
             legend.key.size = unit(2, 'cm'),
             strip.text = element_text(size = 25))

# posterior estimates
gpost <- ggplot(posts, aes(x = sim))
gpost <- gpost + geom_histogram(aes(y = ..density..), binwidth = 1/20)
gpost <- gpost + facet_grid(val ~ .)
gpost <- gpost + labs(x = 'log value', y = 'density')
ggsave(gpost, filename = '../doc/figure/beta_post.png', 
       width = 15, height = 10)

gshap <- ggplot(data.frame(x = shape), aes(x = x))
gshap <- gshap + geom_histogram(aes(y = ..density..), binwidth = 1/20)
gshap <- gshap + labs(x = 'value', y = 'density')
ggsave(gshap, filename = '../doc/figure/shape_post.png',
       width = 15, height = 10)


# posterior predictive checks
samp <- sum(data$N_unc, data$N_cen)
n.sim <- length(sims$lp__)

# mean duration given no biology
m.rep <- array(NA, c(samp, n.sim))
for(s in seq(n.sim)) {
  m.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                         scale = exp(sims$beta[s, 1] + 
                                     sims$beta[s, 2] * 0 + 
                                     sims$beta[s, 3] * 0 + 
                                     sims$beta[s, 4] * 0 +
                                     sims$beta[s, 5] * 0))
}
sim.mean <- colMeans(y.rep)
dur.mean <- mean(c(data$dur_unc, data$dur_cen)) 
gmean <- ggplot(data.frame(x = sim.mean), aes(x = x))
gmean <- gmean + geom_histogram(aes(y = ..density..))
gmean <- gmean + geom_vline(xintercept = dur.mean, colour = 'blue')
gmean <- gmean + labs(x = 'duration time', y = 'density')
ggsave(gmean, filename = '../doc/figure/mean_ppc.png',
       width = 15, height = 10)


# affinity
a0.rep <- array(NA, c(samp, n.sim))
a5.rep <- array(NA, c(samp, n.sim))
a1.rep <- array(NA, c(samp, n.sim))

aff.obs <- c(data$aff_unc, data$aff_cen)

for(s in seq(n.sim)) {
  a0.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                         scale = exp(sims$beta[s, 1] + 
                                     sims$beta[s, 2] * 0 + 
                                     sims$beta[s, 3] * min(aff.obs) + 
                                     sims$beta[s, 4] * 0 +
                                     sims$beta[s, 5] * 0))
  a5.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                         scale = exp(sims$beta[s, 1] + 
                                     sims$beta[s, 2] * 0 + 
                                     sims$beta[s, 3] * median(aff.obs) + 
                                     sims$beta[s, 4] * 0 +
                                     sims$beta[s, 5] * 0))
  a1.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                         scale = exp(sims$beta[s, 1] + 
                                     sims$beta[s, 2] * 0 + 
                                     sims$beta[s, 3] * max(aff.obs) + 
                                     sims$beta[s, 4] * 0 +
                                     sims$beta[s, 5] * 0))
}
a0.mean <- colMeans(a0.rep)
a5.mean <- colMeans(a5.rep)
a1.mean <- colMeans(a1.rep)

aff.dur <- cbind(c(data$dur_unc, data$dur_cen), aff.obs)
min.dur <- aff.dur[which.min(aff.obs), 1]
med.dur <- aff.dur[which(aff.obs == median(aff.obs)), 1]
max.dur <- aff.dur[which.max(aff.obs), 1]
inteps <- data.frame(pnt = c(min.dur, med.dur, max.dur), 
                     lvl = c('min', 'med', 'max'))

aff.sims <- data.frame(lvl = c(rep('min', length(a0.mean)),
                               rep('med', length(a5.mean)),
                               rep('max', length(a1.mean))),
                       val = c(a0.mean, a5.mean, a1.mean))

gaff <- ggplot(aff.sims, aes(x = val))
gaff <- gaff + geom_histogram(aes(y = ..density..))
gaff <- gaff + geom_vline(data = inteps, aes(xintercept = pnt), colour = 'blue')
gaff <- gaff + facet_grid(. ~ lvl)
gaff <- gaff + labs(x = 'duration time', y = 'density')
ggsave(gaff, filename = '../doc/figure/aff_ppc.png',
       width = 15, height = 10)
