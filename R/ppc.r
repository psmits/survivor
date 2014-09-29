library(ggplot2)
library(grid)

#source('../R/bayes_survival.r')

# look at posteriors
sims <- extract(fit1, permuted = TRUE)
pint <- sims$beta[, 1]
psize <- sims$beta[, 2]
paff <- sims$beta[, 3]
pocc <- sims$beta[, 4]
phab <- sims$beta[, 5]
shape <- sims$alpha

pint <- cbind(data.frame(val = rep('constant', length(pint))), sim = pint)
psize <- cbind(data.frame(val = rep('beta_{size}', length(psize))), sim = psize)
paff <- cbind(data.frame(val = rep('beta_{aff}', length(paff))), sim = paff)
pocc <- cbind(data.frame(val = rep('beta_{occ}', length(pocc))), sim = pocc)
phab <- cbind(data.frame(val = rep('beta_{hab}', length(phab))), sim = phab)
pshap <- cbind(data.frame(val = rep('v', length(shape))), sim = shape)

posts <- rbind(pint, psize, paff, pocc, phab, pshap)

# graphs
theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')
theme_update(axis.text = element_text(size = 20),
             axis.title = element_text(size = 30),
             axis.title.y = element_text(hjust = 0.1),
             legend.text = element_text(size = 25),
             legend.title = element_text(size = 26),
             legend.key.size = unit(2, 'cm'),
             strip.text = element_text(size = 20))

# posterior estimates
gpost <- ggplot(posts, aes(x = sim))
gpost <- gpost + geom_histogram(aes(y = ..density..), binwidth = 1/20)
gpost <- gpost + facet_grid(val ~ .)
gpost <- gpost + labs(x = 'value', y = 'density')
ggsave(gpost, filename = '../doc/figure/post.png', 
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
sim.mean <- colMeans(m.rep)
dur.mean <- mean(c(data$dur_unc, data$dur_cen)) 
gmean <- ggplot(data.frame(x = sim.mean), aes(x = x))
gmean <- gmean + geom_histogram(aes(y = ..density..))
gmean <- gmean + geom_vline(xintercept = dur.mean, colour = 'blue', size = 3)
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
min.aff <- aff.dur[which.min(aff.obs), 1]
med.aff <- aff.dur[which(aff.obs == median(aff.obs)), 1]
max.aff <- aff.dur[which.max(aff.obs), 1]
aff.int <- data.frame(pnt = c(min.aff, med.aff, max.aff), 
                     lvl = c('min', 'med', 'max'))

aff.sims <- data.frame(lvl = c(rep('min', length(a0.mean)),
                               rep('med', length(a5.mean)),
                               rep('max', length(a1.mean))),
                       val = c(a0.mean, a5.mean, a1.mean))

# affinity
s0.rep <- array(NA, c(samp, n.sim))
s5.rep <- array(NA, c(samp, n.sim))
s1.rep <- array(NA, c(samp, n.sim))

siz.obs <- c(data$size_unc, data$size_cen)

for(s in seq(n.sim)) {
  s0.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                         scale = exp(sims$beta[s, 1] + 
                                     sims$beta[s, 2] * min(siz.obs) + 
                                     sims$beta[s, 3] * 0 + 
                                     sims$beta[s, 4] * 0 +
                                     sims$beta[s, 5] * 0))
  s5.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                         scale = exp(sims$beta[s, 1] + 
                                     sims$beta[s, 2] * median(siz.obs) + 
                                     sims$beta[s, 3] * 0 + 
                                     sims$beta[s, 4] * 0 +
                                     sims$beta[s, 5] * 0))
  s1.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                         scale = exp(sims$beta[s, 1] + 
                                     sims$beta[s, 2] * max(siz.obs) + 
                                     sims$beta[s, 3] * 0 + 
                                     sims$beta[s, 4] * 0 +
                                     sims$beta[s, 5] * 0))
}
s0.mean <- colMeans(s0.rep)
s5.mean <- colMeans(s5.rep)
s1.mean <- colMeans(s1.rep)

siz.dur <- cbind(c(data$dur_unc, data$dur_cen), siz.obs)
min.siz <- siz.dur[which.min(siz.obs), 1]
med.siz <- siz.dur[which(siz.obs == median(siz.obs)), 1]
max.siz <- siz.dur[which.max(siz.obs), 1]
siz.int <- data.frame(pnt = c(min.siz, med.siz, max.siz), 
                     lvl = c('min', 'med', 'max'))

siz.sims <- data.frame(lvl = c(rep('min', length(s0.mean)),
                               rep('med', length(s5.mean)),
                               rep('max', length(s1.mean))),
                       val = c(s0.mean, s5.mean, s1.mean))

aff.sims <- cbind(type = rep('affinity', nrow(aff.sims)), aff.sims)
siz.sims <- cbind(type = rep('size', nrow(siz.sims)), siz.sims)
bio.sims <- rbind(aff.sims, siz.sims)

aff.int <- cbind(type = rep('affinity', nrow(aff.int)), aff.int)
siz.int <- cbind(type = rep('size', nrow(siz.int)), siz.int)
bio.int <- rbind(aff.int, siz.int)


gbio <- ggplot(bio.sims, aes(x = val))
gbio <- gbio + geom_histogram(aes(y = ..density..))
gbio <- gbio + geom_vline(data = bio.int, aes(xintercept = pnt), 
                          colour = 'blue', size = 3)
gbio <- gbio + facet_grid(lvl ~ type)
gbio <- gbio + labs(x = 'time', y = 'density')
ggsave(gbio, filename = '../doc/figure/bio_ppc.png',
       width = 15, height = 10)
