library(ggplot2)
library(grid)

source('../R/bayes_survival.r')

# make flat data
rm <- which(names(data) %in% c('N_unc', 'N_cen'))
flat <- data[-rm]
fs <- Reduce(cbind, flat[1:5])
sn <- Reduce(cbind, flat[6:10])
flat <- rbind(data.frame(fs), sn)
names(flat) <- c('dur', 'size', 'aff', 'hab', 'occ')

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

samp <- sum(data$N_unc, data$N_cen)
n.sim <- length(sims$lp__)

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

# make a bunch of weibull distributions
durs <- rbind(cbind(data.frame(dur = data$dur_unc), type = rep('No', data$N_unc)),
              cbind(dur = data$dur_cen, type = rep('Yes', data$N_cen)))
durs$dur <- as.numeric(durs$dur)

dists <- ggplot(durs, aes(x = dur))
dists <- dists + geom_histogram(aes(y = ..density..), 
                                binwidth = 1, fill = 'grey')
dists <- dists + scale_fill_manual(values = cbp,
                                   name = 'Censored')
dists <- dists + labs(y = 'Density', x = 'Duration')
for(i in seq(1000)) {
  w <- sample(n.sim, 1)
  ints <- exp(pint[w, 2] + psize[w, 2] * sample(flat$size, 1) + 
              paff[w, 2] * sample(flat$aff, 1) +
              pocc[w, 2] * sample(flat$occ, 1) +
              phab[w, 2] * sample(flat$hab, 1))
  dists <- dists + stat_function(fun = dweibull, 
                                 size = 1.5, 
                                 alpha = 0.01,
                                 arg = list(shape = pshap[w, 2], 
                                            scale = ints),
                                 colour = 'blue')
}
ggsave(dists, filename = '../doc/figure/dur_post.png',
       width = 15, height = 10)




# posterior predictive checks

# mean duration given most average specimen
m.rep <- array(NA, c(samp, n.sim))
for(s in seq(n.sim)) {
  m.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                         scale = exp(sims$beta[s, 1] + 
                                     sims$beta[s, 2] * 0 + 
                                     sims$beta[s, 3] * 0 + 
                                     sims$beta[s, 4] * 0 +
                                     sims$beta[s, 5] * 0))
}
sim.med <- apply(m.rep, 2, median)
sim.mean <- colMeans(m.rep)
dur.med <- median(c(data$dur_unc, data$dur_cen)) 
dur.mean <- mean(c(data$dur_unc, data$dur_cen)) 
# fantastic estimates of the median
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

min.aff <- flat[which.min(flat$aff), ]

mid <- ceiling(length(sort(flat$aff)) / 2)
sor <- flat[order(flat$aff), ]
med.aff <- sor[mid, ]

max.aff <- flat[which.max(flat$aff), ]

for(s in seq(n.sim)) {
  a0.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                          scale = exp(sims$beta[s, 1] + 
                                      sims$beta[s, 2] * 0 + 
                                      sims$beta[s, 3] * min.aff$aff +
                                      sims$beta[s, 4] * 0 +
                                      sims$beta[s, 5] * 0))
  a5.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                          scale = exp(sims$beta[s, 1] + 
                                      sims$beta[s, 2] * 0 +
                                      sims$beta[s, 3] * med.aff$aff +
                                      sims$beta[s, 4] * 0 +
                                      sims$beta[s, 5] * 0))
  a1.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                          scale = exp(sims$beta[s, 1] + 
                                      sims$beta[s, 2] * 0 +
                                      sims$beta[s, 3] * max.aff$aff + 
                                      sims$beta[s, 4] * 0 +
                                      sims$beta[s, 5] * 0))
}
a0.mean <- colMeans(a0.rep)
a5.mean <- colMeans(a5.rep)
a1.mean <- colMeans(a1.rep)

aff.int <- data.frame(pnt = c(min.aff$dur, med.aff$dur, max.aff$dur), 
                      lvl = c('min', 'med', 'max'))

aff.sims <- data.frame(lvl = c(rep('min', length(a0.mean)),
                               rep('med', length(a5.mean)),
                               rep('max', length(a1.mean))),
                       val = c(a0.mean, a5.mean, a1.mean))

# size
s0.rep <- array(NA, c(samp, n.sim))
s5.rep <- array(NA, c(samp, n.sim))
s1.rep <- array(NA, c(samp, n.sim))

min.siz <- flat[which.min(flat$siz), ]

mid <- ceiling(length(sort(flat$siz)) / 2)
sor <- flat[order(flat$siz), ]
med.siz <- sor[mid, ]

max.siz <- flat[which.max(flat$siz), ]

for(s in seq(n.sim)) {
  s0.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                          scale = exp(sims$beta[s, 1] + 
                                      sims$beta[s, 2] * med.siz$size + 
                                      sims$beta[s, 3] * 0 +
                                      sims$beta[s, 4] * 0 +
                                      sims$beta[s, 5] * 0))
  s5.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                          scale = exp(sims$beta[s, 1] + 
                                      sims$beta[s, 2] * med.siz$size + 
                                      sims$beta[s, 3] * 0 +
                                      sims$beta[s, 4] * 0 +
                                      sims$beta[s, 5] * 0))
  s1.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                          scale = exp(sims$beta[s, 1] + 
                                      sims$beta[s, 2] * max.siz$size + 
                                      sims$beta[s, 3] * 0 + 
                                      sims$beta[s, 4] * 0 +
                                      sims$beta[s, 5] * 0))
}
s0.mean <- colMeans(s0.rep)
s5.mean <- colMeans(s5.rep)
s1.mean <- colMeans(s1.rep)

siz.int <- data.frame(pnt = c(min.siz$dur, med.siz$dur, max.siz$dur), 
                      lvl = c('min', 'med', 'max'))

siz.sims <- data.frame(lvl = c(rep('min', length(s0.mean)),
                               rep('med', length(s5.mean)),
                               rep('max', length(s1.mean))),
                       val = c(s0.mean, s5.mean, s1.mean))

# habitat
h0.rep <- array(NA, c(samp, n.sim))
h5.rep <- array(NA, c(samp, n.sim))
h1.rep <- array(NA, c(samp, n.sim))

min.hab <- flat[which.min(flat$hab), ]

mid <- ceiling(length(sort(flat$hab)) / 2)
sor <- flat[order(flat$hab), ]
med.hab <- sor[mid, ]

max.hab <- flat[which.max(flat$hab), ]

for(s in seq(n.sim)) {
  h0.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                          scale = exp(sims$beta[s, 1] + 
                                      sims$beta[s, 2] * 0 + 
                                      sims$beta[s, 3] * 0 +
                                      sims$beta[s, 4] * 0 +
                                      sims$beta[s, 5] * min.hab$hab))
  h5.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                          scale = exp(sims$beta[s, 1] + 
                                      sims$beta[s, 2] * 0 + 
                                      sims$beta[s, 3] * 0 +
                                      sims$beta[s, 4] * 0 +
                                      sims$beta[s, 5] * med.hab$hab))
  h1.rep[, s] <- rweibull(samp, shape = sims$alpha[s], 
                          scale = exp(sims$beta[s, 1] + 
                                      sims$beta[s, 2] * 0 + 
                                      sims$beta[s, 3] * 0 + 
                                      sims$beta[s, 4] * 0 +
                                      sims$beta[s, 5] * max.hab$hab))
}
h0.mean <- colMeans(h0.rep)
h5.mean <- colMeans(h5.rep)
h1.mean <- colMeans(h1.rep)

hab.int <- data.frame(pnt = c(min.hab$dur, med.hab$dur, max.hab$dur), 
                      lvl = c('min', 'med', 'max'))

hab.sims <- data.frame(lvl = c(rep('min', length(s0.mean)),
                               rep('med', length(s5.mean)),
                               rep('max', length(s1.mean))),
                       val = c(h0.mean, h5.mean, s1.mean))


aff.sims <- cbind(type = rep('affinity', nrow(aff.sims)), aff.sims)
siz.sims <- cbind(type = rep('size', nrow(siz.sims)), siz.sims)
hab.sims <- cbind(type = rep('habitat', nrow(hab.sims)), hab.sims)
bio.sims <- rbind(aff.sims, siz.sims, hab.sims)

aff.int <- cbind(type = rep('affinity', nrow(aff.int)), aff.int)
siz.int <- cbind(type = rep('size', nrow(siz.int)), siz.int)
hab.int <- cbind(type = rep('habitat', nrow(hab.int)), hab.int)
bio.int <- rbind(aff.int, siz.int, hab.int)


gbio <- ggplot(bio.sims, aes(x = val))
gbio <- gbio + geom_histogram(aes(y = ..density..))
gbio <- gbio + geom_vline(data = bio.int, aes(xintercept = pnt), 
                          colour = 'blue', size = 3)
gbio <- gbio + facet_grid(lvl ~ type)
gbio <- gbio + labs(x = 'time', y = 'density')
ggsave(gbio, filename = '../doc/figure/bio_ppc.png',
       width = 15, height = 10)
