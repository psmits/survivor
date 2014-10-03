library(ggplot2)
library(grid)

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

# make flat data
rm <- which(names(data) %in% c('N_unc', 'N_cen'))
flat <- data[-rm]
fs <- Reduce(cbind, flat[1:4])
sn <- Reduce(cbind, flat[5:9])
flat <- rbind(data.frame(fs), sn)
names(flat) <- c('dur', 'size', 'hab', 'occ')

# constants
samp <- nrow(flat)
n.sim <- 100  

# posterior simulations
sims <- extract(wfit, permuted = TRUE)
n.post <- length(sims$lp__)

preds <- data.frame(sims$beta)
names(preds) <- c('constant', 'beta_size', 'beta_occ', 'beta_hab')
long.preds <- melt(preds)
names(long.preds) <- c('val', 'sim')

pshap <- cbind(data.frame(val = rep('v', length(sims$alpha))), sim = sims$alpha)


# posterior simulation graph
posts <- rbind(long.preds, pshap)
gpost <- ggplot(posts, aes(x = sim))
gpost <- gpost + geom_histogram(aes(y = ..density..), binwidth = 1/20)
gpost <- gpost + facet_grid(val ~ .)
gpost <- gpost + labs(x = 'value', y = 'density')
ggsave(gpost, filename = '../doc/figure/wei_post.png', 
       width = 15, height = 10)


# overlay sampled weibull distributions over empirical histogram
durs <- rbind(cbind(data.frame(dur = data$dur_unc), type = rep('No', data$N_unc)),
              cbind(dur = data$dur_cen, type = rep('Yes', data$N_cen)))
durs$dur <- as.numeric(durs$dur)

dists <- ggplot(durs, aes(x = dur))
dists <- dists + geom_histogram(aes(y = ..density..), 
                                binwidth = 1, fill = 'grey')
dists <- dists + scale_fill_manual(values = cbp,
                                   name = 'Censored')
dists <- dists + labs(y = 'Density', x = 'Duration')
for(i in seq(n.sim)) {
  p <- sample(n.post, 1)
  w <- sample(samp, 1)
  ints <- exp(sum(preds[p, ] * unlist(c(1, flat[w, 2:4]))))
  dists <- dists + stat_function(fun = dweibull, 
                                 size = 1.5, 
                                 alpha = 0.05,
                                 arg = list(shape = sims$alpha[p], 
                                            scale = ints),
                                 colour = 'blue')
}
ggsave(dists, filename = '../doc/figure/wei_dur_post.png',
       width = 15, height = 10)


# esimate of mean duration
y.rep <- array(NA, c(samp, n.sim))
for(s in seq(n.sim)) {
  p <- sample(n.post, 1)
  w <- sample(samp, 1)
  ints <- exp(sum(preds[p, ] * unlist(c(1, flat[w, 2:4]))))
  y.rep[, s] <- rweibull(samp, shape = sims$alpha[p], 
                         scale = ints)
}
sim.mean <- colMeans(y.rep)
dur.mean <- mean(flat$dur)
gmean <- ggplot(data.frame(x = sim.mean), aes(x = x))
gmean <- gmean + geom_histogram(aes(y = ..density..), binwidth = 2)
gmean <- gmean + geom_vline(xintercept = dur.mean, colour = 'blue', size = 2)
gmean <- gmean + labs(x = 'duration time', y = 'density')
ggsave(gmean, filename = '../doc/figure/wei_mean_ppc.png',
       width = 15, height = 10)
