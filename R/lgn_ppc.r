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
sims <- extract(lfit, permuted = TRUE)
n.post <- length(sims$lp__)

preds <- data.frame(sims$beta)
names(preds) <- c('constant', 'beta_size', 'beta_occ', 'beta_hab')
long.preds <- melt(preds)
names(long.preds) <- c('val', 'sim')

psigma <- cbind(data.frame(val = rep('v', length(sims$sigma))), sim = sims$sigma)


# posterior simulation graph
posts <- rbind(long.preds, psigma)
gpost <- ggplot(posts, aes(x = sim))
gpost <- gpost + geom_histogram(aes(y = ..density..), binwidth = 1/20)
gpost <- gpost + facet_grid(val ~ .)
gpost <- gpost + labs(x = 'value', y = 'density')
ggsave(gpost, filename = '../doc/figure/lgn_post.png', 
       width = 15, height = 10)


# overlay sampled log-normal distributions over empirical histogram
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
  dists <- dists + stat_function(fun = dlnorm, 
                                 size = 1.5, 
                                 alpha = 0.05,
                                 arg = list(sdlog = sims$sigma[p], 
                                            meanlog = ints),
                                 colour = 'blue')
}
ggsave(dists, filename = '../doc/figure/lgn_dur_post.png',
       width = 15, height = 10)


# esimate of mean duration
y.rep <- array(NA, c(samp, n.sim))
for(s in seq(n.sim)) {
  p <- sample(n.post, 1)
  w <- sample(samp, 1)
  ints <- exp(sum(preds[p, ] * unlist(c(1, flat[w, 2:4]))))
  y.rep[, s] <- rlnorm(samp, sdlog = sims$sigma[p], 
                         meanlog = ints)
}
sim.mean <- colMeans(y.rep)
dur.mean <- mean(flat$dur)
gmean <- ggplot(data.frame(x = sim.mean), aes(x = x))
gmean <- gmean + geom_histogram(aes(y = ..density..), binwidth = 2)
gmean <- gmean + geom_vline(xintercept = dur.mean, colour = 'blue', size = 2)
gmean <- gmean + labs(x = 'duration time', y = 'density')
ggsave(gmean, filename = '../doc/figure/lgn_mean_ppc.png',
       width = 15, height = 10)

# 25th and 75th quantiles
sim.25 <- apply(y.rep, 2, quantile, probs = 0.25)
dur.25 <- quantile(flat$dur, probs = 0.25)
sim.75 <- apply(y.rep, 2, quantile, probs = 0.75)
dur.75 <- quantile(flat$dur, probs = 0.75)

sim.quant <- melt(cbind(low = sim.25, high = sim.75))[, 2:3]
dur.quant <- melt(cbind(low = dur.25, high = dur.75))[, 2:3]

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Var2") { 
    value[value=="low"] <- "25th"
    value[value=="high"]   <- "75th"
  }
  return(value)
}

gquant <- ggplot(sim.quant, aes(x = value))
gquant <- gquant + geom_histogram(aes(y = ..density..), binwidth = 1)
gquant <- gquant + geom_vline(data = dur.quant, aes(xintercept = value), 
                              colour = 'blue', size = 2)
gquant <- gquant + labs(x = 'duration time', y = 'density')
gquant <- gquant + facet_grid(. ~ Var2, labeller = mf_labeller)
ggsave(gquant, filename = '../doc/figure/lgn_quant_ppc.png',
       width = 15, height = 10)
