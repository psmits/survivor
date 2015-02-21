library(ggplot2)
library(grid)
library(hexbin)
library(reshape2)
library(stringr)

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')
theme_update(axis.text = element_text(size = 20),
             axis.title = element_text(size = 30),
             axis.title.y = element_text(hjust = 0.1),
             legend.text = element_text(size = 25),
             legend.title = element_text(size = 26),
             legend.key.size = unit(2, 'cm'),
             strip.text = element_text(size = 25))


# make flat data
fs <- Reduce(cbind, data[c(1:5, 9)])
sn <- Reduce(cbind, data[c(11:15, 19)])
flat <- rbind(data.frame(fs), sn)
names(flat) <- c('dur', 'size', 'aff', 'hab', 'occ', 'order')

# constants
samp <- nrow(flat)
n.sim <- 100  

# posterior simulations
sims <- extract(hfit, permuted = TRUE)
n.post <- length(sims$lp__)

melty <- lapply(sims, melt)
for(ii in seq(length(melty))) {
  melty[[ii]] <- cbind(melty[[ii]], val = names(melty)[ii])
}

# individual level regression coefficients
betas <- melty[str_detect(names(melty), 'beta')]
betas <- Reduce(rbind, betas)[, 2:4]
names(betas) <- c('order', 'value', 'param')

# group level regression coeffcient prior hyperparameters
beta.prior <- melty[str_detect(names(melty), 'mu') | 
                    str_detect(names(melty), 'sigma')]

# individual level shape parameter
shapes <- melty$alpha[, 2:3]
names(shapes) <- c('value', 'param')

# posterior simulation graph
y.rep <- array(NA, c(samp, n.sim))
byorder <- split(flat, flat$order)
for(s in seq(n.sim)) {
  rdur <- c()
  for(o in seq(data$O)) {
    number <- nrow(byorder[[o]])
    p <- sample(n.post, size = number)

    bio.preds <- apply((sims$beta[p, ] * byorder[[o]][, 2:5]), 1, sum)
    intercept <- sims$inter[p, o]
    regression <- intercept + bio.preds
    shapes <- sims$alpha[p]

    for(i in seq(number)) {
      rdur <- c(rdur, rweibull(1, shape = shapes[i], 
                               scale = exp(-regression[i] / shapes[i])))
    }
  }
  y.rep[, s] <- rdur
}

# esimate of mean duration
#y.rep <- ceiling(y.rep)
sim.mean <- colMeans(y.rep)
dur.mean <- mean(flat$dur)
gmean <- ggplot(data.frame(x = sim.mean), aes(x = x))
gmean <- gmean + geom_histogram(aes(y = ..density..), binwidth = 0.5)
gmean <- gmean + geom_vline(xintercept = dur.mean, colour = 'blue', size = 2)
gmean <- gmean + labs(x = 'Duration', y = 'Relative\nFrequency')
ggsave(gmean, filename = '../doc/figure/hier_mean_ppc.png',
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
gquant <- gquant + geom_histogram(aes(y = ..density..), binwidth = 0.5)
gquant <- gquant + geom_vline(data = dur.quant, aes(xintercept = value), 
                              colour = 'blue', size = 2)
gquant <- gquant + labs(x = 'Duration', y = 'Relative\nFrequency')
gquant <- gquant + facet_grid(. ~ Var2, labeller = mf_labeller)
ggsave(gquant, filename = '../doc/figure/heir_quant_ppc.png',
       width = 15, height = 10)


durs <- rbind(cbind(data.frame(dur = data$dur_unc), type = rep('No', data$N_unc)),
              cbind(dur = data$dur_cen, type = rep('Yes', data$N_cen)))
durs$dur <- as.numeric(durs$dur)

dists <- ggplot(durs, aes(x = dur))
dists <- dists + geom_histogram(aes(y = ..density..), 
                                binwidth = 1, fill = 'grey')
dists <- dists + scale_fill_manual(values = cbp,
                                   name = 'Censored')
dists <- dists + labs(y = 'Relative\nFrequency', x = 'Duration')
ggsave(dists, filename = '../doc/figure/hier_dur.png',
       width = 15, height = 10)

hist.sim <- melt(y.rep)
hist.sim <- hist.sim[hist.sim[, 1] %in% 1:16, ]
ghist <- ggplot(hist.sim, aes(x = value))
ghist <- ghist + geom_histogram(aes(y = ..density..), 
                                binwidth = 1, fill = 'blue', alpha = 0.4)
ghist <- ghist + geom_histogram(data = durs, aes(x = dur, y = ..density..),
                                binwidth = 1, fill = 'grey', alpha = 0.5)
ghist <- ghist + facet_wrap( ~ Var1)
ghist <- ghist + labs(y = 'Relative\nFrequency', x = 'Duration')
ggsave(ghist, filename = '../doc/figure/hier_dur_post.png',
       width = 15, height = 10)
