library(ggplot2)
library(grid)
library(hexbin)

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
#rm <- which(names(data) %in% c('N_unc', 'N_cen'))
#flat <- data[-rm]
fs <- Reduce(cbind, data[1:5])
sn <- Reduce(cbind, data[9:13])
flat <- rbind(data.frame(fs), sn)
names(flat) <- c('dur', 'size', 'aff', 'hab', 'occ')

# constants
samp <- nrow(flat)
n.sim <- 1000  

# posterior simulations
sims <- extract(mixfit, permuted = TRUE)
n.post <- length(sims$lp__)

preds <- data.frame(sims$beta)
names(preds) <- c('size', 'occ', 'habitat', 'substrate')
long.preds <- melt(preds)
names(long.preds) <- c('val', 'sim')

pshap <- data.frame(sim = sims$alpha)
names(pshap) <- c('alpha_1', 'alpha_2')
pshap <- melt(pshap)
names(pshap) <- c('val', 'sim')


# posterior simulation graph
posts <- rbind(long.preds, pshap)
gpost <- ggplot(posts, aes(x = sim))
gpost <- gpost + geom_vline(xintercept = 0, colour = 'grey', size = 2)
gpost <- gpost + geom_histogram(aes(y = ..density..), 
                                binwidth = 1/20)
gpost <- gpost + facet_grid(val ~ ., scales = 'free')
gpost <- gpost + labs(x = 'Value', y = 'Density')
gpost <- gpost + theme(axis.text.y = element_text(size = 15))
ggsave(gpost, filename = '../doc/figure/mix_post.png', 
       width = 15, height = 10)



# esimate of mean duration
y.rep <- array(NA, c(samp, n.sim))
guess <- runif(n.sim)
for(s in seq(n.sim)) {
  for(i in seq(samp)) {
    p <- sample(n.post, 1)
    
    mixing <- sims$theta[p, 1]

    ints <- exp(-(sum(preds[p, ] * unlist(flat[i, 2:5]))) / sims$alpha[p])

    if(guess[s] < mixing) {
      y.rep[i, s] <- rweibull(1, shape = sims$alpha[p, 2], 
                             scale = ints)
    } else {
      y.rep[i, s] <- rweibull(1, shape = sims$alpha[p, 1], 
                             scale = ints)
    }
  }
}
#y.rep <- ceiling(y.rep)
sim.mean <- colMeans(y.rep)
dur.mean <- mean(flat$dur)
gmean <- ggplot(data.frame(x = sim.mean), aes(x = x))
gmean <- gmean + geom_histogram(aes(y = ..density..), binwidth = 0.5)
gmean <- gmean + geom_vline(xintercept = dur.mean, colour = 'blue', size = 2)
gmean <- gmean + labs(x = 'Duration', y = 'Density')
ggsave(gmean, filename = '../doc/figure/mix_mean_ppc.png',
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
gquant <- gquant + labs(x = 'Duration', y = 'Density')
gquant <- gquant + facet_grid(. ~ Var2, labeller = mf_labeller)
ggsave(gquant, filename = '../doc/figure/mix_quant_ppc.png',
       width = 15, height = 10)


durs <- rbind(cbind(data.frame(dur = data$dur_unc), type = rep('No', data$N_unc)),
              cbind(dur = data$dur_cen, type = rep('Yes', data$N_cen)))
durs$dur <- as.numeric(durs$dur)

dists <- ggplot(durs, aes(x = dur))
dists <- dists + geom_histogram(aes(y = ..density..), 
                                binwidth = 1, fill = 'grey')
dists <- dists + scale_fill_manual(values = cbp,
                                   name = 'Censored')
dists <- dists + labs(y = 'Density', x = 'Duration')
ggsave(dists, filename = '../doc/figure/mix_dur.png',
       width = 15, height = 10)

hist.sim <- melt(y.rep)
hist.sim <- hist.sim[hist.sim[, 1] %in% 1:12, ]
ghist <- ggplot(hist.sim, aes(x = value))
ghist <- ghist + geom_histogram(aes(y = ..density..), 
                                binwidth = 1, fill = 'blue', alpha = 0.4)
ghist <- ghist + geom_histogram(data = durs, aes(x = dur, y = ..density..),
                                binwidth = 1, fill = 'grey', alpha = 0.5)
ghist <- ghist + facet_wrap( ~ Var1)
ghist <- ghist + labs(y = 'Density', x = 'Duration')
ggsave(ghist, filename = '../doc/figure/mix_dur_post.png',
       width = 15, height = 10)
