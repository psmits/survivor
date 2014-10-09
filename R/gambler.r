library(plyr)
library(reshape2)
library(ggplot2)
library(grid)
library(scales)

set.seed(420)

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')
theme_update(axis.text = element_text(size = 32),
             axis.title = element_text(size = 35),
             axis.title.y = element_text(hjust = 0.1),
             legend.text = element_text(size = 25),
             legend.title = element_text(size = 26),
             legend.key.size = unit(2, 'cm'),
             strip.text = element_text(size = 30))

# exponential
lam <- 1
k <- 1
durs <- rweibull(1000, k, lam)
surv <- exp(-lam * durs^k)
haz <- lam * k * durs^(k - 1)

survival <- data.frame(durs = durs, surv = surv, haz = haz)
gd <- ggplot(survival, aes(x = durs))
gd <- gd + geom_histogram(aes(y = ..density..), fill = 'grey')
gd <- gd + stat_function(fun = dweibull, colour = 'blue', size = 3,
                         arg = list(shape = k, scale = lam))
gd <- gd + labs(x = 'Duration', y = 'Density')
ggsave(gd, filename = '../doc/figure/dur_exp.png', width = 15, height = 10)

gsv <- ggplot(survival, aes(x = durs, y = surv))
gsv <- gsv + geom_line(size = 3)
gsv <- gsv + scale_y_continuous(trans = log10_trans())
gsv <- gsv + labs(x = 'Duration', y = 'P(T > t)')
ggsave(gsv, filename = '../doc/figure/sur_exp.png', width = 15, height = 10)

ghz <- ggplot(survival, aes(x = durs, y = haz))
ghz <- ghz + geom_line(size = 3)
ghz <- ghz + scale_y_continuous(trans = log10_trans())
ghz <- ghz + labs(x = 'Duration', y = 'h(t)')
ggsave(ghz, filename = '../doc/figure/haz_exp.png', width = 15, height = 10)


# dec
lam <- 1
k <- 0.5
durs <- rweibull(1000, k, lam)
surv <- exp(-lam * durs^k)
haz <- lam * k * durs^(k - 1)

survival <- data.frame(durs = durs, surv = surv, haz = haz)
gdd <- ggplot(survival, aes(x = durs))
gdd <- gdd + geom_histogram(aes(y = ..density..), fill = 'grey')
gdd <- gdd + stat_function(fun = dweibull, 
                           data = data.frame(ss = seq(0, 50, 0.1)),
                           mapping = aes(x = ss),
                           colour = 'blue', size = 3,
                           arg = list(shape = k, scale = lam))
gdd <- gdd + labs(x = 'Duration', y = 'Density')
ggsave(gdd, filename = '../doc/figure/dur_dec.png', width = 15, height = 10)

gsvd <- ggplot(survival, aes(x = durs, y = surv))
gsvd <- gsvd + geom_line(size = 3)
gsvd <- gsvd + scale_y_continuous(trans = log10_trans())
gsvd <- gsvd + labs(x = 'Duration', y = 'P(T > t)')
ggsave(gsvd, filename = '../doc/figure/sur_dec.png', width = 15, height = 10)

ghzd <- ggplot(survival, aes(x = durs, y = haz))
ghzd <- ghzd + geom_line(size = 3)
ghzd <- ghzd + scale_y_continuous(trans = log10_trans())
ghzd <- ghzd + labs(x = 'Duration', y = 'h(t)')
ggsave(ghzd, filename = '../doc/figure/haz_dec.png', width = 15, height = 10)


# acc
lam <- 1
k <- 1.5
durs <- rweibull(1000, k, lam)
surv <- exp(-lam * durs^k)
haz <- lam * k * durs^(k - 1)

survival <- data.frame(durs = durs, surv = surv, haz = haz)
gda <- ggplot(survival, aes(x = durs))
gda <- gda + geom_histogram(aes(y = ..density..), fill = 'grey')
gda <- gda + stat_function(fun = dweibull, colour = 'blue', size = 3,
                           arg = list(shape = k, scale = lam))
gda <- gda + labs(x = 'Duration', y = 'Density')
ggsave(gda, filename = '../doc/figure/dur_acc.png', width = 15, height = 10)

gsva <- ggplot(survival, aes(x = durs, y = surv))
gsva <- gsva + geom_line(size = 3)
gsva <- gsva + scale_y_continuous(trans = log10_trans())
gsva <- gsva + labs(x = 'Duration', y = 'P(T > t)')
ggsave(gsva, filename = '../doc/figure/sur_acc.png', width = 15, height = 10)

ghza <- ggplot(survival, aes(x = durs, y = haz))
ghza <- ghza + geom_line(size = 3)
ghza <- ghza + scale_y_continuous(trans = log10_trans())
ghza <- ghza + labs(x = 'Duration', y = 'h(t)')
ggsave(ghza, filename = '../doc/figure/haz_acc.png', width = 15, height = 10)
