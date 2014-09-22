library(plyr)
library(parallel)
library(doParallel)
library(reshape2)
library(ggplot2)
library(grid)
library(scales)

RNGkind(kind = "L'Ecuyer-CMRG")
registerDoParallel(cores = detectCores())
set.seed(420)

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')

gamble <- function(init, loss = 0.5, win = 0) {
  t <- 0
  n <- init
  pot <- data.frame(t = t, pot = n)
  while(n > 0) {
    dt <- rexp(1, rate = loss * n + win * n)
    t <- t + dt
    n <- n + sample(c(-1, 1), 1, prob = c(loss * n, win * n))
    pot <- rbind(pot, c(t, n))
  }

  pot[-nrow(pot), ]
}

sim <- 100
pot.small <- 10
pot.med <- 50
pot.big <- 100
go.small <- go.med <-  go.big <- list()
for(ii in seq(sim)) {
  go.small[[ii]] <- gamble(pot.small)
  go.med[[ii]] <- gamble(pot.med)
  go.big[[ii]] <- gamble(pot.big)

  go.small[[ii]] <- cbind(go.small[[ii]], sim = rep(ii, nrow(go.small[[ii]])))
  go.med[[ii]] <- cbind(go.med[[ii]], sim = rep(ii, nrow(go.med[[ii]])))
  go.big[[ii]] <- cbind(go.big[[ii]], sim = rep(ii, nrow(go.big[[ii]])))
}

go.small <- Reduce(rbind, go.small)
go.small <- data.frame(go.small)

go.med <- Reduce(rbind, go.med)
go.med <- data.frame(go.med)

go.big <- Reduce(rbind, go.big)
go.big <- data.frame(go.big )

go.small <- cbind(go.small, type = rep('10', nrow(go.small)))
go.med <- cbind(go.med, type = rep('50', nrow(go.med)))
go.big <- cbind(go.big, type = rep('100', nrow(go.big)))
go <- rbind(go.small, go.med, go.big)

gambling <- ggplot(go, aes(y = pot, x = t, 
                           group = interaction(sim, type), 
                           colour = type))
gambling <- gambling + geom_line(alpha = 0.3)
gambling <- gambling + scale_y_continuous(trans = log10_trans())
gambling <- gambling + scale_color_manual(values = cbp,
                                          name = 'Starting chips')
gambling <- gambling + labs(x = 'Time', y = 'Chip count')
gambling <- gambling + theme(axis.title.y = element_text(angle = 0),
                             axis.text = element_text(size = 27),
                             axis.title = element_text(size = 30),
                             legend.text = element_text(size = 25),
                             legend.title = element_text(size = 26),
                             legend.key.size = unit(2, 'cm'),
                             strip.text = element_text(size = 25))
ggsave(plot = gambling, file = '../doc/figure/gambling.png',
       width = 15, height = 10)


###### same size different rates
sim <- 100
same <- 100
low <- high <- list()
for(ii in seq(sim)) {
  low[[ii]] <- gamble(same, loss = 0.05)
  high[[ii]] <- gamble(same, loss = 0.1)

  low[[ii]] <- cbind(low[[ii]], sim = rep(ii, nrow(low[[ii]])))
  high[[ii]] <- cbind(high[[ii]], sim = rep(ii, nrow(high[[ii]])))
}

low <- Reduce(rbind, low)
low <- data.frame(low)

high <- Reduce(rbind, high)
high <- data.frame(high)

low <- cbind(low, type = rep('0.05', nrow(low)))
high <- cbind(high, type = rep('0.1', nrow(high)))
sames <- rbind(low, high)

gsame <- ggplot(sames, aes(y = pot, x = t, 
                           group = interaction(sim, type), 
                           colour = type))
gsame <- gsame + geom_line(alpha = 0.3)
gsame <- gsame + scale_y_continuous(trans = log10_trans())
gsame <- gsame + scale_color_manual(values = cbp,
                                    name = 'Loss rate')
gsame <- gsame + labs(x = 'Time', y = 'Chip count')
gsame <- gsame + theme(axis.title.y = element_text(angle = 0),
                       axis.text = element_text(size = 27),
                       axis.title = element_text(size = 30),
                       legend.text = element_text(size = 25),
                       legend.title = element_text(size = 26),
                       legend.key.size = unit(2, 'cm'),
                       strip.text = element_text(size = 25))
ggsave(plot = gsame, file = '../doc/figure/gsame.png',
       width = 15, height = 10)
