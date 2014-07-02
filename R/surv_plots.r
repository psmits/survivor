library(survival)
library(ggplot2)
library(scales)
library(reshape2)
source('../R/step_ribbon.r')

source('../R/para_surv.r')

theme_set(theme_bw())
cbp <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
         "#D55E00", "#CC79A7")
# base
bc <- predict(swei[[1]], type = 'quantile', 
              p = seq(0.01, 0.99, by = 0.01), se.fit = TRUE)
bc <- lapply(bc, function(x) x[1, ])
bc <- lapply(bc, t)
bc <- lapply(bc, melt)
bc <- cbind(fit = bc$fit[, -1], se = bc$se.fit$value)
bc[, 1] <- (100 - bc[, 1]) / 100

reg <- ggplot(bc, aes(x = fit.Var2, y = fit.value))
reg <- reg + geom_line(size = 1)
reg <- reg + geom_ribbon(aes(ymin = fit.value - se, ymax = fit.value + se),
                         alpha = 0.3)
reg <- reg + coord_flip()
reg <- reg + labs(y = 'Time', x = 'P(T > t)')
reg <- reg + scale_x_continuous(trans = log10_trans())
reg <- reg + theme(axis.title.y = element_text(angle = 0),
                   axis.text = element_text(size = 20),
                   axis.title = element_text(size = 23),
                   legend.text = element_text(size = 17),
                   legend.title = element_text(size = 19),
                   strip.text = element_text(size = 20))
ggsave(filename = '../doc/figure/para_reg.png', plot = reg,
       width = 15, height = 10)

# substrate affinity
saf <- data.frame(aff = c(min(persist$aff),
                           quantile(persist$aff, .25),
                           quantile(persist$aff, .5),
                           quantile(persist$aff, .75),
                           max(persist$aff)))
sa <- predict(swei[[2]], newdata = saf,
              type = 'quantile',
              p = seq(0.01, 0.99, by = 0.01), se.fit = TRUE)
rownames(sa$fit) <- rownames(sa$se.fit) <- c('Min', 'Q1', 'Median', 'Q3', 'Max')
sa <- lapply(sa, t)
sa <- lapply(sa, melt)
sa <- cbind(fit = sa$fit, se = sa$se.fit$value)
sa[, 1] <- (100 - sa[, 1]) / 100

gaf <- ggplot(sa, aes(x = fit.Var1, y = fit.value, fill = fit.Var2))
gaf <- gaf + geom_line(aes(colour = fit.Var2), size = 1)
gaf <- gaf + geom_ribbon(aes(ymin = fit.value - se, ymax = fit.value + se),
                         alpha = 0.3)
gaf <- gaf + coord_flip()
gaf <- gaf + labs(y = 'Time', x = 'P(T > t)')
gaf <- gaf + scale_x_continuous(trans = log10_trans())
gaf <- gaf + scale_color_manual(values = cbp[-1],
                                name = 'Quantile\nP(Carbonate)')
gaf <- gaf + scale_fill_manual(values = cbp[-1],
                               name = 'Quantile\nP(Carbonate)')
gaf <- gaf + theme(axis.title.y = element_text(angle = 0),
                   axis.text = element_text(size = 20),
                   axis.title = element_text(size = 23),
                   legend.text = element_text(size = 17),
                   legend.title = element_text(size = 19),
                   strip.text = element_text(size = 20))
ggsave(filename = '../doc/figure/para_aff.png', plot = gaf,
       width = 15, height = 10)

# paleoenvironment
ha <- predict(swei[[3]], newdata = data.frame(hab = c('offshore', 
                                                      'inshore',
                                                      'none')),

              type = 'quantile',
              p = seq(0.01, 0.99, by = 0.01), se.fit = TRUE)
rownames(ha$fit) <- rownames(ha$se.fit) <- c('offshore', 'inshore', 'none')
ha <- lapply(ha, t)
ha <- lapply(ha, melt)
ha <- cbind(fit = ha$fit, se = ha$se.fit$value)
ha[, 1] <- (100 - ha[, 1]) / 100

gah <- ggplot(ha, aes(x = fit.Var1, y = fit.value, fill = fit.Var2))
gah <- gah + geom_line(aes(colour = fit.Var2), size = 1)
gah <- gah + geom_ribbon(aes(ymin = fit.value - se, ymax = fit.value + se),
                         alpha = 0.3)
gah <- gah + coord_flip()
gah <- gah + labs(y = 'Time', x = 'P(T > t)')
gah <- gah + scale_x_continuous(trans = log10_trans())
gah <- gah + scale_color_manual(values = cbp[-1],
                                name = 'Paleoenvironment')
gah <- gah + scale_fill_manual(values = cbp[-1],
                               name = 'Paleoenvironment')
gah <- gah + theme(axis.title.y = element_text(angle = 0),
                   axis.text = element_text(size = 20),
                   axis.title = element_text(size = 23),
                   legend.text = element_text(size = 17),
                   legend.title = element_text(size = 19),
                   strip.text = element_text(size = 20))
ggsave(filename = '../doc/figure/para_hab.png', plot = gah,
       width = 15, height = 10)

# occupancy
occu <- data.frame(occu = c(min(persist$occu),
                            quantile(persist$occu, .25),
                            quantile(persist$occu, .5),
                            quantile(persist$occu, .75),
                            max(persist$occu)))
oc <- predict(swei[[5]], newdata = occu, type = 'quantile', 
              p = seq(0.01, 0.99, by = 0.01), se.fit = TRUE)
rownames(oc$fit) <- rownames(oc$se.fit) <- c('Min', 'Q1', 'Median', 'Q3', 'Max')
oc <- lapply(oc, t)
oc <- lapply(oc, melt)
oc <- cbind(fit = oc$fit, se = oc$se.fit$value)
oc[, 1] <- (100 - oc[, 1]) / 100

goc <- ggplot(oc, aes(x = fit.Var1, y = fit.value, fill = fit.Var2))
goc <- goc + geom_line(aes(colour = fit.Var2), size = 1)
goc <- goc + geom_ribbon(aes(ymin = fit.value - se, ymax = fit.value + se),
                         alpha = 0.3)
goc <- goc + coord_flip()
goc <- goc + labs(y = 'Time', x = 'P(T > t)')
goc <- goc + scale_x_continuous(trans = log10_trans())
goc <- goc + scale_color_manual(values = cbp[-1],
                                name = 'Mean BC\nOccupancy')
goc <- goc + scale_fill_manual(values = cbp[-1],
                               name = 'Mean BC\nOccupancy')
goc <- goc + theme(axis.title.y = element_text(angle = 0),
                   axis.text = element_text(size = 20),
                   axis.title = element_text(size = 23),
                   legend.text = element_text(size = 17),
                   legend.title = element_text(size = 19),
                   strip.text = element_text(size = 20))
ggsave(filename = '../doc/figure/para_occ.png', plot = goc,
       width = 15, height = 10)

# size
siz <- data.frame(size = c(min(persist$size),
                           quantile(persist$size, .25),
                           quantile(persist$size, .5),
                           quantile(persist$size, .75),
                           max(persist$size)))
sz <- predict(swei[[4]], newdata = siz, type = 'quantile', 
              p = seq(0.01, 0.99, by = 0.01), se.fit = TRUE)
rownames(sz$fit) <- rownames(sz$se.fit) <- c('Min', 'Q1', 'Median', 'Q3', 'Max')
sz <- lapply(sz, t)
sz <- lapply(sz, melt)
sz <- cbind(fit = sz$fit, se = sz$se.fit$value)
sz[, 1] <- (100 - sz[, 1]) / 100

gsz <- ggplot(sz, aes(x = fit.Var1, y = fit.value, fill = fit.Var2))
gsz <- gsz + geom_line(aes(colour = fit.Var2), size = 1)
gsz <- gsz + geom_ribbon(aes(ymin = fit.value - se, ymax = fit.value + se),
                         alpha = 0.3)
gsz <- gsz + coord_flip()
gsz <- gsz + labs(y = 'Time', x = 'P(T > t)')
gsz <- gsz + scale_x_continuous(trans = log10_trans())
gsz <- gsz + scale_color_manual(values = cbp[-1],
                                name = 'Log(Size)\nQuartile')
gsz <- gsz + scale_fill_manual(values = cbp[-1],
                               name = 'Log(Size)\nQuartile')
gsz <- gsz + theme(axis.title.y = element_text(angle = 0),
                   axis.text = element_text(size = 20),
                   axis.title = element_text(size = 23),
                   legend.text = element_text(size = 17),
                   legend.title = element_text(size = 19),
                   strip.text = element_text(size = 20))
ggsave(filename = '../doc/figure/para_szc.png', plot = gsz,
       width = 15, height = 10)
