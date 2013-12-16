library(survival)
library(ggplot2)
library(scales)
library(reshape2)

source('../R/basic_surv.r')

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')

affcurve <- predict(moda.wei, newdata = data.frame(aff = c('carbonate',
                                                            'clastic',
                                                            'mixed')),
                    type = 'quantile',
                    p = seq(0.01, 0.99, by = 0.01),
                    se.fit = TRUE)
rownames(affcurve$fit) <- c('carbonate', 'clastic', 'mixed')
rownames(affcurve$se.fit) <- c('carbonate', 'clastic', 'mixed')
affcurve <- lapply(affcurve, t)
affcurve <- lapply(affcurve, melt)
affcurve <- cbind(affcurve$fit, se.fit = affcurve$se.fit$value)

ggenv <- ggplot(affcurve, aes(x = -Var1, y = value, colour = Var2)) 
ggenv <- ggenv + geom_line()
ggenv <- ggenv + geom_ribbon(aes(ymin = value - se.fit, ymax = value + se.fit,
                                 fill = Var2), alpha = 0.3, colour = NA)
ggenv <- ggenv + coord_flip()
ggenv <- ggenv + scale_fill_manual(values = cbp,
                                   name = 'substrate preference')
ggenv <- ggenv + scale_colour_manual(values = cbp, 
                                     name = 'substrate preference')
