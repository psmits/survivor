library(survival)
library(ggplot2)
library(scales)
library(reshape2)

source('../R/basic_surv.r')

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')

# nonparametric curves
nn <- c(rep('carbonate', kmaff$strata[1]),
        rep('clastic', kmaff$strata[2]),
        rep('mixed', kmaff$strata[3]))
kmacurve <- cbind(data.frame(time = kmaff$time), surv = kmaff$surv, aff = nn)
ggkma <- ggplot(kmacurve, aes(x = time, y = surv, colour = aff)) 
ggkma <- ggkma + geom_step()
ggsave(filename = '../doc/figure/km_aff.png', plot = ggkma)

ee <- c(rep('inshore', kmhab$strata[1]),
        rep('none', kmhab$strata[2]),
        rep('offshore', kmhab$strata[3]))
kmecurve <- cbind(data.frame(time = kmhab$time), surv = kmhab$surv, hab = ee)
ggkme <- ggplot(kmecurve, aes(x = time, y = surv, colour = hab)) 
ggkme <- ggkme + geom_step()
ggsave(filename = '../doc/figure/km_hab.png', plot = ggkme)


# parametric curves
affcurve <- predict(moda.wei, newdata = data.frame(aff = c('carbonate',
                                                           'clastic',
                                                           'mixed')),
                    type = 'quantile',
                    p = seq(0.0, 0.99, by = 0.01),
                    se.fit = TRUE)
rownames(affcurve$fit) <- c('carbonate', 'clastic', 'mixed')
rownames(affcurve$se.fit) <- c('carbonate', 'clastic', 'mixed')
affcurve <- lapply(affcurve, t)
affcurve <- lapply(affcurve, melt)
affcurve <- cbind(affcurve$fit, se.fit = affcurve$se.fit$value)
affcurve[, 1] <- (100 - affcurve[, 1]) / 100

ggaff <- ggplot(affcurve, aes(x = value, y = Var1, colour = Var2)) 
ggaff <- ggaff + geom_line()
#ggaff <- ggaff + geom_line(aes(x = value - se.fit, y = Var1, colour = Var2), 
#                           linetype = 2, alpha = 0.3)
#ggaff <- ggaff + geom_line(aes(x = value + se.fit, y = Var1, colour = Var2), 
#                           linetype = 2, alpha = 0.3)
#ggaff <- ggaff + geom_ribbon(aes(ymin = value - se.fit, ymax = value + se.fit,
#                                 fill = Var2), alpha = 0.3, colour = NA)
ggaff <- ggaff + geom_step(data = kmacurve, 
                           mapping = aes(x = time, y = surv, colour = aff))
ggaff <- ggaff + labs(x = 'time', y = 'S(t)')
ggaff <- ggaff + scale_fill_manual(values = cbp,
                                   name = 'substrate preference')
ggaff <- ggaff + scale_colour_manual(values = cbp, 
                                     name = 'substrate preference')
ggaff <- ggaff + theme(axis.title.y = element_text(angle = 0))
#ggaff <- ggaff + coord_flip()
ggsave(filename = '../doc/figure/aff.png', plot = ggaff)

habcurve <- predict(mode.wei, newdata = data.frame(hab = c('inshore',
                                                           'none',
                                                           'offshore')),
                    type = 'quantile',
                    p = seq(0.0, 0.99, by = 0.01),
                    se.fit = TRUE)
rownames(habcurve$fit) <- c('inshore', 'none', 'offshore')
rownames(habcurve$se.fit) <- c('inshore', 'none', 'offshore')
habcurve <- lapply(habcurve, t)
habcurve <- lapply(habcurve, melt)
habcurve <- cbind(habcurve$fit, se.fit = habcurve$se.fit$value)
habcurve[, 1] <- (100 - habcurve[, 1]) / 100

gghab <- ggplot(habcurve, aes(x = value, y = Var1, colour = Var2)) 
gghab <- gghab + geom_line()
#gghab <- gghab + geom_line(aes(x = value - se.fit, y = Var1, colour = Var2), 
#                           linetype = 2, alpha = 0.3)
#gghab <- gghab + geom_line(aes(x = value + se.fit, y = Var1, colour = Var2), 
#                           linetype = 2, alpha = 0.3)
#gghab <- gghab + geom_ribbon(aes(ymin = value - se.fit, ymax = value + se.fit,
#                                 fill = Var2), alpha = 0.3, colour = NA)
gghab <- gghab + labs(x = 'time', y = 'S(t)')
gghab <- gghab + geom_step(data = kmecurve, 
                           mapping = aes(x = time, y = surv, colour = hab))
gghab <- gghab + scale_fill_manual(values = cbp,
                                   name = 'habitat preference')
gghab <- gghab + scale_colour_manual(values = cbp, 
                                     name = 'habitat preference')
gghab <- gghab + theme(axis.title.y = element_text(angle = 0))
#gghab <- gghab + coord_flip()
ggsave(filename = '../doc/figure/hab.png', plot = gghab)
