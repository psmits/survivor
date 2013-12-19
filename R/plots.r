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

ee <- c(rep('inshore', kmenv$strata[1]),
        rep('none', kmenv$strata[2]),
        rep('offshore', kmenv$strata[3]))
kmecurve <- cbind(data.frame(time = kmenv$time), surv = kmenv$surv, env = ee)
ggkme <- ggplot(kmecurve, aes(x = time, y = surv, colour = env)) 
ggkme <- ggkme + geom_step()
ggsave(filename = '../doc/figure/km_env.png', plot = ggkme)


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

ggaff <- ggplot(affcurve, aes(x = Var1, y = value, colour = Var2)) 
ggaff <- ggaff + geom_line()
ggaff <- ggaff + geom_ribbon(aes(ymin = value - se.fit, ymax = value + se.fit,
                                 fill = Var2), alpha = 0.3, colour = NA)
ggaff <- ggaff + labs(x = 'S(t)', y = 'time')
ggaff <- ggaff + geom_step(data = kmacurve, 
                           mapping = aes(x = surv, y = time, colour = aff))
ggaff <- ggaff + coord_flip()
ggaff <- ggaff + scale_fill_manual(values = cbp,
                                   name = 'substrate preference')
ggaff <- ggaff + scale_colour_manual(values = cbp, 
                                     name = 'substrate preference')
ggsave(filename = '../doc/figure/aff.png', plot = ggaff)

envcurve <- predict(mode.wei, newdata = data.frame(env = c('inshore',
                                                           'none',
                                                           'offshore')),
                    type = 'quantile',
                    p = seq(0.0, 0.99, by = 0.01),
                    se.fit = TRUE)
rownames(envcurve$fit) <- c('inshore', 'none', 'offshore')
rownames(envcurve$se.fit) <- c('inshore', 'none', 'offshore')
envcurve <- lapply(envcurve, t)
envcurve <- lapply(envcurve, melt)
envcurve <- cbind(envcurve$fit, se.fit = envcurve$se.fit$value)
envcurve[, 1] <- (100 - envcurve[, 1]) / 100

ggenv <- ggplot(envcurve, aes(x = Var1, y = value, colour = Var2)) 
ggenv <- ggenv + geom_line()
ggenv <- ggenv + geom_ribbon(aes(ymin = value - se.fit, ymax = value + se.fit,
                                 fill = Var2), alpha = 0.3, colour = NA)
ggenv <- ggenv + labs(x = 'S(t)', y = 'time')
ggenv <- ggenv + geom_step(data = kmecurve, 
                           mapping = aes(x = surv, y = time, colour = env))
ggenv <- ggenv + coord_flip()
ggenv <- ggenv + scale_fill_manual(values = cbp,
                                   name = 'habitat preference')
ggenv <- ggenv + scale_colour_manual(values = cbp, 
                                     name = 'habitat preference')
ggsave(filename = '../doc/figure/env.png', plot = ggenv)
