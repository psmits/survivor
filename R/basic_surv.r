library(survival)
library(MuMIn)

source('../R/mung.r')

surv <- Surv(time = persist$dur, event = persist$ext)
moda.wei <- survreg(formula = surv ~ aff, data = persist, dist = 'weibull')
modae.wei <- survreg(formula = surv ~ aff + env, data = persist, dist = 'weibull')
mode.wei <- survreg(formula = surv ~ env, data = persist, dist = 'weibull')
modi.wei <- survreg(formula = surv ~ 1, data = persist, dist = 'weibull')

moda.exp <- survreg(formula = surv ~ aff, data = persist, dist = 'exponential')
modae.exp <- survreg(formula = surv ~ aff + env, data = persist, dist = 'exponential')
mode.exp <- survreg(formula = surv ~ env, data = persist, dist = 'exponential')
modi.exp <- survreg(formula = surv ~ 1, data = persist, dist = 'exponential')

affcurve <- predict(mode.wei, newdata = data.frame(env = c('inshore', 
                                                            'none', 
                                                            'offshore')),
                    type = 'quantile', p = seq(0.01, 0.99, by = 0.01))
rownames(affcurve) <- c('inshore', 'none', 'offshore')
affcurve <- t(affcurve)
affcurve <- melt(affcurve)

require(ggplot2)
ggplot(affcurve, aes(x = value, y = -Var1, colour = Var2)) + geom_line()


