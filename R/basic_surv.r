library(survival)
library(MuMIn)

source('../R/mung.r')

surv <- Surv(time = persist$dur, event = persist$ext)
mod.wei <- survreg(formula = surv ~ aff, data = persist, dist = 'weibull')
mod.exp <- survreg(formula = surv ~ aff, data = persist, dist = 'exponential')

affcurve <- predict(model, newdata = data.frame(aff = c('carbonate', 
                                                        'clastic', 
                                                        'mixed')), 
                    type = 'quantile', p = seq(0.01, 0.99, by = 0.01))
rownames(affcurve) <- c('carbonate', 'clastic', 'mixed')
affcurve <- t(affcurve)
affcurve <- melt(affcurve)

#require(ggplot2)
#ggplot(affcurve, aes(x = value, y = -Var1, colour = Var2)) + geom_line()


