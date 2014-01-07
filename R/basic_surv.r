library(survival)

source('../R/mung.r')

# 0 right, 1 event, 2 left, 3 interval
surv <- Surv(time = persist$dur, event = persist$ext)

# nonparametric K-M curves
kmaff <- survfit(formula = surv ~ aff, data = persist)
kmhab <- survfit(formula = surv ~ hab, data = persist)


# parametric models
# weibull dist
moda.wei <- survreg(formula = surv ~ aff, data = persist, dist = 'weibull')
modae.wei <- survreg(formula = surv ~ aff + hab, data = persist, dist = 'weibull')
modaec.wei <- survreg(formula = surv ~ aff * hab, data = persist, dist = 'weibull')
modaei.wei <- survreg(formula = surv ~ aff : hab, data = persist, dist = 'weibull')
mode.wei <- survreg(formula = surv ~ hab, data = persist, dist = 'weibull')
modi.wei <- survreg(formula = surv ~ 1, data = persist, dist = 'weibull')

# exp dist
moda.exp <- survreg(formula = surv ~ aff, data = persist, dist = 'exponential')
modae.exp <- survreg(formula = surv ~ aff + hab, data = persist, dist = 'exponential')
modaec.exp <- survreg(formula = surv ~ aff * hab, data = persist, dist = 'exponential')
modaei.exp <- survreg(formula = surv ~ aff : hab, data = persist, dist = 'exponential')
mode.exp <- survreg(formula = surv ~ hab, data = persist, dist = 'exponential')
modi.exp <- survreg(formula = surv ~ 1, data = persist, dist = 'exponential')

# try lognormal...


# list of all models
models <- list(moda.wei, modae.wei, modaec.wei, modaei.wei, mode.wei, modi.wei,
               moda.exp, modae.exp, modaec.exp, modaei.exp, mode.exp, modi.exp)
