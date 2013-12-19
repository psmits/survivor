library(survival)
library(MuMIn)

source('../R/mung.r')

surv <- Surv(time = persist$dur, event = persist$ext)

# nonparametric K-M curves
kmaff <- survfit(formula = surv ~ aff, data = persist)
kmenv <- survfit(formula = surv ~ env, data = persist)


# parametric models
# weibull dist
moda.wei <- survreg(formula = surv ~ aff, data = persist, dist = 'weibull')
modae.wei <- survreg(formula = surv ~ aff + env, data = persist, dist = 'weibull')
modaec.wei <- survreg(formula = surv ~ aff * env, data = persist, dist = 'weibull')
modaei.wei <- survreg(formula = surv ~ aff : env, data = persist, dist = 'weibull')
mode.wei <- survreg(formula = surv ~ env, data = persist, dist = 'weibull')
modi.wei <- survreg(formula = surv ~ 1, data = persist, dist = 'weibull')

# exp dist
moda.exp <- survreg(formula = surv ~ aff, data = persist, dist = 'exponential')
modae.exp <- survreg(formula = surv ~ aff + env, data = persist, dist = 'exponential')
modaec.exp <- survreg(formula = surv ~ aff * env, data = persist, dist = 'exponential')
modaei.exp <- survreg(formula = surv ~ aff : env, data = persist, dist = 'exponential')
mode.exp <- survreg(formula = surv ~ env, data = persist, dist = 'exponential')
modi.exp <- survreg(formula = surv ~ 1, data = persist, dist = 'exponential')
