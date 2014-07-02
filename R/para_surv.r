library(survival)
library(MuMIn)
source('../R/model_sel.r')

source('../R/networks.r')

vars <- names(persist[, -(ncol(persist))])
mods <- create.model(vars = vars)
persist$size <- log(as.numeric(as.character(persist$size)))

surv.wei <- survreg(surv ~ 1, data = persist, dist = 'weibull')
swei <- fit.models(surv.wei, mods, data = persist, dist = 'weibull')

surv.exp <- survreg(surv ~ 1, data = persist, dist = 'exponential')
sexp <- fit.models(surv.exp, mods, data = persist, dist = 'exponential')

models <- c(swei, sexp)
