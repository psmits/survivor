library(survival)
library(MuMIn)
source('../R/model_sel.r')

source('../R/networks.r')

nsim <- 1000

#persist$cvo[is.na(persist$cvo)] <- 0

vars <- names(persist[, -(ncol(persist))])
mods <- create.model(vars = vars)
persist$size <- log(as.numeric(as.character(persist$size)))

surv.wei <- survreg(surv ~ 1, data = persist, dist = 'weibull')
swei <- fit.models(surv.wei, mods, data = persist, dist = 'weibull')

per.wei <- list()
for(ii in seq(nsim)) {
  per.wei[[ii]] <- var.imp(model.base(surv, mods = mods, 
                                      datas = persist, 
                                      distribution = 'weibull'))
}

surv.exp <- survreg(surv ~ 1, data = persist, dist = 'exponential')
sexp <- fit.models(surv.exp, mods, data = persist, dist = 'exponential')

per.exp <- list()
for(ii in seq(nsim)) {
  per.exp[[ii]] <- var.imp(model.base(surv, mods = mods, 
                                      datas = persist, 
                                      distribution = 'exponential'))
}

models <- c(swei, sexp)
per.mod <- c(per.wei, per.exp)
