library(survival)

source('../R/mung.r')

surv <- Surv(persist$st, persist$age, persist$ext)
surv.est <- survfit(surv ~ 1)

cox.est <- coxph(surv ~ 1)
cox.aff <- coxph(surv ~ aff, data = persist)
