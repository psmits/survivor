library(survival)

source('../R/mung.r')

surv <- Surv(persist$st, persist$age, persist$ext)
surv.est <- survfit(surv ~ 1)

cox.est <- coxph(surv ~ 1)
sur.est <- survfit(cox.est)

cox.aff <- coxph(surv ~ aff, data = persist)
sur.aff <- survfit(cox.aff)
