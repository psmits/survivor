library(survival)
library(MuMIn)
library(xtable)

source('../R/make_table.r')
source('../R/model_sel.r')

source('../R/para_surv.r')

mod.tab <- surv.tab(models, 'tab:mod')
print.xtable(mod.tab, file = '../doc/mod_sel.tex',
             hline.after = 0,
             include.rownames = FALSE)

imp <- var.imp(models)
med <- ddply(Reduce(rbind, per.mod), .(pred), summarize,
             baseline = median(imp))
