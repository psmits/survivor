library(xtable)
library(MuMIn)

source('../R/basic_surv.r')

aic.wts <- function(aic) {
  dels <- aic - min(aic)
  rel <- exp(-0.5 * dels)

  rel / sum(rel)
}


# get predictors, distribution, k, df, logLik, AICc
get.vals <- function(model) {
  pred <- paste(as.character(model$call$formula)[-2], collapse = ' ')
  d <- model$dist
  k <- 1 / model$scale
  df <- model$df
  ll <- logLik(model)
  aic <- AICc(model)

  out <- list(formula = pred, distribution = d, shape = k, 
              df = df, 
              #logLik = ll, 
              AICc = aic)
}


res <- lapply(models, get.vals)
res <- Reduce(rbind, res)
rownames(res) <- NULL
res <- apply(res, 2, unlist)
res <- as.data.frame(res, stringsAsFactors = FALSE)
res <- res[order(res$AICc), ]
res$shape[res$distribution == 'exponential'] <- NA
rownames(res) <- NULL
res[, 3:5] <- apply(res[, 3:5], 2, as.numeric)
res$weight <- aic.wts(res$AICc)

res.table <- xtable(res)
digits(res.table)[5:7] <- c(0, 4, 2)
label(res.table) <- 'tab:brach'

print.xtable(res.table, file = '../doc/model_tabs.tex',
             hline.after = 0,
             include.rownames = FALSE)
