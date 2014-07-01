library(survival)
library(MuMIn)

create.model <- function(vars) {
  mod <- list()
  for(ii in seq(length(vars))) {
    cc <- combn(vars, ii, simplify = FALSE)
    mod[[ii]] <- cc #lapply(cc, function(x) paste(x, collapse = ' + '))
  }
  mod <- unlist(mod, recursive = FALSE)
  mod
}

fit.models <- function(initial, models, data, dist) {
  ups <- list()
  ups[[1]] <- initial
  for(ii in seq(from = 2, to = (length(models) + 1))) {
    ups[[ii]] <- update(initial, paste('. ~ . +', 
                                       paste(models[[ii - 1]], 
                                             collapse = ' + ')), 
                        data = data, dist = dist)
  }
  ups
}

model.base <- function(surv, mods, datas, distribution) {
  tm <- unclass(surv)
  ord <- sample(nrow(tm))
  ptm <- tm[ord, ]
  psurv <- Surv(time = ptm[, 1], event = ptm[, 2])

  pmod <- survreg(psurv ~ 1, data = datas, dist = distribution)
  pmods <- fit.models(pmod, mods, data = datas, dist = distribution)
  pmods
}

var.imp <- function(models) {
  preds <- lapply(models, function(x) as.character(x$call$formula[-2])[-1])
  preds <- lapply(preds, function(x) unlist(str_split(x, ' \\+ ')))
  uni.pred <- unique(unlist(preds))

  wts <- Weights(laply(models, AICc))

  rel <- c()
  for(ii in seq(length(uni.pred))) {
    wh <- lapply(preds, function(x) any(x %in% uni.pred[ii]))
    rel[ii] <- sum(wts[unlist(wh)])
  }
  
  incepts <- grepl('[0-9]', uni.pred)

  uni.pred[incepts] <- 'no predictors'
  rel <- cbind(data.frame(imp = rel), pred = uni.pred[])
  rel
}

ext.shape <- function(model) {
  sc <- 1 / model$scale

  # standard errors
  vari <- model$var[nrow(model$var), ncol(model$var)] * model$scale ^ 2
  std <- sqrt(vari / model$scale ^ 4)
  se <- std# / sqrt(model$

  out <- list()
  out$shape <- sc
  out$se <- se
  out
}
