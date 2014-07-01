#' Format a model selection table for a list of survreg objects
#'
#' @param mods list; objects of class survreg
#' @return object of class xtable
surv.tab <- function(mods, label) {

  res <- lapply(mods, get.vals)
  res <- Reduce(rbind, res)
  rownames(res) <- NULL
  res <- apply(res, 2, unlist)
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  res <- res[order(as.numeric(res$AICc)), ]
  res$shape[res$distribution == 'exponential'] <- NA
  rownames(res) <- NULL
  num <- c('shape', 'df', 'AICc')
  res[, num] <- apply(res[, num], 2, as.numeric)
  res$weight <- aic.wts(res$AICc)

  tab <- xtable(res)
  short <- which(names(tab) %in% c('df', 'AICc', 'weight')) + 1
  digits(tab)[short] <- c(0, 4, 2)
  label(tab) <- label

  tab
}

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
  out
}

