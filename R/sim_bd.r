#' Simulate birth-death process
#'
#' @param n scalar; starting number of observations
#' @param max scalar; stopping number of observations
#' @param lambda scalar; per-capita birth rate
#' @param mu scalar; per-capita death rate
#' 
#' @return data frame with 4 columns corresponding to taxon number, start time, end time, and duration. NAs are returned for right censored taxa.
simbd <- function(n, max, lambda, mu) {
  t <- 0
  bd <- data.frame(n = seq(n), s = rep(t, n), e = rep(NA, n))

  while(n < max & n > 0) {
    dt <- rexp(1, rate = (mu * n + lambda * n))
    t <- t + dt

    event <- sample(c(-1, 1), 1, prob = c(mu * n, lambda * n))
    n <- n + event

    # extinction or origination?
    if(event == 1) {
      bd <- rbind(bd, c(nrow(bd) + 1, t, NA))
    } else if(event == -1) {
      alive <- which(is.na(bd[, 3]))
      dead <- sample(alive, 1)
      bd[dead, 3] <- t
    }

    if(n == 0) {
      bd[is.na(bd[, 3]), 3] <- t
    }
  }

  bd$dur <- abs(bd$s - bd$e)

  if(n != 0) {
    study <- rexp(1, rate = (mu * n + lambda * n))
    study <- t + study
    alive <- which(is.na(bd[, 3]))
    bd[alive, 3] <- study
  }

  bd
}
