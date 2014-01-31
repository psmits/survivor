#' Simulate pure birth process
#'
#' @param n scalar; initial number of observations
#' @param max scalar; number of observation for when to stop
#' @param lambda scalar; birth rate
simbirth <- function(n, max, lambda) {
  t <- 0
  bb <- data.frame(t = t, n = n)

  while(n < max) {
    dt <- rexp(1, rate = lambda * n)
    t <- t + dt
    n <- n + 1
    bb <- rbind(bb, c(t, n))
  }

  bb
}

