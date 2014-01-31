#' Simulate birth-death process
#'
#' @param n scalar; starting number of observations
#' @param max scalar; stopping number of observations
#' @param lambda scalar; per-capita birth rate
#' @param mu scalar; per-capita death rate
simbd <- function(n, max, lambda, mu) {
  t <- 0
  bd <- data.frame(t = t, n = n)

  while(n < max & n > 0) {
    dt <- rexp(1, rate = (mu * n + lambda * n))

    event <- sample(c(-1, 1), 1, prob = c(mu * n, lambda * n))

    t <- t + dt
    n <- n + event

    bd <- rbind(bd, c(t, n))
  }

  bd

  # now modify this to record the duration of each observation
}
