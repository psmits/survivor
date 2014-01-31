#' Simulate pure death process
#'
#' @param n scalar; starting number of observations
#' @param mu scalar; per-capita death rate
simdeath <- function(n, mu) {
  t <- 0
  dd <- data.frame(t = t, n = n)

  while(n > 0) {
    dt <- rexp(1, rate = mu * n)
    t <- t + dt
    n <- n - 1
    dd <- rbind(dd, c(t, n))
  }

  dd
}
