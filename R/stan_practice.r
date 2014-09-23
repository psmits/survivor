library(rstan)
source('sim_bd.r')

sim <- simbd(1, 100, 0.5, 0.5)
dur <- sim$dur

#plot(density(rgamma(100, 1, 0.00000000001)))


data <- list(N = length(dur),
             dur = dur)

mod <- stan(file = '../stan/practice.stan')
fit1 <- stan(fit = mod, data = data, iter = 1000, chains = 4)
