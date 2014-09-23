library(rstan)

source('../R/mung.r')

# assemble data
duration <- surv[, 1]
extinct <- surv[, 3]  # 1 yes, 0 no

size <- uni[, 2]

aff <- persist$aff

# this is for the really complicated idea i have about 
# having affinity as a distribution
taxa.carb <- unlist(lapply(tocc, function(x) {
                           if(is.na(x['carbonate'])) 0 else x['carbonate']}))
taxa.occ <- unlist(lapply(tocc, sum))
total.carb <- unlist(lapply(kocc, function(x){
                            if(is.na(x['carbonate'])) 0 else x['carbonate']}))
total.occ <- unlist(lapply(kocc, sum))



data <- list(duration = duration,
             siz = log(size),
             aff = aff)
good <- !is.na(duration)
data <- lapply(data, function(x) x[good])
data$N <- length(data$duration) 

# compile and fit model
fit <- stan(file = '../stan/survival_model.stan')

fit1 <- stan(fit = fit, data = data, iter = 1000, chains = 4)

#plot(density(rweibull(1000, 1.77, 21.6)))
