library(rstan)

source('../R/mung.r')

# assemble data
duration <- surv[, 1]
extinct <- surv[, 3]  # 1 yes, 0 no
size <- uni[, 2]

taxa.carb <- unlist(lapply(tocc, function(x) {
                           if(is.na(x['carbonate'])) 0 else x['carbonate']}))
taxa.occ <- unlist(lapply(tocc, sum))

total.carb <- unlist(lapply(kocc, function(x){
                            if(is.na(x['carbonate'])) 0 else x['carbonate']}))
total.occ <- unlist(lapply(kocc, sum))


# compile and fit model
