library(rstan)
library(boot)
library(parallel)

source('../R/networks.r')

RNGkind(kind = "L'Ecuyer-CMRG")
seed <- 420


# having affinity as a distribution
#taxa.carb <- unlist(lapply(tocc, function(x) {
#                           if(is.na(x['carbonate'])) 0 else x['carbonate']}))
#taxa.occ <- unlist(lapply(tocc, sum))
#total.carb <- unlist(lapply(kocc, function(x){
#                            if(is.na(x['carbonate'])) 0 else x['carbonate']}))
#total.occ <- unlist(lapply(kocc, sum))
# observed
keep <- names(affinity) %in% occ.val$taxa

# assemble data
duration <- dur[keep]
extinct <- 1 - censored[keep]

size <- size[keep]

aff <- affinity[keep]
hab <- inshore[keep]

occ <- occ.val$mean

data <- list(duration = duration,
             siz = log(size),
             aff = logit(aff),
             occ = occ,
             hab = logit(hab),
             extinct = extinct)
good <- !is.na(duration)
data <- lapply(data, function(x) x[good])

# uncensored vs censored
grab <- data$extinct == 1
unc <- lapply(data, function(x) x[grab])
cen <- lapply(data, function(x) x[!grab])

data <- list(dur_unc = unc$duration,
             size_unc = unc$siz,
             aff_unc = unc$aff,
             hab_unc = unc$hab,
             occ_unc = unc$occ,
             N_unc = length(unc$duration),
             dur_cen = cen$duration,
             size_cen = cen$siz,
             aff_cen = cen$aff,
             hab_cen = cen$hab,
             occ_cen = cen$occ,
             N_cen = length(cen$duration))


# compile and fit model
fit <- stan(file = '../stan/survival_model.stan')

# parallel magic
fitlist <- mclapply(1:4, mc.cores = detectCores(),
                    function(x) stan(fit = fit, seed = seed,
                                     data = data,
                                     chains = 1, chain_id = x,
                                     refresh = -1))
fit1 <- sflist2stanfit(fitlist)
