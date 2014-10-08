library(rstan)
library(boot)
library(parallel)

source('../R/networks.r')

RNGkind(kind = "L'Ecuyer-CMRG")
seed <- 420

# compile models
weim <- stan(file = '../stan/weibull_survival.stan')
seim <- stan(file = '../stan/shift_exp_surv.stan')
mwim <- stan(file = '../stan/mixture_survival.stan')

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

small.data <- list(dur_unc = unc$duration,
                   N_unc = length(unc$duration),
                   dur_cen = cen$duration,
                   N_cen = length(cen$duration),
                   K = 2)

# fit models with parallel magic
# weibull
weilist <- mclapply(1:4, mc.cores = detectCores(),
                    function(x) stan(fit = weim, seed = seed,
                                     data = data,
                                     chains = 1, chain_id = x,
                                     refresh = -1))

# list of results
wfit <- sflist2stanfit(weilist)

# shifted exp 
explist <- mclapply(1:4, mc.cores = detectCores(),
                    function(x) stan(fit = seim, seed = seed,
                                     data = data,
                                     chains = 1, chain_id = x,
                                     refresh = -1))
efit <- sflist2stanfit(explist)

## mixture model
#mixlist <- mclapply(1:4, mc.cores = detectCores(),
#                    function(x) stan(fit = mwim, seed = seed,
#                                     data = small.data,
#                                     iter = 50000,
#                                     chains = 1, chain_id = x,
#                                     refresh = -1))
#mfit <- sflist2stanfit(mixlist)
