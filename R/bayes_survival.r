library(rstan)
library(boot)
library(parallel)

source('../R/networks.r')

RNGkind(kind = "L'Ecuyer-CMRG")
seed <- 420

# compile models
weim <- stan(file = '../stan/weibull_survival.stan')
sepwei <- stan(file = '../stan/sep_weibull_survival.stan')
heirwei <- stan(file = '../stan/heir_weibull_survival.stan')
#mixwei <- stan(file = '../stan/mixture_survival.stan')

# observed
keep <- names(affinity) %in% occ.val$taxa
ord <- orders[keep]

# assemble data
duration <- dur[keep]
extinct <- 1 - censored[keep]
size <- size[keep]
aff <- affinity[keep]
hab <- inshore[keep]
nsp <- ratio[keep]
occ <- occ.val$mean

names(ord) <- names(nsp)
splitsort <- split(ord, ord)
wss <- lapply(splitsort, names)
mem <- c()
for(i in seq(length(duration))) {
  hooray <- which(laply(wss, function(x) any(x %in% names(ord[i]))))
  mem[i] <- hooray
}


data <- list(duration = duration,
             ord = ord,
             siz = log(size),
             aff = logit(aff),
             occ = occ,
             hab = logit(hab),
             extinct = extinct,
             nsp = nsp,
             mem = mem)
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
             nsp_unc = unc$nsp,
             mem_unc = unc$mem,
             N_unc = length(unc$duration),
             dur_cen = cen$duration,
             size_cen = cen$siz,
             aff_cen = cen$aff,
             hab_cen = cen$hab,
             occ_cen = cen$occ,
             nsp_cen = cen$nsp,
             mem_cen = cen$mem,
             N_cen = length(cen$duration))

data$N <- data$N_unc + data$N_cen
data$samp_unc <- seq(data$N_unc)
data$samp_cen <- seq(from = data$N_unc + 1, 
                     to = data$N_unc + data$N_cen, 
                     by = 1)

data$G <- length(unique(ord))
data$K <- 2

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

## seperate model on shape
seplist <- mclapply(1:4, mc.cores = detectCores(),
                    function(x) stan(fit = sepwei, seed = seed,
                                     data = data,
                                     iter = 5000,
                                     chains = 1, chain_id = x,
                                     refresh = -1))
sfit <- sflist2stanfit(seplist)

# heirarchical model on shape
heirlist <- mclapply(1:4, mc.cores = detectCores(),
                     function(x) stan(fit = heirwei, seed = seed,
                                      data = data,
                                      chains = 1, chain_id = x,
                                      refresh = -1))
hfit <- sflist2stanfit(heirlist)


# mixed
# heirarchical model on shape
#mixlist <- mclapply(1:4, mc.cores = detectCores(),
#                    function(x) stan(fit = mixwei, seed = seed,
#                                     data = data,
#                                     iter = 10000,
#                                     chains = 1, chain_id = x,
#                                     refresh = -1))
#mixfit <- sflist2stanfit(mixlist)
