library(reshape2)
library(plyr)
library(survival)

source('../R/clean_funcs.r')
source('../R/occurrence.r')
source('../R/affinity.r')
source('../R/paleo_surv.r')

info <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)
dur <- read.csv('../data/psmits-ranges.csv', stringsAsFactors = FALSE)

ptbound <- 252.2
pst <- 298.9

# remove taxa that went extinct before the permian
rg <- dur[which(dur[, 3] > pst), 1]
dur <- dur[!(dur[, 1] %in% rg), ]
info <- info[!(info$occurrence.genus_name %in% rg), ]

# remove taxa originating after the permian
rg <- dur[which(dur[, 2] < ptbound), 1]
dur <- dur[!(dur[, 1] %in% rg), ]
info <- info[!(info$occurrence.genus_name %in% rg), ]

# remove taxa that originated before the permian
#rg <- dur[which(dur[, 2] > pst), 1]
#dur <- dur[!(dur[, 1] %in% rg), ]
#info <- info[!(info$occurrence.genus_name %in% rg), ]

# remove missing lithology information
info <- info[info$lithology1 != '', ]
info$lithology1 <- gsub(pattern = '[\\"?]',
                        replacement = '',
                        info$lithology1, 
                        perl = TRUE)
# remove missing environmental information
info <- info[info$environment != '', ]
rmlith <- c('lithified', 'not reported')
info <- info[!(info$lithology1 %in% rmlith), ]
info <- info[info$lithology1 != 'mixed', ]

# lithology
info$lithology1 <- clean.lith(info$lithology1)
info$environment <- clean.env(info$environment)

info <- info[with(info, order(occurrence.genus_name)), ]
pocc <- split(info, info$occurrence.genus_name)
dur <- dur[dur[, 1] %in% names(pocc), ]

paff <- get.occ(dur[, 2], dur[, 3], info$ma_mid, info$lithology1)
names(paff) <- dur[, 1]

litaf <- list()
for(ii in seq(length(pocc))) {
  litaf[[ii]] <- shprob(occur = pocc[[ii]]$lithology1, avil = paff[[ii]])
}
names(litaf) <- names(pocc)
litprob <- unlist(litaf)

litaf[litaf >= (2/3)] <- 'carbonate'
litaf[litaf <= (1/3)] <- 'clastic'
litaf[litaf > (1/3) & litaf < (2/3)] <- 'mixed'

# environment
penv <- get.occ(dur[, 2], dur[, 3], info$ma_mid, info$environment)
names(penv) <- dur[, 1]
hab <- list()
for(ii in seq(length(penv))) {
  ins <- shprob(occur = pocc[[ii]]$environment, avil = penv[[ii]], 
                ph1 = 1/3, ph2 = 2/3, aff = 'inshore')
  off <- shprob(occur = pocc[[ii]]$environment, avil = penv[[ii]], 
                ph1 = 1/3, ph2 = 2/3, aff = 'offshore')
  non <- shprob(occur = pocc[[ii]]$environment, avil = penv[[ii]], 
                ph1 = 1/3, ph2 = 2/3, aff = 'none')
  hab[[ii]] <- c(ins, off, non)
}
names(hab) <- names(penv)
hab <- lapply(lapply(hab, which.max), 
              function(x) c('inshore', 'offshore', 'none')[x])


sf <- as.character(dur$genus) %in% names(litaf) & 
as.character(dur$genus) %in% names(hab)
dur <- dur[sf, ]

surv <- paleosurv(dur[, 2], dur[, 3], start = pst, end = ptbound + 5)

# make the data frame for survival analysis
persist <- as.data.frame(cbind(aff = unlist(litaf),
                               hab = unlist(hab)))
