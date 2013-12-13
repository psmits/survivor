library(reshape2)
library(plyr)

source('../R/clean_funcs.r')
source('../R/occurrence.r')
source('../R/affinity.r')

info <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)
dur <- read.csv('../data/psmits-ranges.csv', stringsAsFactors = FALSE)

ptbound <- 252.28

# remove missing lithology information
info <- info[info$lithology1 != '', ]
info$lithology1 <- gsub(pattern = '[\\"?]',
                        replacement = '',
                        info$lithology1, 
                        perl = TRUE)
# remove missing environmental information
info <- info[info$environment != '', ]
info$environment <- as.character(info$environment)

info$lithology1 <- clean.lith(info$lithology1)
info$environment <- clean.env(info$environment)

rmlith <- c('lithified', 'not reported')
info <- info[-(which(info$lithology1 %in% rmlith)), ]
info <- info[info$lithology1 != 'mixed', ]

paff <- get.occ(dur[, 2], dur[, 3], info$ma_mid, info$lithology1)
names(paff) <- dur[, 1]

info <- info[with(info, order(occurrence.genus_name)), ]
pocc <- split(info, info$occurrence.genus_name)

litaf <- list()
for(ii in seq(length(pocc))) {
  litaf[[ii]] <- shprob(occur = pocc[[ii]]$lithology1,
                        avil = paff[[ii]])
}
names(litaf) <- names(pocc)
litprob <- unlist(litaf)

# assign text affinity
litaf[litaf >= (2/3)] <- 'carbonate'
litaf[litaf <= (1/3)] <- 'clastic'
litaf[litaf > (1/3) & litaf < (2/3)] <- 'mixed'

genenv <- split(info, info$occurrence.genus_name)
env <- lapply(genenv, function(x) {
              sub.aff(x$environment, middle = 'none', level = 0.6)})


sf <- as.character(dur$genus) %in% names(litaf) & 
      as.character(dur$genus) %in% names(env)
dur <- dur[sf, ]


rms <- which(dur[, 2] < ptbound & dur[, 3] < ptbound)
dur <- dur[-rms, ]
litaf <- litaf[-rms]
env <- env[-rms]

# make the data frame for survival analysis
# need to allow for originations
zero <- max(dur[, 2])
rel.or <- abs(dur[, 2] - zero)
rel.end <- abs(dur[, 3] - zero)
persist <- cbind(st = as.data.frame(rel.or), 
                 age = rel.end, 
                 ext = rep(1, length(rel.or)),
                 aff = unlist(litaf),
                 env = unlist(env))
names(persist)[1] <- 'st'

# fix the censored ones
persist$ext[dur[, 3] < ptbound] <- 0
reps <- dur[dur[, 3] < ptbound, 2] - ptbound
persist$age[dur[, 3] < ptbound] <- reps

persist$dur <- abs(persist$st - persist$age)
