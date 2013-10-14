################################################################################
#
#
#
#
#
#
#
#
################################################################################

library(reshape2)
library(plyr)

source('../R/substrate_affinity.r')

info <- read.csv('../data/psmits-occs.csv')
dur <- read.csv('../data/psmits-ranges.csv')

ptbound <- 252.28

# remove missing lithology information
info <- info[info$lithology1 != '', ]
info$lithology1 <- gsub(pattern = '[\\"?]',
                        replacement = '',
                        info$lithology1, 
                        perl = TRUE)

carbonate <- c('limestone', 'carbonate', 'lime mudstone', 'mudstone', 
               'siltstone')
info$lithology1[info$lithology1 %in% carbonate] <- 'carbonate'

clastic <- c('siliciclastic')
info$lithology1[info$lithology1 %in% clastic] <- 'clastic'

mixed <- c('mixed carbonate-siliciclastic')
info$lithology1[info$lithology1 %in% mixed] <- 'mixed'


litaf <- ddply(info, .(occurrence.genus_name), summarise,
              affinity = sub.aff(lithology1))

litaf$occurrence.genus_name <- as.character(litaf$occurrence.genus_name)
names(litaf)[1] <- 'genus'

sf <- as.character(dur$genus) %in% litaf$genus 
dur <- dur[sf, ]

rms <- which(dur[, 2] < ptbound & dur[, 3] < ptbound)
dur <- dur[-rms, ]
litaf <- litaf[-rms, ]

# make the data frame for survival analysis
# need to allow for originations
zero <- max(dur[, 2])
rel.or <- abs(dur[, 2] - zero)
rel.end <- abs(dur[, 3] - zero)
persist <- cbind(st = as.data.frame(rel.or), 
                 age = rel.end, 
                 ext = rep(1, length(rel.or)),
                 aff = litaf$affinity)
names(persist)[1] <- 'st'

# fix the censored ones
persist$ext[dur[, 3] < ptbound] <- 0
reps <- dur[dur[, 3] < ptbound, 2] - ptbound
persist$age[dur[, 3] < ptbound] <- reps
