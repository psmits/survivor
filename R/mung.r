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

info <- read.csv('../data/psmits-occs.csv')
dur <- read.csv('../data/psmits-ranges.csv')


# remove missing lithology information
info <- info[info$lithology1 != '', ]
info$lithology1 <- gsub(pattern = '[\\"?]',
                        replacement = '',
                        info$lithology1, 
                        perl = TRUE)

litaf <- ddply(info, .(occurrence.genus_name), summarise,
              affinity = names(which.max(table(lithology1))))

litaf$occurrence.genus_name <- as.character(litaf$occurrence.genus_name)
names(litaf)[1] <- 'genus'

sf <- as.character(dur$genus) %in% litaf$genus 
dur <- dur[sf, ]


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
