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
age <- dur[, 2] - dur[, 3]
persist <- cbind(st = rep(0, length(age)),
                 as.data.frame(age),
                 ext = rep(1, length(age)),
                 aff = litaf$affinity)
