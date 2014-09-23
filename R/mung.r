library(reshape2)
library(plyr)
library(survival)
library(stringr)

source('../R/clean_funcs.r')
source('../R/occurrence.r')
source('../R/affinity.r')
source('../R/paleo_surv.r')

source('../R/govt2occ.r')
source('../R/env_match.r')

source('../R/read_fred.r')

info <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)
dur <- read.csv('../data/psmits-ranges.csv', stringsAsFactors = FALSE)
bs <- read.delim('../data/payne_bodysize/Occurrence_PaleoDB.txt', 
                 stringsAsFactors = FALSE)

ptbound <- 252.2
pst <- 298.9

# basic cleaning
# remove taxa that went extinct before the permian
rg <- dur[which(dur[, 3] > pst), 1]
dur <- dur[!(dur[, 1] %in% rg), ]
info <- info[!(info$occurrence.genus_name %in% rg), ]

# remove taxa originating after the permian
rg <- dur[which(dur[, 2] < ptbound), 1]
dur <- dur[!(dur[, 1] %in% rg), ]
info <- info[!(info$occurrence.genus_name %in% rg), ]

# remove all occurrences not from the permian
rg <- which(info$ma_mid > pst | info$ma_mid < ptbound)
info <- info[-rg, ]


# put in information i've learned
# lithology
seenlith <- info[, c('formation', 'lithology1', 'lithology2')]
mm <- match(seenlith$formation, forms.geol$formation)
addlith <- forms.geol[mm, 2:3]
addlith <- apply(addlith, 2, as.character)
addlith[is.na(addlith[, 1]), 1] <- seenlith[is.na(addlith[, 1]), 2]
addlith[is.na(addlith[, 2]), 2] <- seenlith[is.na(addlith[, 2]), 3]
addlith <- gsub(pattern = '[\\"?]',
                replacement = '',
                addlith,
                perl = TRUE)
addlith <- str_replace_all(addlith, 's$', '')
info$lithology1 <- addlith[, 1]
info$lithology2 <- addlith[, 2]

# environment 
seenenv <- info[, c('formation', 'environment')]
addenv <- got[match(seenenv$formation, got$formation), 2:3]
addenv <- apply(addenv, 2, as.character)
addenv[is.na(addenv[, 1]), 1] <- seenenv[is.na(addenv[, 1]), 2]
info$environment <- addenv[, 1]

# what is still missing from env and lith
#nolith <- sort(unique(seenlith$formation)[!(unique(seenlith$formation) 
#                                            %in% forms.geol[, 1])])
#noenv <- sort(unique(seenenv$formation)[!(unique(seenenv$formation) 
#                                          %in% got$formation)])
#write.csv(nolith, file = '../data/missing_lithology.csv')
#write.csv(noenv, file = '../data/missing_environ.csv')

# body size
uni <- unique(bs[, c('taxon_name', 'size')])
uni <- uni[uni$taxon_name %in% dur$genus, ]
uni <- uni[order(uni$taxon_name), ]

dur <- dur[dur$genus %in% uni$taxon_name, ] 
info <- info[info$occurrence.genus_name %in% uni$taxon_name, ]

# final cleaning step
info <- info[info$lithology1 != '', ]  # remove missing lithology
info <- info[info$environment != '', ]  # remove missing environment
rmlith <- c('lithified', 'not reported')
info <- info[!(info$lithology1 %in% rmlith), ]
info <- info[info$lithology1 != 'mixed', ]


# assign information
# lithology
info$lithology1 <- clean.lith(info$lithology1, add = info$lithology2)
info <- info[info$lithology1 != 'mixed', ]
info <- info[info$lithology1 != '', ]  # remove missing lithology
info$environment <- clean.env(info$environment) # environment

info <- info[with(info, order(occurrence.genus_name)), ]
pocc <- split(info, info$occurrence.genus_name)
dur <- dur[dur[, 1] %in% names(pocc), ]

paff <- get.occ(dur[, 2], dur[, 3], info$ma_mid, info$lithology1)
names(paff) <- dur[, 1]

# tabled lithology occurrences
kocc <- lapply(paff, table)
tocc <- lapply(pocc, function(x) table(x$lithology1))

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

# tabled env occurrences
kenv <- lapply(penv, table)
tenv <- lapply(pocc, function(x) table(x$environment))

hab <- list()
for(ii in seq(length(penv))) {
  ins <- shprob(occur = pocc[[ii]]$environment, avil = penv[[ii]], 
                ph1 = 1/3, aff = 'inshore')
  off <- shprob(occur = pocc[[ii]]$environment, avil = penv[[ii]], 
                ph1 = 1/3, aff = 'offshore')
  non <- shprob(occur = pocc[[ii]]$environment, avil = penv[[ii]], 
                ph1 = 1/3, aff = 'none')
  hab[[ii]] <- c(ins, off, non)
}
names(hab) <- names(penv)
hab <- lapply(lapply(hab, which.max), 
              function(x) c('inshore', 'offshore', 'none')[x])

# put it all together
sf <- as.character(dur$genus) %in% names(litaf) & 
as.character(dur$genus) %in% names(hab)
dur <- dur[sf, ]

surv <- paleosurv(dur[, 2], dur[, 3], start = pst, end = ptbound + 5)

# make the data frame for survival analysis
uni <- uni[uni$taxon_name %in% dur$genus, ]
persist <- as.data.frame(cbind(aff = unlist(litprob),
                               hab = unlist(hab),
                               size = uni[, 2]))
persist$aff <- as.numeric(as.character(persist$aff))


# new zealand
zea <- unique(bs[, c('taxon_name', 'size')])
zea <- zea[zea$taxon_name %in% zealand$genus, ]
zea <- zea[order(zea$taxon_name), ]

zealand <- zealand[zealand$genus %in% zea[, 1], ]
zea.dur <- zea.dur[zea.dur$genus %in% zea[, 1], ]

late <- zea.dur[zea.dur$start > ptbound, 1]
zealand <- zealand[zealand$genus %in% late, ]
zea.dur <- zea.dur[zea.dur$genus %in% late, ]

zea.surv <- paleosurv(zea.dur$start, zea.dur$end, start = pst, end = ptbound)



