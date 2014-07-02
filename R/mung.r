library(reshape2)
library(plyr)
library(survival)

source('../R/clean_funcs.r')
source('../R/occurrence.r')
source('../R/affinity.r')
source('../R/paleo_surv.r')

source('../R/govt2occ.r')
source('../R/env_match.r')

info <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)
dur <- read.csv('../data/psmits-ranges.csv', stringsAsFactors = FALSE)
bs <- read.delim('../data/payne_bodysize/Occurrence_PaleoDB.txt', 
                 stringsAsFactors = FALSE)

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

# remove all occurrences not from the permian
rg <- which(info$ma_mid > pst | info$ma_mid < ptbound)
info <- info[-rg, ]

# put in information i've learned
# lithology
seenlith <- info[, c('formation', 'lithology1', 'lithology2')]

ll <- laply(forms.geol, length)
ll[which(ll > 1)] <- c(2, 2, 1, 1, 2, 1, 2, 3, 2, 1, 1, 1, 2)
implith <- Map(function(x, y) x[[y]][1:2], forms.geol, ll)
implith <- cbind(data.frame(form = names(implith)), 
                 Reduce(rbind, implith))
colnames(implith) <- c('form', 'lith1', 'lith2')
addlith <- implith[match(seenlith$formation, names(forms.geol)), 2:3]
addlith <- apply(addlith, 2, as.character)
addlith[is.na(addlith[, 1]), 1] <- seenlith[is.na(addlith[, 1]), 2]
addlith[is.na(addlith[, 2]), 2] <- seenlith[is.na(addlith[, 2]), 3]
#info$lithology1 <- addlith[, 1]

# environment 
seenenv <- info[, c('formation', 'environment')]
addenv <- got[match(seenenv$formation, got$X1), 2:3]
addenv <- apply(addenv, 2, as.character)
addenv[is.na(addenv[, 1]), 1] <- seenenv[is.na(addenv[, 1]), 2]
#info$environment <- addenv[, 1]

# body size
uni <- unique(bs[, c('taxon_name', 'size')])
uni <- uni[uni$taxon_name %in% dur$genus, ]
uni <- uni[order(uni$taxon_name), ]

dur <- dur[dur$genus %in% uni$taxon_name, ] 
info <- info[info$occurrence.genus_name %in% uni$taxon_name, ]


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
info <- info[info$lithology1 != 'mixed', ]
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

# put it all together
sf <- as.character(dur$genus) %in% names(litaf) & 
as.character(dur$genus) %in% names(hab)
dur <- dur[sf, ]

surv <- paleosurv(dur[, 2], dur[, 3], start = pst, end = ptbound + 5)

# make the data frame for survival analysis
uni <- uni[uni$taxon_name %in% dur$genus, ]
persist <- as.data.frame(cbind(aff = unlist(litaf),
                               hab = unlist(hab),
                               size = uni[, 2]))
