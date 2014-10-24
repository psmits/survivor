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

info <- read.csv('../data/smits-occs.csv', stringsAsFactors = FALSE)
#dur <- read.csv('../data/smits-ranges.csv', stringsAsFactors = FALSE)
bs <- read.delim('../data/payne_bodysize/Occurrence_PaleoDB.txt', 
                 stringsAsFactors = FALSE)
sp.gen <- read.csv('../data/spgen-occs.csv', stringsAsFactors = FALSE)

# non-permian ranging taxa
per <- c('Carboniferous', 'Permian', 'Triassic')
info <- info[info$period %in% per, ]
carboniferous <- info$period == per[1]
permian <- info$period == per[2]
triassic <- info$period == per[3]

# remove things that aren't from the east coast
#info <- info[info$state != 'Western Australia', ]

# remove things that don't have an order
info <- info[info$order_name != '', ]

# each genus
genus.info <- split(info, info$occurrence.genus_name)
outbounds <- laply(genus.info, function(x) {
                   if(all(x$period == per[1]) | all(x$period == per[3])) {
                     TRUE
                   } else {
                     FALSE
                 }})
genus.info <- genus.info[!outbounds]

# remove that range completely through
rangers <- laply(genus.info, function(x) {
                 if (any(x$period == per[1]) & any(x$period == per[3])) {
                   TRUE
                 } else {
                   FALSE
                 }})
genus.info <- genus.info[!rangers]

# how many permian stages
info <- info[info$stage == '', ]
pst <- c('Changhsingian', 'Wuchiapingian', 'Capitanian',
         'Wordian', 'Roadian', 'Kungurian', 'Artinskian',
         'Sakmarian', 'Asselian')
find.dur <- function(x) {
  mm <- which(pst %in% unique(x$stage))
  max(mm) - min(mm) + 1
}
n.stage <- unlist(lapply(genus.info, find.dur))
cohort <- unlist(lapply(genus.info, function(x) 
                        max(which(pst %in% unique(x$stage)))))

# censored?
cen <- unlist(lapply(genus.info, function(x) any(x$period != per[2]))) * 1

# range-in cohort
cohort[which(cen == 1 & cohort == 2)] <- 1


# put it back together
info <- Reduce(rbind, genus.info)

# remove the non-permian occurrences
info <- info[info$period == 'Permian', ]


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

# body size
uni <- unique(bs[, c('taxon_name', 'size')])
uni <- uni[uni$taxon_name %in% info$occurrence.genus_name, ]
uni <- uni[order(uni$taxon_name), ]

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

info <- info[with(info, order(occurrence.genus_name)), ]

# all permian occurrences of a taxon
pocc <- split(info, info$occurrence.genus_name)
pocc <- lapply(pocc, function(x) x[x$period == per[2], ])

# all simultaneous occurrences
socc <- vector(mode = 'list', length = length(pocc))
for(ii in seq(length(pocc))) {
  mm <- info$stage %in% pocc[[ii]]$stage 
  socc[[ii]] <- info[mm, ]$lithology1
}
names(socc) <- names(pocc)

# tabled lithology occurrences
kocc <- lapply(socc, table)
tocc <- lapply(pocc, function(x) table(x$lithology1))

litaf <- list()
for(ii in seq(length(pocc))) {
  litaf[[ii]] <- shprob(occur = pocc[[ii]]$lithology1, avil = socc[[ii]])
}
names(litaf) <- names(pocc)
litprob <- unlist(litaf)
litaf[litaf >= (2/3)] <- 'carbonate'
litaf[litaf <= (1/3)] <- 'clastic'
litaf[litaf > (1/3) & litaf < (2/3)] <- 'mixed'


# environment
seenenv <- info[, c('formation', 'environment')]
mm <- match(seenenv$formation, got$formation)
addenv <- got[mm, 2:3]
addenv <- apply(addenv, 2, as.character)
addenv[is.na(addenv[, 1]), 1] <- seenenv[is.na(addenv[, 1]), 2]
addenv <- gsub(pattern = '[\\"?]',
               replacement = '',
               addenv,
               perl = TRUE)
addenv <- str_replace_all(addenv, 's$', '')
info$environment1 <- addenv[, 1]
info$environment2 <- addenv[, 2]
info$environment1 <- clean.env(info$environment1) # environment
info$environment1[info$environment1 != 'inshore'] <- 'other'

pocc <- split(info, info$occurrence.genus_name)
pocc <- lapply(pocc, function(x) x[x$period == per[2], ])
# all simultaneous occurrences
eocc <- vector(mode = 'list', length = length(pocc))
for(ii in seq(length(eocc))) {
  mm <- info$stage %in% pocc[[ii]]$stage 
  eocc[[ii]] <- info[mm, ]$environment1
}
names(eocc) <- names(pocc)

envaf <- list()
for(ii in seq(length(pocc))) {
  envaf[[ii]] <- shprob(occur = pocc[[ii]]$environment1, avil = eocc[[ii]],
                        aff = 'inshore')
}
names(envaf) <- names(pocc)
envprob <- unlist(envaf)


# values of interest
affinity <- litprob
inshore <- envprob
dur <- n.stage
dur <- dur[names(dur) %in% names(affinity)]
censored <- cen
censored <- censored[names(censored) %in% names(affinity)]
size <- uni$size
size <- size[uni$taxon_name %in% names(affinity)]
cohort <- cohort[names(cohort) %in% names(affinity)]
orders <- unique(info[, c('occurrence.genus_name', 'order_name')])[, 2]


# get remaining genera
sp.gen <- sp.gen[sp.gen$occurrence.genus_name %in% names(dur), ]
#sp.gen <- sp.gen[sp.gen$occurrence.species_name != '', ]
sp <- !is.na(str_match(sp.gen$occurrence.species_name, 'sp.'))
sp.gen$occurrence.species_name[sp] <- 'sp.'
# sp.
split.gen <- split(sp.gen, sp.gen$occurrence.genus_name)
ratio <- lapply(split.gen, 
                function(x) length(unique(x$occurrence.species_name)))
ratio <- unlist(ratio)
