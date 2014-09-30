# match the govermental lithology information with the units 
# present in the brachiopod occurrence data
library(plyr)
library(reshape2)
library(stringr)
source('../R/lith_tabler.r')

load('../data/rock_names.rdata')  # govt.rock is the govt geological information 

occs <- read.csv('../data/smits-occs.csv', stringsAsFactors = FALSE)
occs <- occs[occs$period == 'Permian', ]
govt <- names(govt.rock)
govt <- gsub('[_/]', ' ', govt)

me <- read.csv('../data/geology.csv', stringsAsFactors = FALSE)

# problem is with partial matches
# parse down to simple words
clean <- str_replace(govt, '\\s+\\S*$', '')
good <- sort(unique(clean))
known <- lapply(good, function(yy) {
                which(laply(str_match_all(occs$formation, yy),
                            function(x) length(x) > 0))})

out <- list()
for(ii in seq(length(good))) {
  rform <- unique(occs[known[[ii]], 'formation'])
  rlith <- govt.rock[clean %in% good[ii]]
  rlith <- lapply(rlith, function(x) x[1:2])
  if(length(rform) > 0) {
    out[[ii]] <- rlith
  }
}
out <- unlist(out, recursive = FALSE)
out <- cbind(names(out), data.frame(Reduce(rbind, out)))
names(out) <- c('formation', 'lith1', 'lith2')


# now again with my stuff
mrcl <- str_replace(me$formation, '\\s+\\S*$', '')
neut <- sort(unique(mrcl))
known <- lapply(neut, function(yy) {
                which(laply(str_match_all(occs$formation, yy),
                            function(x) length(x) > 0))})

temp <- list()
for(ii in seq(length(neut))) {
  rform <- unique(occs[known[[ii]], 'formation'])
  rlith <- me[mrcl %in% neut[ii], c('formation', 'lithology1', 'lithology2')]
  rlith <- rlith[!duplicated(rlith$formation), ]
  if(length(rform) > 0) {
    temp[[ii]] <- rlith
  }
}
temp <- Reduce(rbind, temp)
names(temp)[2:3] <- c('lith1', 'lith2')

# combined and again
forms.geol <- rbind(temp, out)
forms.geol$formation <- str_replace_all(forms.geol$formation, '_', ' ')

forms.geol$formation <- str_replace(forms.geol$formation, '\\s+\\S*$', '')
forms.geol <- forms.geol[!duplicated(forms.geol$formation), ]

matched <- lapply(forms.geol$formation, function(yy) {
                  which(laply(str_match_all(occs$formation, yy),
                              function(x) length(x) > 0))})

nice <- list()
for(ii in seq(nrow(forms.geol))) {
  ff <- unique(occs[matched[[ii]], 'formation'])
  if(length(ff) > 1) {
    li <- Reduce(rbind, replicate(length(ff), forms.geol[ii, 2:3], 
                                  simplify = FALSE))
    ll <- cbind(formation = ff, li)
  } else {
    ll <- c(formation = ff, forms.geol[ii, 2:3])
  }
  if(length(ff) > 0) {
    nice[[ii]] <- ll
  }
}
forms.geol <- data.frame(Reduce(rbind, nice))
forms.geol <- data.frame(t(aaply(forms.geol, 2, unlist)), row.names = NULL)
