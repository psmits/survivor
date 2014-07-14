library(xtable)
library(plyr)
library(stringr)

gov <- read.csv('../data/geology.csv', stringsAsFactors = FALSE)
occ <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)
occ <- occ[occ$period == 'Permian', ]

gov <- gov[gov$environment1 != '', ]

# problem with partial matches
# parse down to the formations observed in each
clean.kwn <- str_replace(gov$formation, '\\s+\\S*$', '')
good <- sort(unique(clean.kwn))
known <- lapply(good, function(yy) {
                which(laply(str_match_all(occ$formation, yy), 
                            function(x) length(x) > 0))})

out <- list()
for(ii in seq(length(good))) {
  rform <- unique(occ[known[[ii]], 'formation'])
  renv <- gov[clean.kwn %in% good[ii], 
              c('formation', 'environment1', 'environment2')]
  renv <- renv[!duplicated(renv$formation), ]
  if(length(rform) > 0) {
    out[[ii]] <- data.frame(cbind(formation = rform, renv[, 2:3]))
  }
}
out <- Reduce(rbind, out)
got <- out[out$environment1 != '', ]
