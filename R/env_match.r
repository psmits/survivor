library(xtable)
library(plyr)

gov <- read.csv('../data/geology.csv', stringsAsFactors = FALSE)
occ <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)

form <- cbind(occ$geological_group, occ$formation, occ$member)

form <- aaply(form, 1, function(x) gsub(pattern = '[^[:alnum:] ]', '',
                                        x, perl = TRUE))
known <- unique(gov$formation[gov$environment1 != ''])

# find out which in the pdbd i know about currently
oo <- list()
for(ii in seq(ncol(form))) {
  ll <- list()
  for(jj in seq(length(known))) {
    ll[[jj]] <- which(grepl(pattern = known[jj], form[, ii]))
  }
  oo[[ii]] <- ll
}
oo <- lapply(oo, unlist)
oo <- oo[[which.max(unlist(lapply(oo, length)))]]

kk <- occ[oo, ]  # this is the data i have environment for currently
kk <- cbind(kk$formation, kk$environment)
kk <- aaply(kk, 1, function(x) gsub(pattern = '[^[:alnum:] ]', '',
                                    x, perl = TRUE))
kk <- data.frame(kk[!duplicated(kk[, 1]), ], stringsAsFactors = FALSE)

# combine my info with the pbdb
mm <- lapply(known, function(x) grep(x, kk[, 1]))
good <- which(unlist(lapply(mm, any)))
mm <- mm[good]
names(mm) <- known[good]

# for each good formation, grab the environmental
grab <- gov$formation %in% names(mm)
got <- gov[grab, ]
got <- got[got$environment1 != '', ]
got <- got[!duplicated(got$formation), ]
got <- data.frame(cbind(got$formation, got$environment1, got$environment2), stringsAsFactors = FALSE)

# ok, make a table that has old and new next to each other
improve <- cbind(kk, got[, -1])
names(improve) <- c('formation', 'PBDB paleoenvironment', 'my paleoenvironment 1', 'my paleoenvironment 2')
rownames(improve) <- NULL
improve <- improve[order(improve$formation), ]
improve.table <- xtable(improve)
label(improve.table) <- 'tab:paleoenv'
print.xtable(improve.table, file = '../doc/paleoenv_tab.tex', 
             include.rownames = FALSE)
