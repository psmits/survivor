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



# more information
string <- 'Stratigraphic\ definition\ Current.txt'
geo <- list.files(path = '../data/', pattern = string)
states <- list()
for(ii in seq(length(geo))) {
  states[[ii]] <- read.delim(paste('../data/', geo[ii], sep = ''), sep = '|',
                             stringsAsFactors = FALSE)
}
defin <- Reduce(rbind, states)
defin <- defin[str_detect(defin$Category, 'environment'), 1:4]
defin$Contents <- tolower(defin$Contents)
defin$Contents <- gsub(pattern = '[^[:alnum:] ]', '', defin$Contents)
look <- dlply(defin, .(Stratigraphic.Name), summarize, 
              environs = unlist(strsplit(Contents, 
                                         split = '\\s', 
                                         perl = TRUE)))

envs <- c('estuarine/bay', 'deltaic indet', 'delta front', 'delta plain', 
          'prodelta', 'lagoonal', 'lagoonal/restricted shallow subtidal',
          'marginal marine indet', 'coastal indet.', 'sand shoal', 'peritidal',
          'paralic indet.', 'shoreface', 'foreshore', 'shallow subtidal indet.',
          'open shallow subtidal', 'transition zone/lower shoreface', 
          'sublittoral strand', 'alluvial fan', 'alluvial valley fill', 'alluvial',
          'alluvial plain', 'costal plain', 'delta', 'deltaic/coastal plain',
          'fan delta', 'fluvial coastal', 'nearshore marine', 'prograding shelf',
          'coastal plain', 'deep-water indet.', 'deep subtidal ramp', 'offshore', 
          'offshore indet.', 'offshore ramp', 'offshore shelf', 
          'basinal (carbonate)', 'basinal (siliciclastic)', 'marine shelf', 
          'offshore marine', 'shallow-water', 'fluviatile', 'lacustrine', 'fan',
          'bay', 'stream', 'deltaic', 'shallow', 'marginal', 'marine', 'outer', 
          'shallowwater', 'shelf', 'fandelta', 'beach')

mts <- lapply(look, function(x) apply(x, 1, function(y) y %in% envs))
oob <- Map(function(x, y) x[y, ], look, mts)
oob <- oob[unlist(laply(oob, function(x) length(x) > 0))]

mats <- data.frame(laply(oob, function(x) x[1:3]), stringsAsFactors = FALSE)
modifiers <- c('shallow', 'outer', 'marginal')
shals <- apply(mats, 2, function(x) x %in% modifiers)

strip.na <- function(x) str_replace(x, ' NA', '')
for(ii in seq(nrow(mats))) {
  com <- which(shals[ii, ])
  if (length(com) > 0 && length(com) < 2 && com < 3) {
    com <- seq(from = com, to = com + 1)
    o <- paste(mats[ii, com], collapse = ' ')
    o <- strip.na(o)
    mats[ii, com] <- c(o, NA)
  } else if (length(com) == 2) {
    o <- paste(mats[ii, com], collapse = ' ')
    o <- strip.na(o)
    mats[ii, com] <- c(o, NA)
  }
}

off <- alply(mats, 1, function(x) x %in% c('marine', 'shelf'))
sof <- which(unlist(llply(off, function(x) sum(x) == 2)))
repl <- paste(mats[sof, off[[sof]]], collapse = ' ')
mats[sof, off[[sof]]] <- c(repl, NA)

mats$names <- names(oob)
mats$names <- str_replace(mats$names, '\\s+\\S*$', '')
addd <- cbind(formation = mats$names, 
              environment1 = mats[, 1], 
              environment2 = mats[, 2])


got <- rbind(got, addd)
got <- got[order(got$formation), ]
