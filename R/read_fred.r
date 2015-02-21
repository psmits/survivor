library(reshape2)
library(plyr)
library(stringr)

header <- read.delim(file = '../data/fred_header.txt',
                     sep = '\t', skip = 9, stringsAsFactors = FALSE, 
                     row.names = NULL)

taxon <- read.delim(file = '../data/fred_taxon.txt',
                    sep = '\t', skip = 9, stringsAsFactors = FALSE)
lab <- c('FR Number', 'Yard FR Number', 'Locality Type', 
         'Field Number/Drillhole Name', 'Depth From', 'Depth To', 
         'Depth Unit', 'Drill Type', 'Identifier', 'Biostatistics')
taxon <- taxon[!(taxon[, 1] %in% lab), ]

splits <- str_split(taxon[, 2], '\\s')
splits <- llply(splits, function(x) x[1])
taxon[, 2] <- laply(splits, function(x) str_replace(x, '\\?', ''))

taxon <- taxon[!(str_detect(taxon[, 2], '\\.') | 
                 str_length(taxon[, 2]) == 0), ]

taxon[, -(1:2)] <- apply(taxon[, -(1:2)], 2, function(x) {
                         rr <- x != ''
                         x[rr] <- 1
                         x[!rr] <- 0
                         x})
taxon <- taxon[, -ncol(taxon)]

header[, 1] <- str_replace(header[, 1], '\\/', '\\.')

site <- str_extract(colnames(taxon[, -(1:2)]), '[^\\.]*\\.[^\\.]*')
sites <- c()
for(ii in seq(nrow(site))) {
  sites[ii] <- paste(site[ii, 1], site[ii, 2], sep = '.')
}

by.genus <- split(taxon[, -(1:2)], taxon[, 2], drop = TRUE)
nr <- llply(by.genus, nrow)
by.genus <- llply(by.genus, function(x) apply(x, 2, as.numeric))
by.genus <- Map(function(x, y) matrix(x, nrow = y), x = by.genus, y = nr)
by.genus <- llply(by.genus, colSums)
occs <- data.frame(Reduce(rbind, by.genus))

rownames(occs) <- unique(taxon[, 2])
colnames(occs) <- colnames(taxon[, -(1:2)])

occs <- occs[, colSums(occs) != 0]
occs <- occs[rowSums(occs) != 0, ]

melt.occ <- melt(cbind(occs, rownames(occs), 
                       taxon[match(rownames(occs), taxon[, 2]), 1]))
melt.occ[, 3] <- str_extract(as.character(melt.occ[, 3]), '[^\\.]*\\.[^\\.]*')
melt.occ <- unique(melt.occ)
names(melt.occ) <- c('genus', 'class', 'locality', 'pres')
melt.occ <- melt.occ[melt.occ$pres != 0, ]

corrections <- colnames(header)[-1]
nam <- colnames(header)[-1]
header <- header[, -ncol(header)]
names(header) <- nam
locals <- header[header[, 1] %in% melt.occ$locality, ]

locals <- locals[match(melt.occ[, 3], locals$FR.Number), ]

melt.occ <- cbind(melt.occ, locals[, -1])

melt.occ <- melt.occ[!is.na(melt.occ$Age.Start), ]
melt.occ <- melt.occ[melt.occ$Age.Start[melt.occ$Age.Start <= 66], ]
melt.occ <- ddply(melt.occ, .(genus, class), summarize, 
                  old = max(Age.Start, na.rm = TRUE), 
                  young = min(Age.Stop, na.rm = TRUE))
