# fred data base is in multiple parts
# locality
# paleo
#   header (site information)
#   taxon list
# the goal is to get taxon occurrence connected to locality information
library(reshape2)
library(plyr)
library(stringr)


# locality information
fred.strat <- read.delim(file = '../data/fred_perm_strat.txt',
                         sep = '\t', skip = 9, stringsAsFactors = FALSE,
                         row.names = NULL)
fred.feat <- read.delim(file = '../data/fred_perm_fea.txt', 
                        sep = '\t', skip = 9, stringsAsFactors = FALSE,
                        row.names = NULL)

# taxon occurrence
fred.taxon <- read.delim(file = '../data/fred_perm_taxon.txt', 
                         sep = '\t', skip = 9, stringsAsFactors = FALSE)

lab <- c('FR Number', 'Yard FR Number', 'Locality Type', 'Field Number/Drillhole Name',
         'Depth From', 'Depth To', 'Depth Unit', 'Drill Type', 'Identifier')
taxa <- fred.taxon[!(fred.taxon[, 1] %in% lab), ]
bra <- taxa[taxa[, 1] == 'Brachiopoda', -ncol(taxa)]

# collapse to genera
nam <- str_split(bra[, 2], '\\s')
nam <- llply(nam, str_replace_all, pattern = '\'', '')
sor <- llply(nam, str_detect, pattern = '[\\.\\?]')
sor <- laply(sor, function(x) x[1] == FALSE)

bra <- bra[sor, -1]
bra[, 1] <- laply(str_split(bra[, 1], '\\s'), function(x) x[1])
bra[, 1] <- laply(bra[, 1], str_replace_all, pattern = '\'', '')
bra <- bra[order(bra[, 1]), ]

# change to pressence absence
oo <- apply(bra[, -1], 2, function(x) {
            rr <- x != ''
            x[rr] <- 1
            x[!rr] <- 0
            x})
oo <- data.frame(apply(oo, 2, as.numeric))
bra <- cbind(genus = bra[, 1], oo)

# sum within genera
sg <- split(bra[, -1], bra[, 1])
sg <- Reduce(rbind, llply(sg, function(x) colSums(x)))
bra <- cbind(genus = unique(as.character(bra$genus)), data.frame(sg))
rownames(bra) <- NULL

# occurrence long
oc <- melt(bra)
oc <- oc[oc$value != 0, ]
oc[, 2] <- str_replace(oc[, 2], '\\.(\\d{1})\\.*$', '')
names(oc) <- c('genus', 'locality', 'occurrence')

# match with locality information
st <- str_replace(fred.strat[, 1], '\\/', '\\.')
fred.strat <- fred.strat[match(oc[, 2], st), -ncol(fred.strat)]

sit <- str_replace(fred.feat[, 1], '\\/', '\\.')
fred.feat <- fred.feat[match(oc[, 2], sit), -ncol(fred.feat)]

# bring it all together
zealand <- cbind(oc, fred.feat, fred.strat[, 21:33])
