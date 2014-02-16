# assign the exact lithologies of the governmental information
source('../R/desc2lith.r')

govt <- read.delim('../data/govt_geology.txt', sep = '|', 
                   stringsAsFactors = FALSE)
govt <- govt[!(duplicated(govt[, 1])), ]
govt <- govt[order(govt[, 1]), ]
govt[, 1] <- gsub(', ', '/', govt[, 1])

forms <- read.csv('../data/geology.csv', stringsAsFactors = FALSE)

glith <- tolower(govt$Lithology.Description)
glith <- gsub(pattern = '[^[:alnum:] ]', '', glith)
glith <- lapply(glith, function(x) 
                unlist(strsplit(x, split = '\\s', perl = TRUE)))

interest <- c('limestone', 'dolomite', 'carbonate', 'lime mudstone', 
              'grainstone', 'wackestone', 'packstone', 'bafflestone',
              'framestone', 'bindstone', 'rudstone', 'floatstone', 
              'siliciclastic', 'sandstone', 'sandy', 'sandy,calcareous', 
              'shale', 'mudstone', 'siltstone', 'conglomerate', 'quartzite',
              'phyllite', 'schist', 'slate', 'mixed carbonate-siliciclastic', 
              'marl')
geo <- unique(c(forms$lithology1, forms$lithology2, interest))
geo <- geo[geo != '']

govt.rock <- desc2lith(glith, geo)
names(govt.rock) <- gsub(pattern = '\\s', '_', govt[, 1])
save(govt.rock, file = '../data/rock_names.rdata')


# extract the lithologies from the occurrence information
occs <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)
olith <- occs$lithdescript
olith <- gsub(pattern = '[^[:alnum:] ]', '', olith)
olith <- lapply(olith, function(x) 
                unlist(strsplit(x, split = '\\s', perl = TRUE)))

occ.rock <- desc2lith(olith, geo)
save(occ.rock, file = '../data/occ_rocks.rdata')
