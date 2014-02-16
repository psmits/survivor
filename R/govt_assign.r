# assign the exact lithologies of the governmental information

govt <- read.delim('../data/govt_geology.txt', sep = '|', 
                   stringsAsFactors = FALSE)
govt <- govt[!(duplicated(govt[, 1])), ]
govt <- govt[order(govt[, 1]), ]
govt[, 1] <- gsub(', ', '/', govt[, 1])

forms <- read.csv('../data/geology.csv', stringsAsFactors = FALSE)

lith <- tolower(govt$Lithology.Description)
lith <- gsub(pattern = '[^[:alnum:] ]', '', lith)
lith <- lapply(lith, function(x) 
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

# go through the lithologies and match with the important geological information
ww <- oo <- list()
for(ii in seq(length(geo))) {
  aa <- bb <- list()
  for(jj in seq(length(lith))) {
    aa[[jj]] <- grepl(geo[ii], lith[[jj]])
    bb[[jj]] <- grep(geo[ii], lith[[jj]])
  }
  oo[[ii]] <- aa
  ww[[ii]] <- bb
}
mm <- lapply(oo, function(x) {
             Map(function(a, b) a[b], a = lith, b = x)})
mm <- do.call(Map, c(c, mm))
ww <- do.call(Map, c(c, ww))
ww <- lapply(ww, rank)

mm <- Map(function(x, y) x[y], mm, ww)

names(mm) <- gsub(pattern = '\\s', '_', govt[, 1])
