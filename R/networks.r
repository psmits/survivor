library(igraph)
library(mapproj)
library(plyr)
library(parallel)

source('../R/mung_help.r')
source('../R/bin_network.r')
source('../R/biogeo_struct.r')
source('../R/window.r')

source('../R/mung.r')

# temporal bins
wdt <- 2
bb <- seq(from = ptbound, to = pst, by = wdt)
bb <- cbind(top = bb[-1], bot = bb[-length(bb)])
bins <- rep(NA, nrow(info))
for(ii in seq(nrow(bb))) {
  oo <- which(info$ma_mid < bb[ii, 1] & info$ma_mid >= bb[ii, 2])
  bins[oo] <- bb[ii, 1]
}
info$bins <- bins

# east coast
# info <- info[info$state != 'Queensland', ]

# occurrence grids
gid <- grid.id(lat = info$paleolatdec, long = info$paleolngdec, 
               width = 2, projection = 'mercator')
gid <- factor(gid, unique(gid))
info$gid <- gid

# remove duplicate grid ids for each bin
foo <- split(info, info$bins)
fgi <- lapply(foo, function(x) split(x, x$gid))
uu <- lapply(fgi, function(x) {
             lapply(x, function(y) {
                    dup <- duplicated(y$occurrence.genus_name)
                    y[!dup, ]})})
uu <- lapply(uu, function(x) {
             rms <- lapply(x, nrow) == 0
             x[!rms]})
uu <- lapply(uu, function(x) Reduce(rbind, x))
occ <- Reduce(rbind, uu)

# bin networks
nets <- network.bin(occ, 
                    bin = 'bins', 
                    taxa = 'occurrence.genus_name', 
                    loc = 'gid')

biogeo <- function(taxawin) {
  biocom <- lapply(taxawin, infomap.community)
  biomes <- Map(contract.vertices, taxawin, lapply(biocom, membership))
  biomes <- lapply(biomes, function(x){
                   x$weight = 1
                   x})
  biomes <- lapply(biomes, simplify)
  mem <- lapply(biocom, function(x) x$membership)

  win.bg <- list(bc = lapply(taxawin, bc),
                 end = Map(endemic, graph = taxawin, membership = mem),
                 avgcoc = Map(avgocc, graph = taxawin, membership = mem),
                 code = lapply(taxawin, code))
  win.bg
}

# network measures
stats <- biogeo(nets)

# occupancy
occp <- lapply(nets, function(x) {
              occupancy(x, membership = membership(infomap.community(x)))})
occp <- Reduce(rbind, occp)
sp.occ <- split(occp, occp$taxa)
mean.occ <- melt(lapply(sp.occ, function(x) mean(x[, 1])))
names(mean.occ) <- c('mean', 'taxa')
cv.occ <- melt(lapply(sp.occ, function(x) var(x[, 1]) / mean(x[, 1])))
names(cv.occ) <- c('cv', 'taxa')

occ.val <- cbind(cv = cv.occ$cv, mean.occ)
occ.val <- occ.val[order(occ.val$taxa), ]
persist <- cbind(persist, occu = occ.val$mean, cvo = occ.val$cv)

# split by category
substrate <- split(occ, occ$lithology1)

habitat <- split(occ, occ$environment)
