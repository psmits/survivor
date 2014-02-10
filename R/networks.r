library(igraph)
library(mapproj)

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

# network measures
stats <- lapply(biogeosum, function(x) {
                lapply(nets, x)})
