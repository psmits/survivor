library(igraph)

source('../R/networks.r')


# bipartite projections
binets <- lapply(nets, bipartite.projection)

# get the projection that is sites
get.site <- function(x, pattern = '\\(') {
  out <- c()
  for(ii in seq(length(x))) {
    out[ii] <- any(grep(pattern, V(x[[ii]])$name))
  }
  out
}

wloc <- lapply(binets, get.site)
locnet <- unlist(Map(function(x, y) x[y], binets, wloc), recursive = FALSE)

locinfo <- lapply(locnet, function(x) {
                  infomap.community(x, 
                                    e.weights = E(x)$weight, 
                                    nb.trials = 1000)})

