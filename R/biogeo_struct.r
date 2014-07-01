#' Calculate BC value for network
#'
#' Biogeographic connectedness is defined as (O - N) / (LN - N)
#' O is the number of edges
#' N is the number of taxa
#' L is the number of localities
#'
#' THIS IS FROM A PAPER THAT I NEED TO ADD THE FORMAL CITATION 
#' SIDOR ET AL. PNAS 2013
#'
#' TODO check for bipartite property
#'
#' @param graph object of class igraph (bipartite)
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
bc <- function(graph, l.small = TRUE) {
  oo <- length(E(graph))
  bip <- bipartite.projection(graph)

  len <- lapply(bip, function(x) length(V(x)))
  ws <- which.min(unlist(len))
  wm <- which.max(unlist(len))
  if (l.small) {
    ll <- length(V(bip[[ws]]))
    nn <- length(V(bip[[wm]]))
  } else if (!l.small) {
    ll <- length(V(bip[[wm]]))
    nn <- length(V(bip[[ws]]))
  }

  bc <- (oo - nn) / (ll * nn - nn)

  bc
}

#' Calculate the average number of endemics
#'
#' An endemic is a taxa that that occurs at only one locality. 
#' This is a property of a bipartite biogeographic network.
#' This function only returns the average number of endemics in a graph.
#'
#' THIS IS FROM A PAPER THAT I NEED TO ADD THE FORMAL CITATION 
#' SIDOR ET AL. PNAS 2013
#'
#' TODO check for bipartite property
#'
#' @param graph object of class igraph (bipartite)
#' @param membership vector of biome membership
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @param trait vector of trait values
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
endemic <- function(graph, membership, l.small = TRUE, trait = NULL) {
  bip <- bipartite.projection(graph)
  len <- lapply(bip, function(x) length(V(x)))
  ws <- which.min(unlist(len))
  wm <- which.max(unlist(len))

  if(l.small) {
    st <- V(bip[[ws]])$name
    tx <- V(bip[[wm]])$name
  } else if(!l.small) {
    st <- V(bip[[wm]])$name
    tx <- V(bip[[ws]])$name
  }

  tx.mem <- membership[V(graph)$name %in% tx]
  loc.mem <- membership[!(V(graph)$name %in% tx)]

  nei <- lapply(st, function(x) neighbors(graph, x))
  mem.nei <- split(nei, loc.mem)
  oo <- lapply(mem.nei, function(x) unique(unlist(x)))
  uni <- list()
  for (ii in seq(length(oo))) {
    shared <- oo[[ii]] %in% unique(unlist(oo[-ii]))
    uni[[ii]] <- oo[[ii]][!shared]
  }

  if(!(is.null(trait))) {
    uni.trait <- lapply(uni, function(x) trait[x])
    uni.tot <- lapply(uni.trait, table)

    other.trait <- lapply(oo, function(x) trait[x])
    other.tot <- lapply(other.trait, table)

    tt <- list()
    for (ii in seq(length(uni.tot))) {
      ww <- match(names(uni.tot[[ii]]), names(other.tot[[ii]]))
      tt[[ii]] <- uni.tot[[ii]] / other.tot[[ii]][ww]
    }
    prop <- split(unlist(tt), names(unlist(tt)))
    avg.end <- lapply(prop, mean)
  } else {
    prop <- unlist(Map(function(x, y) length(x) / length(y), uni, oo))
    num <- sum(prop)
    avg.end <- num / length(oo)
  }

  avg.end
}

#' Find which are the endemic taxa of each locality
#'
#' @param graph object of class igraph (bipartite)
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
which.endemic <- function(graph, l.small = TRUE) {
  bip <- bipartite.projection(graph)
  len <- lapply(bip, function(x) length(V(x)))
  ws <- which.min(unlist(len))
  wm <- which.max(unlist(len))

  if(l.small) {
    st <- V(bip[[ws]])$name
  } else if(!l.small) {
    st <- V(bip[[wm]])$name
  }

  nei <- lapply(st, function(x) neighbors(graph, x))
  endem <- corefind(nei)
  out <- lapply(endem, function(x) V(graph)$name[V(graph) %in% x])

  out
}

#' Find the unique neihbors for each node
#'
#' @param nei list of neighbors
#' @author Peter D Smits psmits@uchicago.edu
#' @references
corefind <- function(nei){
  # find the intersect all pairwise comparisons
  if(length(nei) > 1) {
    pw <- combn(length(nei), 2)
  } else {
    return(0)
  }

  ints <- list()
  for(ii in seq(ncol(pw))) {
    ints[[ii]] <- intersect(nei[[pw[1, ii]]], nei[[pw[2, ii]]])
  }
  uu <- unique(unlist(ints))
  lapply(nei, function(x) x[!(x %in% uu)])
}

#' Average occurence of taxa
#'
#' How many localities do taxa appear in on average?
#'
#' @param graph object of class igraph (bipartite)
#' @param membership vector of biome memberships
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @param trait partition based on trait information (vector)
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
avgocc <- function(graph, membership, l.small = TRUE, trait = NULL) {
  bip <- bipartite.projection(graph)

  len <- lapply(bip, function(x) length(V(x)))
  ws <- which.min(unlist(len))
  wm <- which.max(unlist(len))

  if(l.small) {
    taxa <- V(bip[[wm]])$name
    st <- V(bip[[ws]])$name
  } else if(!l.small) {
    taxa <- V(bip[[ws]])$name
    st <- V(bip[[wm]])$name
  }

  tx.mem <- membership[V(graph)$name %in% taxa]
  loc.mem <- membership[!(V(graph)$name %in% taxa)]

  nei <- lapply(st, function(x) neighbors(graph, x)) # taxa per grid cell
  mem.nei <- split(nei, loc.mem) # taxa per biome
  oo <- lapply(mem.nei, function(x) unique(unlist(x))) # taxa that occur in biome
  occ <- table(unlist(oo)) # number of biomes per taxon
  relocc <- occ/length(oo)

  if(!(is.null(trait))) {
    spl <- split(relocc, trait)
    avg.occ <- lapply(spl, mean)
  } else {
    avg.occ <- mean(relocc) # average relative number of biome occurrences.
  }

  avg.occ
}


# list of biogeographic structure functions
code <- function(x, l.small = TRUE) {
  y <- infomap.community(x, nb.trials = 100)
  code.length(y)
}
biogeosum <- list(bc = bc, end = endemic, avgcoc = avgocc, code = code)


#' Number of BU occurrences per taxa
#'
#' Get the number of BUs that each taxa occurs in for a given network.
#' This is very similar to the avgcooc function except it is not reletavized
#' or averaged.
#'
#' @param graph object of class igraph (bipartite)
#' @param membership vector of biome memberships
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
occupancy <- function(graph, membership) {
  bip <- bipartite.projection(graph)

  # find which half is the taxa
  if(any(grepl('[0-9]', V(bip[[1]])$name))) {
    taxa <- V(bip[[2]])$name
    st <- V(bip[[1]])$name 
  } else {
    taxa <- V(bip[[1]])$name
    st <- V(bip[[2]])$name 
  }

  tx.mem <- membership[V(graph)$name %in% taxa]
  loc.mem <- membership[!(V(graph)$name %in% taxa)]

  nei <- lapply(st, function(x) neighbors(graph, x)) # taxa per grid cell
  mem.nei <- split(nei, loc.mem) # taxa per biome
  oo <- lapply(mem.nei, function(x) unique(unlist(x))) # taxa that occur in biome
  occ <- table(unlist(oo)) # number of biomes per taxon
  out <- as.data.frame(cbind(occ, taxa))
  out$occ <- as.numeric(as.character(out$occ))

  out
}
