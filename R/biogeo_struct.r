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
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
endemic <- function(graph, l.small = TRUE) {
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

  nei <- lapply(tx, function(x) neighbors(graph, x))
  tps <- table(unlist(nei))
  ende <- which(unlist(lapply(nei, length)) == 1)
  locs <- table(unlist(nei[ende]))
  ww <- match(names(locs), names(tps))
  avg.end <- sum(locs / tps[ww]) / length(st)

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
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
avgocc <- function(graph, l.small = TRUE) {
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

  # ask the neighbors of all the taxa
  nei <- lapply(taxa, function(x) neighbors(graph, x))
  # (sum l / L) / N
  occ <- lapply(lapply(nei, length), function(x) x / length(st))
  ao <- mean(unlist(occ))

  ao
}


# list of biogeographic structure functions
code <- function(x, l.small = TRUE) {
  y <- infomap.community(x, nb.trials = 100)
  code.length(y)
}
biogeosum <- list(bc = bc, end = endemic, avgcoc = avgocc, code = code)
