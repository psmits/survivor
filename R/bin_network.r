#' Bio-network within a bin
#'
#' For a given temporal bin, determine the bipartite network as defined in 
#' Sidor et al. 2011 PNAs and Vilhena et al. 2011 Scientific Reports. 
#' The taxa and locality unit are user defined.
#'
#' @param dat data.frame with necessary taxonomic and locality
#' @param taxa string of taxonomic unit corresponding to column of dat
#' @param loc string of locality unit corresponding to column of dat
#' @return
#' @author Peter D Smits <psmits@uchicago.edu>
#' @export
#' @examples
bin.network <- function(dat, taxa, loc) {
  tt <- as.character(dat[, taxa])
  ll <- as.character(dat[, loc])
  g <- graph.data.frame(cbind(tt, ll), directed = FALSE)
  V(g)$type <- V(g)$name %in% unique(dat[, loc])
  g
}

#' Number of localities in biogeographic network
#'
#' Determine how many localities in the bipartite biogeographic network. 
#'
#' @param graph object of class igraph (bipartite)
#' @param l.small logical if smaller part of bipartite projection is locality information
#' @return
#' @export
#' @keywords
#' @author
#' @references
#' @examples
numloc <- function(graph, l.small = TRUE) {
  bip <- bipartite.projection(graph)

  len <- lapply(bip, function(x) length(V(x)))
  ws <- which.min(unlist(len))
  wm <- which.max(unlist(len))

  if(l.small) {
    nn <- vcount(bip[[ws]])
  } else if(!l.small) {
    nn <- vcount(bip[[wm]])
  }

  nn
}

