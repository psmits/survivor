#' Bin window utility function
#'
#' Slides along a time line and cut data into the different windows. 
#' Windows overlap for obvious reasons.
#'
#' TODO also work with FAD, LADs because might capture more accurate statement of diversity.
#' Though, the high degree of time averaging with expected window width might limit the utility of this.
#'
#' TODO this doesn't really ``slide.'' This just bins. Need to rethink strategy.
#' Technically, it should look at bin and the adjacent bins.
#'
#' @param data object of class df
#' @param width sliding window width
#' @param time string corresponding to the column of data with time estimate
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
binner <- function(data, width, time) {
  my <- data[, time]
  bin <- seq(from = 0, to = ceiling(max(my)), by = width)
  top <- bin[-1]
  bot <- bin[-length(bin)]
  bins <- cbind(top, bot)

  # get the taxa from each window, based on their occurrence time
  out <- vector(mode = 'list', length = nrow(bins))
  names(out) <- top
  for(ii in seq(nrow(bins))) {
    tt <- which(my <= bins[ii, 1] & my > bins[ii, 2])
    out[[ii]] <- data[tt, ]
  }

  out
}

#' Make bipartite network from sliding window
#'
#' Convenience function to automate making biogeographic networks from a 
#' sliding window over time.
#'
#' @param data data.frame with necessary taxonomic and locality
#' @param width sliding window width
#' @param time string corresponding to the column of data with time estimate
#' @param taxa string of taxonomic unit corresponding to column of dat
#' @param loc string of locality unit corresponding to column of dat
#' @return
#' @author Peter D Smits <psmits@uchicago.edu>
#' @export
#' @examples
network.bin <- function(data, bin, taxa, loc) {
  bb <- split(data, data[, bin])
  out <- lapply(bb, bin.network,
                taxa = taxa, loc = loc)
  out
}

#' Sliding window utility function
#'
#' Slide along a time line and create a biogeographic network. 
#' Then calculate summary statistics from the biogeographic network.
#'
#' @param data data.frame with necessary information
#' @param width sliding window width
#' @param speed the difference between windows
#' @param ceil logical if should use ceiling of oldest age estimate
#' @param time string corresponding to the column of data with time estimate
#' @param taxa string of taxonomic unit corresponding to column of dat
#' @param loc string of locality unit corresponding to column of dat
#' @param biogeo list of summary statistics to calculate 
#' @return
#' @author Peter D Smits <psmits@uchicago.edu>
#' @export
#' @examples
slide <- function(data, width, speed = 1, time, taxa, loc, biogeo) {
  my <- data[, time]

  top <- ceiling(max(my))

  tops <- seq(top / speed)
  bots <- tops - width
  rms <- which(bots < 0)
  bins <- cbind(top = tops[-rms], bot = bots[-rms])

  # slide along the time at the width
  out <- vector(mode = 'list', length = nrow(bins))
  names(out) <- bins[, 1]
  for(ii in seq(nrow(bins))) {
    tt <- which(my <= bins[ii, 1] & my > bins[ii, 2])
    out[[ii]] <- data[tt, ]
  }

  # bin.network to the window
  netbin <- lapply(out, bin.network, 
                   taxa = taxa, loc = loc)
  # get rid of windows without any nodes
  rms <- which(unlist(lapply(netbin, vcount)) == 0)
  netbin <- netbin[-rms]


  out <- lapply(biogeo, function(x) {
                lapply(netbin, x)})
}
