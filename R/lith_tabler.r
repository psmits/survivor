require(xtable)

#' Make a table of lithological assignments
#'
#' @param geol list
#' @param lith data.frame; observed lithologies
lith.tab <- function(geol, occ) {
  # get all the names
  nn <- Reduce(c, lapply(geol, names))

  lith <- list()
  for(ii in seq(length(geol))) {
    if(length(geol) == 1) {
      lith[[ii]] <- geol[[ii]][[1]][1:2]
    } else {
      lith[[ii]] <- lapply(geol[[ii]], function(x) x[1:2])
    }
  }
  lith <- unlist(lith, recursive = FALSE)
  
  tab <- cbind(nn, Reduce(rbind, lith))
  tab <- tab[!is.na(tab[, 2]), ]

  # clean off the names
  tab[, 1] <- gsub('_', ' ', tab[, 1])
  tab <- data.frame(tab, stringsAsFactors = FALSE)
  rownames(tab) <- NULL

  # match the occ information with the table
  name.match <- lapply(occ[, 1], function(x) grep(x, tab[, 1]))
  copies <- lapply(name.match, length)
  occ.lith <- list()
  for(ii in seq(nrow(occ))) {
    ml <- matrix(rep(occ[ii, 2:3], copies[[ii]]), 
                 nrow = copies[[ii]], byrow = TRUE)
    occ.lith[[ii]] <- ml
  }

  tab <- cbind(tab[unlist(name.match), 1], Reduce(rbind, occ.lith), 
               tab[unlist(name.match), 2:3])
  tab <- tab[!duplicated(tab[, 1]), ]
  tab <- tab[order(tab[, 1]), ]
  names(tab) <- c('geological unit', 'PDBD lithology 1', 'PBDB lithology 2',
                  'my lithology 1', 'my lithology 2')


  xtab <- xtable(tab)
  xtab
}
