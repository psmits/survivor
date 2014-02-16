#' Exact from lithological description the terms of interest
#'
#' @param desc list; lithological descriptions
#' @param rocks vector; terms of interest
#' @return list of terms interest
desc2lith <- function(desc, rocks) {
  ww <- oo <- list()
  for(ii in seq(length(rocks))) {
    aa <- bb <- list()
    for(jj in seq(length(desc))) {
      aa[[jj]] <- grepl(rocks[ii], desc[[jj]])
      bb[[jj]] <- grep(rocks[ii], desc[[jj]])
    }
    oo[[ii]] <- aa
    ww[[ii]] <- bb
  }
  mm <- lapply(oo, function(x) {
               Map(function(a, b) a[b], a = desc, b = x)})
  mm <- do.call(Map, c(c, mm))
  ww <- do.call(Map, c(c, ww))
  ww <- lapply(ww, rank)

  mm <- Map(function(x, y) x[y], mm, ww)

  mm
}
