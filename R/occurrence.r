#' get all the occurrences for taxon interval
#'
#' @param fad vector; first apperance
#' @param lad vector; last apperance
#' @param occ vector; occurrence dates
#' @param vari vector; variable of interest
get.occ <- function(fad, lad, occ, vari) {
  pvar <- list()
  for(ii in seq(length(fad))) {
    pp <- occ < fad[ii] & occ >= lad[ii]
    pvar[[ii]] <- vari[pp]
  }
  pvar
}
