#' Make paleosurvival object
#'
#' Make a 'Surv' object with appropriate censoring. If interval censoring, uses interval2
#' 
#' @param fad vector; first apperance dates
#' @param lad vector; last apperance dates
#' @param start scalar; interval start
#' @param end scalar; interval end
paleosurv <- function(fad, lad, start, end) {
  old <- max(fad)
  fad <- abs(fad - old)
  lad <- abs(lad - old)
  start <- abs(start - old)
  end <- abs(end - old)

  left <- which(fad < start)
  right <- which(lad >= end)

  fad[left] <- NA
  lad[right] <- NA

  exact <- which(!is.na(fad) & !is.na(lad))
  fad[exact] <- lad[exact] <- abs(fad[exact] - lad[exact])

  Surv(time = fad, time2 = lad, type = 'interval2')

}
