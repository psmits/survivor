#' Make paleosurvival object
#'
#' Make a 'Surv' object with appropriate censoring. If interval censoring, uses interval2
#' 
#' @param fad vector; first apperance dates
#' @param lad vector; last apperance dates
#' @param start scalar; interval start
#' @param end scalar; interval end
paleosurv <- function(fad, lad, start, end) {
  left <- which(fad > start)
  right <- which(lad <= end)

  fad[left] <- NA
  lad[right] <- NA

  old <- max(fad, na.rm = TRUE)
  fad <- abs(fad - old)
  lad <- abs(lad - old)

  # complete
  exact <- which(!is.na(fad) & !is.na(lad))
  fad[exact] <- lad[exact] <- abs(fad[exact] - lad[exact])

  # right
  rr <- which(!is.na(fad) & is.na(lad))
  fad[rr] <- abs(fad[rr] - abs(end - old))

  Surv(time = fad, time2 = lad, type = 'interval2')
}
