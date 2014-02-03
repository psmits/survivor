#' Classical confidence intervals on stratigraphic ranges
#'
#' Based on some stuff by Sadler
#'
#' @param fad scalar; first apperance datum
#' @param lad scalar; last apperance datum
#' @param h scalar; number of samples
#' @param ci scalar; confidence level
#' @return estimated true strat range
classic <- function(fad, lad, h, ci = 0.5) {
  r <- abs(fad - lad)

  ep <- -1 / (h - 1)

  brack <- ((1 - ci) ^ ep) - 1

  out <- r * brack

  out
}
