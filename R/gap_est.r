#' Unbiased point estimate of true LAD
#'
#' Based on average gap size
#'
#' @param fad scalar; first apperance datum
#' @param lad scalar; last apperance datum
#' @param h scalar; number of samples
#' @return average gap size
gap <- function(fad, lad, h) {
  r <- abs(fad - lad)

  ru <- r / (h - 1)

  ru
}
