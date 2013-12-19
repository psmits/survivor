#' Calculate substrate affinity
#'
#' Substrate affinity is a property of an organism that is defined based 
#' on the prevelance of a particular substrate over others. The exact cut-off 
#' for declaring one substrate perfered over another, or a ``mixed'' solution, 
#' has varied between studies. Here, the percentage is user defined.
#'
#' @param trait vector of all substrates where a taxon is observed.
#' @param middle character of mixed option
#' @param level cut-off for declaring ``preference''. numeric value between 0 and 1.
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
aff <- function(substrate, middle, level = 0.5) {
  dis <- table(substrate)
  most <- which.max(dis)
  how <- most / sum(dis)
  if (how >= level) {
    out <- names(most)
  } else {
    out <- middle
  }

  out
}

#' Simpson and Harnik Bayes method
#'
#' @param occur vector; substrate where taxon occurs
#' @param avil vector; all avaliable substrates during taxon's lifetime
#' @param aff character string; affinity used as 1
shprob <- function(occur, avil, ph1 = 0.5, ph2 = 0.5, aff = 'carbonate') {
  pe <- sum(avil == aff) / length(avil)

  peh1 <- pbinom(sum(occur == aff), length(occur), pe)
  peh2 <- pbinom(sum(occur != aff), length(occur), 1 - pe)

  p <- (peh1 * ph1) / ((peh1 * ph1) + (peh2 * ph2))
  p
}