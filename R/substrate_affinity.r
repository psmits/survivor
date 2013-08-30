#' Calculate substrate affinity
#'
#' Substrate affinity is a property of an organism that is defined based 
#' on the prevelance of a particular substrate over others. The exact cut-off 
#' for declaring one substrate perfered over another, or a ``mixed'' solution, 
#' has varied between studies. Here, the percentage is user defined.
#'
#' @param substrate vector of all substrates where a taxon is observed.
#' @param level cut-off for declaring ``preference''. numeric value between 0 and 1.
#' @return
#' @export
#' @keywords
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
sub.aff <- function(substrate, level = 0.5) {
  dis <- table(substrate)
  most <- which.max(dis)
  how <- most / sum(dis)
  if (how >= level) {
    out <- names(most)
  } else {
    out <- 'mixed'
  }

  out
}
