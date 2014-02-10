#' Assign lat long grid
#'
#' @param lat
#' @param long
#' @param width
#' @return
#' @export
#' @author Peter D Smits <psmits@uchicago.edu>
#' @references
#' @examples
grid.id <- function(lat, long, width = 2, projection = '', ...) {


  latrng <- range(lat, na.rm = TRUE)
  latb <- ceiling(abs(Reduce('-', latrng)) / width)
  lngrng <- range(long, na.rm = TRUE)
  lngb <- ceiling(abs(Reduce('-', lngrng)) / width)
  
  pro <- mapproject(x = long, y = lat, projection = projection)

  plat <- seq(from = min(pro$y, na.rm = TRUE),
              to = max(pro$y, na.rm = TRUE),
              length.out = latb)
  plng <- seq(from = min(pro$x, na.rm = TRUE),
              to = max(pro$x, na.rm = TRUE),
              length.out = lngb)

  # make the grid and then project it
  glat <- cut(pro$y, breaks = plat, include.lowest = TRUE)
  glng <- cut(pro$x, breaks = plng, include.lowest = TRUE)


  gid <- interaction(glat, glng)
  gid
}
