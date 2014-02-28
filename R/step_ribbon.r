library(ggplot2)
library(proto)
#Modified version of ggplot:::stairstep function
stairstepn <- function( data, direction="hv", yvars="y" ) {
  direction <- match.arg( direction, c( "hv", "vh" ) )
  data <- as.data.frame( data )[ order( data$x ), ]
  n <- nrow( data )

  if ( direction == "vh" ) {
    xs <- rep( 1:n, each = 2 )[ -2 * n ]
    ys <- c( 1, rep( 2:n, each = 2 ) )
  } else {
    ys <- rep( 1:n, each = 2 )[ -2 * n ]
    xs <- c( 1, rep( 2:n, each = 2))
  }

  data.frame(
             x = data$x[ xs ]
             , data[ ys, yvars, drop=FALSE ]
             , data[ xs, setdiff( names( data ), c( "x", yvars ) ), drop=FALSE ]
             ) 
}

stat <- stepribbon <- function( mapping=NULL, data=NULL, geom="ribbon", position="identity" ) {
  StatStepribbon$new( mapping=mapping, data=data, geom=geom, position=position )
}

StatStepribbon <- proto(ggplot2:::Stat, {
                        objname <- "stepribbon"
                        desc <- "Stepwise area plot"
                        desc <- outputs <- list(
                                                x = "stepped independent variable",
                                                ymin = "stepped minimum dependent variable",
                                                ymax = "stepped maximum dependent variable"
                                                )
                        required <- aes <- c( "x", "ymin", "ymax" )

                        default <- geom <- function(.) GeomRibbon
                        default <- aes <- function(.) aes( x=..x.., ymin = ..y.., ymax=Inf )

                        calculate <- function( ., data, scales, direction = "hv", yvars = c( "ymin", "ymax" ), ...) {
                          stairstepn( data = data, direction = direction, yvars = yvars )
                        }

                        examples <- function(.) {
                          DF <- data.frame( x = 1:3, ymin = runif( 3 ), ymax=rep( Inf, 3 ) )
                          ggplot( DF, aes( x=x, ymin=ymin, ymax=ymax ) ) + stat <- stepribbon()

                        }

             })
