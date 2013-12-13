#' clean lithologies
#'
#' @param lith vector; lithology vector
clean.lith <- function(lith) {

  carbonate <- c('limestone', 'dolomite', 'carbonate', 'lime mudstone', 
                 'grainstone', 'wackestone', 'packstone', 'bafflestone',
                 'framestone', 'bindstone', 'rudstone', 'floatstone')
  clastic <- c('siliciclastic', 'sandstone', 'sandy', 'sandy,calcareous',
               'shale', 'mudstone', 'siltstone', 'conglomerate', 'quartzite',
               'phyllite', 'schist', 'slate')
  mixed <- c('mixed carbonate-siliciclastic', 'marl')

  lith[lith %in% carbonate] <- 'carbonate'
  lith[lith %in% clastic] <- 'clastic'
  lith[lith %in% mixed] <- 'mixed'

  lith
}

#' clean environment assignemnts
#'
#' @param env vector; environmental assignments
clean.env <- function(env) {
  inshore <- c('estuarine/bay', 'deltaic indet', 'delta front', 'delta plain', 
               'prodelta', 'lagoonal', 'lagoonal/restricted shallow subtidal',
               'marginal marine indet', 'coastal indet.', 'sand shoal', 'peritidal',
               'paralic indet.', 'shoreface', 'foreshore', 'shallow subtidal indet.',
               'open shallow subtidal', 'transition zone/lower shoreface')
  offshore <- c('deep-water indet.', 'deep subtidal ramp', 'offshore', 'offshore indet.',
                'offshore ramp', 'offshore shelf', 'basinal (carbonate)', 
                'basinal (siliciclastic)')
  reef <- c('reef, buildup or bioherm')
  none <- c('carbonate indet.', 'marine indet.')

  env[env %in% inshore] <- 'inshore'
  env[env %in% offshore] <- 'offshore'
  env[env %in% reef] <- 'reef'
  env[env %in% none] <- 'none'

  env

}
