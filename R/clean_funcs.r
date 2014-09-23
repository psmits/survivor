#' clean lithologies
#'
#' @param lith vector; lithology vector
clean.lith <- function(lith, add = NULL) {

  other <- c('gabbro', 'metagabbro', 'volcanic', 'basalt', 'basaltic', 
             'andesite', 'rhyolite', 'quartz', 'granodiorite', 'tuff', 'ruff',
             'monzogranite', 'granite', 'diorite', 'dacite', 'pyroclastic', 
             'latite')

  if(!is.null(add)) {
    bads <- lith %in% other
    lith[bads] <- add[bads]
  }

  carbonate <- c('limestone', 'dolomite', 'carbonate', 'lime mudstone', 
                 'grainstone', 'wackestone', 'packstone', 'bafflestone',
                 'framestone', 'bindstone', 'rudstone', 'floatstone', 
                 'calcarenite', 'calcirudite', 'calcilutite')
  clastic <- c('siliciclastic', 'sandstone', 'sandy', 'sandy,calcareous',
               'shale', 'mudstone', 'siltstone', 'conglomerate', 'quartzite',
               'phyllite', 'schist', 'slate', 'breccia', 'diamictite',
               'arenite', 'chert', 'claystone')
  mixed <- c('mixed carbonate-siliciclastic', 'marl', 'carbonatesiliciclastic')

  lith[lith %in% carbonate] <- 'carbonate'
  lith[lith %in% clastic] <- 'clastic'
  lith[lith %in% mixed] <- 'mixed'

  lith[lith %in% other] <- ''

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
               'open shallow subtidal', 'transition zone/lower shoreface', 
               'sublittoral strand', 'alluvial fan', 'alluvial valley fill', 'alluvial',
               'alluvial plain', 'costal plain', 'delta', 'deltaic/coastal plain',
               'fan delta', 'fluvial coastal', 'nearshore marine', 'prograding shelf',
               'coastal plain', 'shallowwater', 'fluviatile', 'subtidal', 
               'upper shoreface', 'lower shoreface', 'Basin-margin alluvial apron')
  offshore <- c('deep-water indet.', 'deep subtidal ramp', 'offshore', 'offshore indet.',
                'offshore ramp', 'offshore shelf', 'basinal (carbonate)', 
                'basinal (siliciclastic)', 'marine shelf', 'offshore marine', 
                'offshore transition-offshore', 'lower shoreface-offshore transition')
  reef <- c('reef, buildup or bioherm')
  none <- c('carbonate indet.', 'marine indet.', 'shelf', 'shallow marine')

  env[env %in% inshore] <- 'inshore'
  env[env %in% offshore] <- 'offshore'
  env[env %in% reef] <- 'reef'
  env[env %in% none] <- 'none'

  env

}
