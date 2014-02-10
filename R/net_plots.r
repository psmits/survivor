library(mgcv)
library(ggplot2)
library(reshape2)
library(scales)

source('../R/networks.r')

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')

bd <- melt(stats)
bd$L2 <- as.numeric(bd$L2)
bd <- bd[!(bd[, 1] == Inf | is.nan(bd[, 1])), ]

bd$L1[bd$L1 == 'bc'] <- 'BC' 
bd$L1[bd$L1 == 'end'] <- 'E'
bd$L1[bd$L1 == 'avgcoc'] <- 'Occ'
bd$L1[bd$L1 == 'code'] <- 'Code length'

gg <- ggplot(bd, aes(x = L2, y = value))
gg <- gg + geom_line()
gg <- gg + labs( x= 'Time (My)')
gg <- gg + facet_wrap(~ L1, scales = 'free_y')
gg <- gg + theme(axis.title.y = element_text(angle = 0),
                 axis.text = element_text(size = 17),
                 axis.title = element_text(size = 20),
                 legend.text = element_text(size = 15),
                 legend.title = element_text(size = 16),
                 strip.text = element_text(size = 15))
