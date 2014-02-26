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
ggsave(filename = '../doc/figure/east_cast.png', plot = gg,
       width = 15, height = 10)

# substrate
sda <- melt(substats)
sda$L3 <- as.numeric(sda$L3)
sda$L2[sda$L2 == 'bc'] <- 'BC'
sda$L2[sda$L2 == 'end'] <- 'E'
sda$L2[sda$L2 == 'avgcoc'] <- 'Occ'
sda$L2[sda$L2 == 'code'] <- 'Code length'

gs <- ggplot(sda, aes(x = L3, y = value, colour = L1))
gs <- gs + geom_line()
gs <- gs + scale_color_manual(values = cbp,
                              name = 'Substrate\nAffinity')
gs <- gs + labs(x = 'Time (My)')
gs <- gs + facet_wrap(~ L2, scales = 'free')
gs <- gs + theme(axis.title.y = element_text(angle = 0),
                 axis.text = element_text(size = 17),
                 axis.title = element_text(size = 20),
                 legend.text = element_text(size = 15),
                 legend.title = element_text(size = 16),
                 strip.text = element_text(size = 15))
ggsave(file = '../doc/figure/substrate_network.png', plot = gs,
       width = 15, height = 10)

# habitat

hda <- melt(habstats)
hda$L3 <- as.numeric(hda$L3)
hda$L2[hda$L2 == 'bc'] <- 'BC'
hda$L2[hda$L2 == 'end'] <- 'E'
hda$L2[hda$L2 == 'avgcoc'] <- 'Occ'
hda$L2[hda$L2 == 'code'] <- 'Code length'

gh <- ggplot(hda, aes(x = L3, y = value, colour = L1))
gh <- gh + geom_line()
gh <- gh + scale_color_manual(values = cbp,
                              name = 'Habitat\nAffinity')
gh <- gh + labs(x = 'Time (My)')
gh <- gh + facet_wrap(~ L2, scales = 'free')
gh <- gh + theme(axis.title.y = element_text(angle = 0),
                 axis.text = element_text(size = 17),
                 axis.title = element_text(size = 20),
                 legend.text = element_text(size = 15),
                 legend.title = element_text(size = 16),
                 strip.text = element_text(size = 15))
ggsave(file = '../doc/figure/habitat_network.png', plot = gh,
       width = 15, height = 10)
