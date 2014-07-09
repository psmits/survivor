# match the govermental lithology information with the units 
# present in the brachiopod occurrence data
library(plyr)
library(reshape2)
source('../R/lith_tabler.r')

load('../data/rock_names.rdata')  # govt.rock is the govt geological information 

occs <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)
occs <- occs[occs$period == 'Permian', ]

govt <- names(govt.rock)
govt <- gsub('[_/]', ' ', govt)

matcher <- function(test, base) {
  test <- gsub('[^[:alnum:] ]', '', test)
  test[test %in% LETTERS] <- ''
  test <- unique(test)
  test <- test[test != '']

  mat <- list()
  for(ii in seq(length(test))) {
    oo <- list()
    for(jj in seq(length(base))) {
      oo[[jj]] <- grepl(test[[ii]], base[jj])
    }
    mat[[ii]] <- oo
  }
  mat <- lapply(mat, unlist)

  names(mat) <- test

  list(test = test, matches = mat)
}

member <- occs$member
member.match <- matcher(member, govt)
member.success <- lapply(member.match$matches, any)
member.geol <- lapply(member.match$matches[unlist(member.success)],
                      function(x) govt.rock[x])

forms <- occs$formation
forms.match <- matcher(forms, govt)
forms.success <- lapply(forms.match$matches, any)
forms.geol <- lapply(forms.match$matches[unlist(forms.success)],
                     function(x) govt.rock[x])  # my rock information

# what is missing
obs <- melt(forms.success)
obs <- obs[order(obs[, 2]), ]
unknown <- obs[!obs[, 1], 2]

# grab the lithologies from the occ
occ.form <- forms %in% names(which(unlist(forms.success)))
form.lith <- cbind(occs$lithology1[occ.form], occs$lithology2[occ.form])
form.lith <- aaply(form.lith, 1, function(x) gsub('[^[:alnum:] ]', '', x))
form.lith <- cbind(forms[occ.form], form.lith)
form.lith <- form.lith[!duplicated(form.lith[, 1]), ]

# make a pretty table to show how much i've improved!
forms.tab <- lith.tab(forms.geol, form.lith)
label(forms.tab) <- 'tab:form_lith'
print.xtable(forms.tab, file = '../doc/form_lith.tex',
             include.rownames = FALSE)

groups <- occs$geological_group
groups.match <- matcher(groups, govt)
groups.success <- lapply(groups.match$matches, any)
groups.geol <- lapply(groups.match$matches[unlist(groups.success)], 
                      function(x) govt.rock[x])
