# match the govermental lithology information with the units 
# present in the brachiopod occurrence data

load('../data/rock_names.rdata')  # govt.rock is the govt geological information 

occs <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)
recs <- read.csv('../data/geology.csv', stringsAsFactors = FALSE)

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
                     function(x) govt.rock[x])

groups <- occs$geological_group
groups.match <- matcher(groups, govt)
groups.success <- lapply(groups.match$matches, any)
groups.geol <- lapply(groups.match$matches[unlist(groups.success)], 
                      function(x) govt.rock[x])
