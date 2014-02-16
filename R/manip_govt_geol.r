library(plyr)

info <- read.csv('../data/psmits-occs.csv', stringsAsFactors = FALSE)

string <- 'Stratigraphic\ names\ Current.txt'
geo <- list.files(path = '../data/', pattern = string)
states <- list()
for(ii in seq(length(geo))) {
  states[[ii]] <- read.delim(paste('../data/', geo[ii], sep = ''), sep = '|',
                             stringsAsFactors = FALSE)
}
geology <- Reduce(rbind, states)
geology <- geology[geology$Primary.Lithology.Group != '', ]

# match the stratigraphic names
forms <- unique(c(info$formation, info$geological_group, info$member))
# exclude ""
forms <- forms[forms != '']
# exclude single letter
forms <- forms[!(forms %in% LETTERS)]
forms <- forms[forms != 'Lower']
forms <- gsub(pattern = '[^[:alnum:] ]', '', forms, perl = TRUE)

geology[, 1] <- gsub(pattern = '[^[:alnum:] ,]', '', geology[, 1], perl = TRUE)

mat <- list()
for(ii in seq(length(forms))) {
  oo <- list()
  for(jj in seq(length(geology[, 1]))) {
    oo[jj] <- grepl(forms[[ii]], geology[jj, 1])
  }
  mat[[ii]] <- oo
}
mat <- lapply(mat, unlist)

matches <- Reduce(rbind, llply(mat, function(x) geology[x, ]))

wf <- lapply(forms, function(x) 
             laply(matches[, 1], function(y, x) grepl(x, y), x = x))

coms <- grep(',', matches[, 1])
matches <- matches[-grep('ungrouped', matches[, 1]), ]
matches <- matches[-grep('Volcanics', matches[, 1]), ]
coms <- grep(',', matches[, 1])

ww <- lapply(forms, function(y) 
             lapply(matches[coms, 1], function(x, y) grep(y, x), y = y))
ww <- unlist(lapply(ww, function(x) any(unlist(x) == 1)))

# get the geology out of the matches
write.table(matches[, c(1, 3, 5, 8, 9, 10, 12, 26, 35, 36, 37)],
            file = '../data/govt_geology.txt', sep = '|')
