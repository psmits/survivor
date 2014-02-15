# read in the statigraphic information
# sort it a lot to get formation level lith
source('../R/mung.r')
string <- 'Stratigraphic\ names\ Current.txt'

geo <- list.files(path = '../data/geology', pattern = string)

states <- list()

for(ii in seq(length(geo))) {
  states[[ii]] <- read.delim(paste('../data/geology/', geo[ii], sep = ''), sep = '|',
                             stringsAsFactors = FALSE)
}

geology <- Reduce(rbind, states)
geology <- geology[geology$Primary.Lithology.Group != '', ]

# match the stratigraphic names
forms <- unique(info$formation)
# exclude ""
forms <- forms[forms != '']

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

