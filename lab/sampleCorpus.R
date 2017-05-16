source('./config/initEnvironment.R')

lapply(seq_along(corpora$raw$documents), function(x) {
  document <- readFile(corpora$raw$documents[[x]])
  ids <- sample(length(document), .075*length(document))
  corpora$development$documents[[x]]$data <- document[ids]
  saveFile(corpora$development$documents[[x]])
})
