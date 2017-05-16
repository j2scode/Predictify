r <- list(
  d = 'data/download/HC-Corpus.zip'
)
d <- 'data/download/HC-Corpus.zip'
download.file(schema$corpora$raw$source$url, 
              destfile = schema$corpora$raw$source$downloadPath, mode = 'wb')