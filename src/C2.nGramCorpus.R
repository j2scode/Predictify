## ---- nGram_corpus

#==============================================================================#
#                               nGramCorpus                                    #
#==============================================================================#
#'  nGramCorpus
#' 
#' This function prepares unigrams, bigrams, trigrams, and quadgrams for a 
#' corpus.
#' 
#' @param korpus - the meta data for pilot corpus
#' @param directories - the project directory structure
#' @author John James
#' @export
nGramCorpus <- function(korpus, directories) {
  
  startTime <- Sys.time()
  
  message(paste('\nCreating nGrams for', korpus$corpusName, 'at', startTime))
  
  message('...loading corpus')
  document <- unlist(lapply(seq_along(korpus$documents), function(d) {
    unlist(readFile(korpus$documents[[d]]))
  }))
  
  
  nGramCounts <- lapply(seq_along(korpus$nGrams), function(n) {
    message(paste('...processing', korpus$nGrams[[n]]$fileName, 'for', korpus$corpusName))
    korpus$nGrams[[n]]$data <- dfm(document, ngrams = n, remove_punct = FALSE, 
                            concatenator = ' ', tolower = FALSE)
    saveObject(korpus$nGrams[[n]]) 
    length(featnames(korpus$nGrams[[n]]$data))
  })

  nGrams <- list()
  nGrams$corpus <- korpus$corpusName
  nGrams$unigrams <- nGramCounts[[1]]
  nGrams$bigrams <- nGramCounts[[2]]
  nGrams$trigrams <- nGramCounts[[3]]
  nGrams$quadgrams <- nGramCounts[[4]]
  
  # Save Results
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('nGrams-for-')),
                             korpus$fileName,
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'nGramCorpus'
  output$data  <- nGrams
  saveObject(output)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpus nGrams created at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(nGrams)
}
## ---- end