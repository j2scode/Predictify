## ---- create_ngrams

#==============================================================================#
#                               createTestNGrams                               #
#==============================================================================#
#'  createTestNGrams
#' 
#' This function prepares the document feature matrices from the test corpus
#'  
#' @param directories - the project directory structure
#' @author John James
#' @export
createTestNGrams <- function(directories) {
  
  korpus <- lapply(seq(1:4), function(n) {
      nGram = list()
      nGram$directory <- file.path(directories$testingDir, 'mkn', 'corpus')
      nGram$objName  <- paste0('ltcorpus', n)
      nGram$fileName <- paste0('ltcorpus', n, '.txt')
      nGram 
  })
  
  nGrams <- lapply(seq(1:4), function(n) {
    nGram = list()
    nGram$directory <- file.path(directories$testingDir, 'mkn', 'nGrams')
    nGram$objName  <- paste0('ltcorpus', n)
    nGram$fileName <- paste0('ltcorpus', n, '.RData')
    nGram 
  })
  
  lapply(seq_along(korpus), function(k) {
    
      document <- readFile(korpus[[k]])
      nGrams[[k]]$data <- dfm(document, ngrams = k, remove_punct = FALSE, 
                                   concatenator = ' ', tolower = FALSE)
      saveObject(nGrams[[k]])
  })

}
## ---- end

createTestNGrams(directories)
