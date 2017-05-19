## ---- tag_corpus

#==============================================================================#
#                                tagCorpus                                     #
#==============================================================================#
#'  tagCorpus
#' 
#' This function performs POS tagging of a corpus.
#' 
#' @param korpus - meta data for the corpus to be tagged
#' @author John James
#' @export
tagCorpus <- function(korpus) {
  
  startTime <- Sys.time()
  
  message(paste('\nTagging', korpus$corpusName, 'at', startTime))

  lapply(seq_along(korpus$documents), function(r) {
    korpus$documents[[r]]$data <- unlist(readFile(korpus$documents[[r]]))
    posData <- tagDocument(korpus$documents[[r]])
    korpus$pos[[r]]$data <- unlist(posData$tags)
    
    saveFile(korpus$pos[[r]])
  })
  
  
  # Log Results
  logR('tagCorpus', startTime, ' ', 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste(korpus$corpusName, 'POS Tagging Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}