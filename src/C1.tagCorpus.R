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
    
    # Saving POS Data
    korpus$pos$tags[[r]]$data <- unlist(posData$tags)
    korpus$pos$pairs[[r]]$data <- unlist(posData$pairs)
    saveFile(korpus$pos$tags[[r]])
    saveFile(korpus$pos$pairs[[r]])
  })
  
  
  # Log Results
  logR('tagCorpus', startTime, ' ', 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste(korpus$corpusName, 'POS Tagging Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}