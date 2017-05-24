## ---- process_corpus

#==============================================================================#
#                           processCorpus                                      #
#==============================================================================#
#'  processCorpus
#' 
#' This function creates multigram versions of the training, validation, 
#' and test set word and pos data. The function annotates each sentence with 
#' sentence boundary tokens.  There are n-1 beginning of  sentence tokens and 
#' one end of sentence token per sentence, for each n-gram order.  Trigram and 
#' quadgram texts will be created for the validation and test sets.  These 
#' will be converted to document frequency matrices for modeling.
#' 
#' @param korpus - the meta data for korpus document to be annotated
#' @export
processCorpus <- function(korpus) {
  
  startTime <- Sys.time()
  message(paste('\nProcessing', korpus$corpusName))
  document <- readFile(korpus$processed$words[[1]])
  
  lapply(seq_along(korpus$processed$words), function(x) {
    message(paste('......creating', korpus$processed$words[[x]]$fileDesc, 'sentence boundaries'))
    annotatedDoc <- unlist(lapply(seq_along(document), function(s) {
      paste(paste0(rep("BOS", times = x-1), collapse = " "), document[s], "EOS", collapse = " ")
    }))
    korpus$processed$words[[x]]$data <- annotatedDoc
    saveFile(korpus$processed$words[[x]])
  })
  
  
  # Log Results
  logR('processCorpus', startTime, ' ', 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpus processing completed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
#processCorpora(corpora)