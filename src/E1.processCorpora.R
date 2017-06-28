## ---- process_corpora

#==============================================================================#
#                           processCorpus                                      #
#==============================================================================#
#'  processCorpus
#' 
#' This function creates multigram versions of a designated corpus.   
#' There are n-1 beginning of  sentence tokens and 
#' one end of sentence token per sentence, for each n-gram order.  Trigram and 
#' quadgram texts will be created for the validation and test sets.  These 
#' will be converted to document frequency matrices for modeling.
#' 
#' @param korpus - the meta data for korpus document to be annotated
#' @export
processCorpus <- function(korpus) {
  
  document <- readFile(korpus$processed[[1]])
  
  lapply(seq_along(korpus$processed), function(x) {
    message(paste('...creating', korpus$processed[[x]]$fileDesc, 'sentence boundaries'))
    annotatedDoc <- unlist(lapply(seq_along(document), function(s) {
      paste(paste0(rep("<s>", times = x-1), collapse = " "), document[s], "</s>", collapse = " ")
    }))
    korpus$processed[[x]]$data <- annotatedDoc
    saveFile(korpus$processed[[x]])
  })

}

#==============================================================================#
#                           processCorpora                                     #
#==============================================================================#
#'  processCorpora
#' 
#' This function processes the training, validation and test corpora by creating
#' unigram, bigram, trigram, quadgram, and quintgram versions of the text. Each
#' sentence is annotated with sentence boundary tokens according to the nGram
#' order.
#' 
#' @param training - the meta data for training corpora
#' @param validation - the meta data for validation corpora
#' @param test - the meta data for test corpora
#' @export
processCorpora <- function(training, validation, test) {
  
  startTime <- Sys.time()
  
  # Process training corpora
  lapply(seq_along(training), function(t) {
    
    message(paste('\nProcessing', training[[t]]$corpusName, 'at', Sys.time()))
    processCorpus(training[[t]])
  })
  
  # Process validation corpora
  lapply(seq_along(validation), function(t) {
    
    message(paste('\nProcessing', validation[[t]]$corpusName, 'at', Sys.time()))
    processCorpus(validation[[t]])
  })
  
  # Process test corpora
  lapply(seq_along(test), function(t) {
    
    message(paste('\nProcessing', test[[t]]$corpusName, 'at', Sys.time()))
    processCorpus(test[[t]])
  })
  
  # Log Results
  logR('processCorpus', startTime, ' ', 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpus processing completed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}

## ---- end