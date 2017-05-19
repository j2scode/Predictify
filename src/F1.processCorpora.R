## ---- annotate_corpora

#==============================================================================#
#                           annotateCorpus                                     #
#==============================================================================#
#'  annotateCorpus
#' 
#' This function takes as its parameters, the preprocessed and processed 
#' corpora meta data, and a vector of n-gram orders for which the annotated
#' texts are being prepared. A processed text with beginning and end-of-
#' sentence markers is prepared for each ngram order.
#' 
#' @param inKorpus - the meta data for preprocess corpus
#' @param outKorpus - the meta data for the processed corpus
#' @param nGramOrders - a vector including 1 or more ngram orders
#' @export
annotateCorpus <- function(inKorpus, outKorpus, nGramOrders) {
  

  document <- readFile(inKorpus)
  
  lapply(seq_along(nGramOrders), function(x) {
    message(paste('......creating', outKorpus[[x]]$fileDesc, 'sentence boundaries'))
    annotatedDoc <- unlist(lapply(seq_along(document), function(s) {
      paste(paste0(rep("BOS", times = nGramOrders[x]-1), collapse = " "), document[s], "EOS", collapse = " ")
    }))
    outKorpus[[x]]$data <- annotatedDoc
    saveFile(outKorpus[[x]])
  })
  
}


#==============================================================================#
#                           processCorpora                                    #
#==============================================================================#
#'  processCorpora
#' 
#' This function takes as its parameters, the corpora meta data and annotated
#' texts for the training, validation, and test set. 
#' 
#' @param korpora - the meta data for project corpora
#' @export
processCorpora <- function(korpora) {
  
  startTime <- Sys.time()
  
  message(paste("\nAnnotating training set word n-gram text at", startTime))
  nGramOrders <- c(1,2,3,4)
  inKorpus <- korpora$training$preprocessed$document
  outKorpus <- korpora$training$processed$text
  annotateCorpus(inKorpus, outKorpus, nGramOrders)
  
  message(paste("\nAnnotating training set POS n-gram text at", startTime))
  nGramOrders <- c(1,2,3)
  inKorpus <- korpora$training$preprocessed$pos
  outKorpus <- korpora$training$processed$pos
  annotateCorpus(inKorpus, outKorpus, nGramOrders)
  
  message(paste("\nAnnotating validation set word n-gram text at", startTime))
  nGramOrders <- c(1,2,3,4)
  inKorpus <- korpora$validation$preprocessed$document
  outKorpus <- korpora$validation$processed$text
  annotateCorpus(inKorpus, outKorpus, nGramOrders)
  
  message(paste("\nAnnotating test set word n-gram text at", startTime))
  nGramOrders <- c(1,2,3,4)
  inKorpus <- korpora$test$preprocessed$document
  outKorpus <- korpora$test$processed$text
  annotateCorpus(inKorpus, outKorpus, nGramOrders)
  
  # Log Results
  logR('annotateCorpus', startTime, ' ', 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpus processing completed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
#processCorpora(corpora)