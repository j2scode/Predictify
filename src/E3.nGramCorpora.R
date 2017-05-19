## ---- ngram_corpora

#==============================================================================#
#                               nGramCorpora                                   #
#==============================================================================#
#'  nGramCorpora
#' 
#' Taking the corpora data structure, this function creates quanteda document
#' frequency matrix nGrams for the training, validation, and test corpora.
#' For the text data, unigram, bigram, trigram, and quadgrams are created.
#' Unigrams, bigrams, and trigrams are created for the POS data.
#' 
#' @param korpora - the meta data for the project corpora
#' @param directories - the project directory structure
#' @author John James
#' @export
nGramCorpora <- function(korpora, directories) {
  
  startTime <- Sys.time()
  
  message(paste("\nPreparing Corpora nGrams at", startTime))
  
  processed <- list(
    training = korpora$training$processed
  )
  
  nGrams <- list(
    training = korpora$training$nGrams
  )
  
  summary <- rbindlist(lapply(seq_along(processed), function(x) {
    createNGrams(processed[[x]], nGrams[[x]], directories)
  }))

  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'korpora-nGram-Frequencies',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'korporaNGramFrequencies'
  output$data  <- summary
  saveObject(output)
  
  # Log and Return results
  logR('nGramCorpora', startTime, ' ', ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpora nGramming complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(summary)
}
## ---- end
