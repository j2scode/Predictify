## ---- mkn_prune
#==============================================================================#
#                               mknPrune                                       #
#==============================================================================#
#'  mknPrune
#' 
#' This function prunes the language model by including only the ngrams, context
#' and suffixes found in the validation/test set.
#' 
#' @param mkn - the meta data for the language model
#' @param korpus - the meta data for the corpus and its N-grams
#' @param regex - regex parameters
#' @author John James
#' @export
mknPrune <- function(mkn, korpus, regex) {
  
  startTime <- Sys.time()
  
  message(paste("\nPruning model for", korpus$corpusName, "at", startTime))
  
  summary <- rbindlist(lapply(seq_along(korpus$processedData$nGrams$text), function(x) {
    message(paste('...pruning model for', korpus$processedData$nGrams$text[[x]]$fileDesc))

    # Get features from nGram and add the "UNK" nGram
    nGrams <- loadObject(korpus$processedData$nGrams$text[[x]])
    nGrams <- featnames(nGrams)
    unk <- paste(rep("UNK", x), collapse = ' ')
    nGrams <- c(nGrams, unk)
    counts <- loadObject(mkn$counts[[x]])
    counts <- counts[ nGram %in% nGrams]
    
    # Save counts
    mkn$counts[[x]]$data <- counts
    saveObject(mkn$counts[[x]])
    
    summary <- list(
      nGramOrder = x,
      count = nrow(counts)
    )
    summary
  }))
  
  

  # Log and Return results
  logR('mknPrune', startTime, mkn$model$unigrams$directory, ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Counts pruned at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(summary)

}
## ---- end