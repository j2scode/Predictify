## ---- mkn_pipeline

#==============================================================================#
#                                mknPipeline                                   #
#==============================================================================#
#'  mknPipeline
#' 
#' This function takes as its parameter, the processed data N-grams, the 
#' meta data for the MKN language model and the regex parameters, and runs
#' the pipeline to train the MKN model, evaluate the model on the test set
#' and return its perplexity. 
#' 
#' @param nGrams - the meta data for the processed training set ngrams
#' @param mkn - the meta data for the MKN language model
#' @param regex - the regex parameters
#' @return perplexity - the perplexity of the test set
#' @author John James
#' @export
mknPipeline <- function(nGrams, mkn, regex) {
  
  startTime <- Sys.time()
  
  message(paste("Starting MKN pipeline at", startTime))


  mknInit(mkn, corpora$training$nGrams$text, regex)
  features <- mknAbsCount(mkn, nGrams)
  mknCKN(mkn)
  mknHistories(mkn)
  discounts <- mknDiscount(mkn)
  mknNorm(mkn)
  mknAlpha(mkn)
  mknLambda(mkn)

  # reports <- list(
  #   unpruned = unpruned,
  #   pruned = pruned,
  #   tokens = tokens
  # )
  
  
  # Log Results
  logR('mknPipeline', startTime, ' ', 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('MKN Pipeline Complete at ', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  # return(reports)
}
## ---- end
#report <- mknPipeline(corpora$train$processedData$nGrams$text, lm$mkn, regexPatterns)