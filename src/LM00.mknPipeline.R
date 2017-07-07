## ---- mkn_pipeline

#------------------------------------------------------------------------------#
#                               mknPipeline                                    #
#------------------------------------------------------------------------------#
#'  mknPipeline  
#' 
#' This function executes the pipeline for the Modified Kneser-Ney Trigram
#' and Quadgram Models.  It processes training corpora of various sizes and 
#' reports the perplexity of the designated test set. 
#' 
#' @param training - the meta data for the training set being mkned
#' @param test - the meta data for the test set 
#' @param mkn - the meta data for the mkn
#' @param regex - the regex patterns
#' @param directories - the project directory structure
#' @author John James
#' @export
mknPipeline <- function(training, test, mkn, regex, directories) {
  
  startTime <- Sys.time()
  message(paste('\nExecuting', mkn$mDesc, 'on',
                training$corpusName, 'at', startTime))
  
  # Get Test NGrams
  #createTestNGrams(directories)
  
  # Initialize MKN language mkn
  mkn <- mknInit(mkn, training, regex)
  
  # Create absolute counts of each nGram
  features <- parallelizeTask(mknAbsCount, mkn, training$nGrams)
  
  # Create continuation counts of each nGram
  parallelizeTask(mknCKN, mkn, mkn$mOrder)
  
  # Count nGram histories
  parallelizeTask(mknHistories, mkn, mkn$mOrder)
  
  # Calculate discounts
  discounts <- mknDiscount(mkn)
  
  # Calculate pseudo probability alpha
  parallelizeTask(mknAlpha, mkn)
  
  # Compute weighting factor lambda
  parallelizeTask(mknLambda, mkn)
  
  # Compute probabilities 
  parallelizeTask(mknEstimate, mkn)
  
  # Extract lm
  #extractMkn(mkn)
  
  # Publish language mkn
  parallelizeTask(mknPublish, mkn, directories)
  
  # Evaluate Model
  pp <- mknEvaluate(mkn, test, sents = 1000, directories = directories)
  
  # Log Results
  logR('mknPipeline', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('MKN Pipeline Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end

