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
#' @param training - the meta data for the training set being modeled
#' @param test - the meta data for the test set 
#' @param model - the meta data for the model
#' @param regex - the regex patterns
#' @param directories - the project directory structure
#' @author John James
#' @export
mknPipeline <- function(training, test, model, regex, directories) {
  
  startTime <- Sys.time()
  message(paste('\nExecuting', model$mDesc, 'on',
                training$corpusName, 'at', startTime))
  
  gc()
  
  # Initialize MKN language model
  mknInit(model, training$nGrams, regex)
  
  # Create absolute counts of each nGram
  features <- parallelizeTask(mknAbsCount, model, training$nGrams)
  
  # Create continuation counts of each nGram
  parallelizeTask(mknCKN, model, model$mOrder)
  
  # Count nGram histories
  parallelizeTask(mknHistories, model, model$mOrder)
  
  # Calculate discounts
  discounts <- mknDiscount(model)
  
  # Calculate pseudo probability alpha
  parallelizeTask(mknAlpha, model)
  
  # Compute weighting factor lambda
  parallelizeTask(mknLambda, model)

  # Compute probabilities 
  parallelizeTask(mknEstimate, model)
  
  # Publish language model
  parallelizeTask(mknPublish, model, directories)
  
  # Log Results
  logR('mknPipeline', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('MKN Pipeline Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
