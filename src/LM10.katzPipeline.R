## ---- katz_pipeline

#------------------------------------------------------------------------------#
#                               katzPipeline                                   #
#------------------------------------------------------------------------------#
#'  katzPipeline  
#' 
#' This function executes the pipeline for the katz-backoff quadgram model.
#' 
#' 
#' @param training - the meta data for the training set being modeled
#' @param test - the meta data for the test set 
#' @param model - the meta data for the model
#' @param regex - the regex patterns
#' @param directories - the project directory structure
#' @author John James
#' @export
katzPipeline <- function(training, test, model, regex, directories) {
  
  startTime <- Sys.time()
  message(paste('\nExecuting', model$mDesc, 'on',
                training$corpusName, 'at', startTime))
  
  gc()
  
  # Initialize Katz Tables
  katzInit(model, training$nGrams, regex)
  
  # Calculate absolute counts
  summaryCounts <- katzAbsCount(model, training$nGrams)
  
  # Calculate Discounts
  katzDiscount(model)

  # Log Results
  logR('mknPipeline', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('MKN Pipeline Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
