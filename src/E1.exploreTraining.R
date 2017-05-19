## ---- explore_training

#==============================================================================#
#                         exploreTraining                                      #
#==============================================================================#
#'  exploreTraining
#' 
#' This function creates the training set nGrams and checks the out of 
#' vocabulary (OOV) rates of a training corpora vis-a-vis a "test corpus".  
#' Words in the test corpus not found in the training corpora are "OOV"
#' 
#' @param training - the meta data for the training data 
#' @param validation - the meta data for the validation set
#' @param directories - the project directory structure
#' @return trainingSummary - list containing the nGram counts and the 
#'                 OOV rates for each training corpora
#' @author John James
#' @export
exploreTraining <- function(training, validation, directories) {
  
  startTime <- Sys.time()
  
  message(paste("\nVerify vocabulary coverage for training sets", startTime))
  coverage <- rbindlist(lapply(seq_along(training), function(t) {
    verifyVocabulary(validation, training[[t]], directories)
  }))
  
  # Save Results
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('training-data-analysis')),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'trainingDataAnalysis'
  output$data  <- coverage
  saveObject(output)
  
  # Log and Return results
  logR('verifyVocabulary', startTime, ' ', ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Training Data Analysis Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))

  return(coverage)
}
## ---- end
#coverage <- verifyVocabulary(testCorpus, )