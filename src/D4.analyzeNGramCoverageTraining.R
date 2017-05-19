## ---- analyze_ngram_coverage_training

#==============================================================================#
#                         analyzeNGramCoverageTraining                         #
#==============================================================================#
#'  analyzeNGramCoverageTraining
#' 
#' This function compares the training and validation ngrams, calculating
#' the degree to which the validation ngrams are covered in the training 
#' data.  The analysis is conducted over the series of four training sets
#' 
#' @param training - the meta data for the training data 
#' @param validation - the meta data for the validation set
#' @param directories - the project directory structure
#' @return trainingSummary - list containing the nGram counts and the 
#'                 OOV rates for each training corpora
#' @author John James
#' @export
analyzeNGramCoverageTraining <- function(training, validation, directories) {
  
  startTime <- Sys.time()
  message(paste('...Analyzing Training Set NGram Coverage at', startTime))
  
  coverage <- lapply(seq_along(validation$nGrams), function(n) {
    
    message(paste('\n...analyzing', validation$nGrams[[n]]$fileDesc))
    validationNGram <- loadObject(validation$nGrams[[n]])
    
    tSetCoverage <- rbindlist(lapply(seq_along(training), function(t) {
      message(paste('\n......loading', training[[t]]$corpusName))
      tSetDfm <- loadObject(training[[t]]$nGrams[[n]])
      tSetNGrams <- featnames(tSetDfm)
      
      message('......calculating OOV rates')
      oov <- dfm_select(validationNGram, features = tSetNGrams, 
                        selection = 'remove', valuetype = 'fixed')
      oovData <- list()
      oovData$tSet <- training[[t]]$corpusName
      oovData$nGram <- training[[t]]$nGrams[[n]]$fileDesc
      oovData$tSetNGrams <- ntoken(tSetDfm)
      oovData$validationNGrams <- ntoken(validationNGram)
      oovData$vOOV <- length(featnames(oov))
      oovData$nOOV <- ntoken(oov)
      oovData$vOOVRate <- length(featnames(oov)) / length(featnames(validationNGram))
      oovData$nOOVRate <- ntoken(oov) / ntoken(validationNGram)
      oovData
    }))
    tSetCoverage
  })  

  # Save Results
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('training-set-coverage')),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'trainingSetCoverage'
  output$data  <- coverage
  saveObject(output)
  
  # Log and Return results
  logR('analyzeNGramCoverageTraining', startTime, ' ', ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Training Set Coverage Analysis at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))

  return(coverage)
}
## ---- end
#coverage <- verifyVocabulary(testCorpus, )