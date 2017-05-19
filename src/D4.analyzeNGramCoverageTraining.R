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
#' @param training - the meta data for the training data set
#' @param validation - the meta data for the validation set
#' @param directories - the project directory structure
#' @return trainingSummary - list containing the nGram counts and the 
#'                 OOV rates for each training corpora
#' @author John James
#' @export
analyzeNGramCoverageTraining <- function(training, validation, directories) {
  
  startTime <- Sys.time()
  message(paste('\n...Analyzing', training$corpusName, 'NGram Coverage at', startTime))
  
  message(paste('...loading', training$corpusName))
  trainingData <- unlist(lapply(seq_along(training$documents), function(d) {
    readFile(training$documents[[d]])
  }))
  
  message(paste('...loading', validation$corpusName))
  valData <- unlist(lapply(seq_along(validation$documents), function(d) {
    readFile(validation$documents[[d]])
  }))
  
  coverage <- rbindlist(lapply(seq_along(training$nGrams), function(n) {
    
    gc()
  
    message(paste('......tokenizing', training$corpusName, 
                  training$nGrams[[n]]$fileDesc))
    trainingNGrams <- quanteda::tokenize(trainingData, what = 'word', ngrams = n)

    message(paste('......tokenizing Validation Set', 
                  training$nGrams[[n]]$fileDesc))
    validationNGrams <- quanteda::tokenize(valData, what = 'word', ngrams = n)
    
    message('......extracting vocabularies')
    validationV <- unique(validationNGrams)
    trainingV <- unique(trainingNGrams)
    
    message('......calculating VOOV')
    vOOV <- length(validationV) - sum(validationV %in% trainingV)
    
    message('......calculating NOOV')
    nOOV <- length(validationNGrams) - sum(validationNGrams %in% trainingNGrams)
    
    message('......summarizing results')
    oovData <- list()
    oovData$tSet  <- training$corpusName
    oovData$vTraining <- length(trainingV)
    oovData$nTraining <- length(trainingNGrams)
    oovData$vValidation <- length(validationV)
    oovData$nValidation <- length(validationNGrams)
    oovData$nValidation <- length(trainingNGrams)
    oovData$vOOV <- vOOV
    oovData$nOOV <- nOOV
    oovData$vOOVRate <- vOOV / oovData$vValidation
    oovData$nOOVRate <- nOOV / oovData$nValidation
    oovData$coverage <- (1 - oovData$nOOVRate) * 100
    oovData
  }))  

  # Save Results
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('coverage-analysis-')),
                             paste0(training$fileName),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- paste0(training$objName, 'CoverageAnalysis')
  output$data  <- coverage
  saveObject(output)
  
  # Log and Return results
  logR(output$objName, startTime, ' ', ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste(training$corpusName, 'Coverage Analysis Completed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))

  return(coverage)
}
## ---- end
#coverage <- verifyVocabulary(testCorpus, )