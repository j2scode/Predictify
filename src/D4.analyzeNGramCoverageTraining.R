## ---- analyze_ngram_coverage_training

#==============================================================================#
#                         analyzeNGramCoverageTraining                         #
#==============================================================================#
#'  analyzeNGramCoverageTraining
#' 
#' This function compares provides coverage analysis of the training ngrams
#' vis-a-vis the validation and test nGrams. It calculates the degree to which 
#' the validation and test ngrams are covered in the training  data.  
#' 
#' @param training - the meta data for the training data set
#' @param validation - the meta data for the validation set
#' @param test - the meta data for the validation set
#' @param directories - the project directory structure
#' @return coverage - list containing the nGram counts and the 
#'                 OOV rates for each training corpora, vis-a-vis validation
#'                 and test data
#' @author John James
#' @export
analyzeNGramCoverageTraining <- function(training, validation, test, directories) {
  
  startTime <- Sys.time()
  message(paste('\nAnalyzing', training$corpusName, 'NGram Coverage at', startTime))
  
  message(paste('...loading', training$corpusName))
  trainingData <- unlist(lapply(seq_along(training$documents), function(d) {
    readFile(training$documents[[d]])
  }))
  
  message(paste('...loading', validation$corpusName))
  valData <- unlist(lapply(seq_along(validation$documents), function(d) {
    readFile(validation$documents[[d]])
  }))

  message(paste('...loading', test$corpusName))
  testData <- unlist(lapply(seq_along(test$documents), function(d) {
    readFile(test$documents[[d]])
  }))
  
  coverage <- rbindlist(lapply(seq_along(training$nGrams$words), function(n) {
    
    gc()
  
    message(paste('\n......tokenizing', training$corpusName, 
                  training$nGrams$words[[n]]$fileDesc))
    trainingNGrams <- unlist(quanteda::tokenize(trainingData, 
                                                what = 'word', ngrams = n,
                                                concatenator = ' '))

    message(paste('......tokenizing Validation Set', 
                  training$nGrams$words[[n]]$fileDesc))
    validationNGrams <- unlist(quanteda::tokenize(valData, what = 'word',
                                                  ngrams = n,
                                                  concatenator = ' '))
    
    message(paste('......tokenizing Test Set', 
                  training$nGrams$words[[n]]$fileDesc))
    testNGrams <- unlist(quanteda::tokenize(testData, what = 'word', 
                                            ngrams = n,
                                            concatenator = ' '))
    
    message('......extracting vocabularies')
    trainingV <- unique(trainingNGrams)
    validationV <- unique(validationNGrams)
    testV <- unique(testNGrams)

    message('......calculating VOOV')
    valVoov <- length(validationV) - sum(validationV %in% trainingV)
    testVoov <- length(testV) - sum(testV %in% trainingV)
    
    message('......calculating NOOV')
    valNoov <- length(validationNGrams) - sum(validationNGrams %in% trainingNGrams)
    testNoov <- length(testNGrams) - sum(testNGrams %in% trainingNGrams)
    
    message('......summarizing results')
    oovData <- list()
    oovData$tset  <- training$corpusName
    oovData$sents <- length(trainingData)
    oovData$words <- sum(ntoken(trainingData))
    oovData$nGram <- training$nGrams$words[[n]]$fileDesc
    oovData$trainingNGramTypes <- length(trainingV)
    oovData$trainingNGrams <- length(trainingNGrams)
    
    oovData$valNGrams <- length(validationNGrams)
    oovData$valnGramsOOV <- valNoov
    oovData$valNGramsOOVRate <- valNoov / oovData$valNGrams
    oovData$valCoverage <- (1 - oovData$valNGramsOOVRate) * 100

    oovData$testNGrams <- length(testNGrams)
    oovData$testnGramsOOV <- testNoov
    oovData$testNGramsOOVRate <- testNoov / oovData$testNGrams
    oovData$testCoverage <- (1 - oovData$testNGramsOOVRate) * 100
    
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