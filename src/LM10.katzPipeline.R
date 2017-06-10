## ---- katz_pipeline

#------------------------------------------------------------------------------#
#                               katzPipeline                                   #
#------------------------------------------------------------------------------#
#'  katzPipeline  
#' 
#' This function executes the pipeline for the katz-backoff quadgram katz.
#'  
#' @param training - the meta data for the training set being katzed
#' @param test - the meta data for the test set 
#' @param katz - the meta data for the katz language model
#' @param regex - the regex patterns
#' @param directories - the project directory structure
#' @author John James
#' @export
katzPipeline <- function(training, test, katz, regex, directories) {
  
  startTime <- Sys.time()
  message(paste('\nExecuting', katz$mDesc, 'on',
                training$corpusName, 'at', startTime))
  
  # Meta data for test corpus and ngrams
  korpus <- lapply(seq(1:4), function(n) {
    nGram = list()
    nGram$directory <- file.path(directories$testingDir, 'katz', 'corpus')
    nGram$objName  <- paste0('ltcorpus', n)
    nGram$fileName <- paste0('ltcorpus', n, '.txt')
    nGram 
  })
  
  nGrams <- lapply(seq(1:4), function(n) {
    nGram = list()
    nGram$directory <- file.path(directories$testingDir, 'katz', 'nGrams')
    nGram$objName  <- paste0('ltcorpus', n)
    nGram$fileName <- paste0('ltcorpus', n, '.RData')
    nGram 
  })
  
  test <- list(
    directory = file.path(directories$testingDir, 'katz', 'test'),
    objName = 'katzTestData',
    fileName = 'test.txt'
  )
  
  # Initialize Katz Tables
  katzInit(katz, nGrams, regex)
  
  # Calculate absolute counts
  #summaryCounts <- katzAbsCount(katz, training$nGrams)
  summaryCounts <- katzAbsCount(katz, nGrams)
  
  # Calculate Adjusted Counts
  summaryCounts <- katzAdjCount(katz, discount = 0.5, directories)
  
  # Calculate alpha weights
  katzAlpha(katz)
  
  # Prune and publish language model
  modelSize <- katzPrune(katz, directories)
  
  # Evaluate language model
  evaluation <- katzEvaluate(katz, training, test, sents = NULL, directories)
  
  
  # Log Results
  logR('katzPipeline', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Katz Pipeline Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
