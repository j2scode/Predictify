## ---- split_data
#==============================================================================#
#                              splitData                                       #
#==============================================================================#
#'  splitData
#' 
#' This function builds the training, validation, and test sets.
#' 
#' @param korpus - the meta data for the corpus to split 
#' @param analysis - the analysis of the corpus to split
#' @param training - meta data for the training set
#' @param validation - the meta data for the validation set
#' @param test - the meta data for the test set
#' @return cv - the cross validation set summary
#' @author John James
#' @export
splitData <- function(korpus, analysis, training, validation, test) {
  
  startTime <- Sys.time()
  
  message(paste("\nSplitting", korpus$corpusName," at ", startTime))
  
  # Designate splits
  splits = c(0.8, 0.1, 0.1)
  
  # Randomly allocate sentences for each register to training validation 
  # and test sets according to splits
  set.seed(1234)
  keys <- lapply(seq_along(korpus$documents), function(x) {
    k <- list()
    document <- readFile(korpus$documents[[x]])
    pool <- seq(1:length(document))
    k$train <- sample(pool, trunc(length(document) * splits[1]))
    pool <- pool[-k$train]
    k$val <- sample(pool, trunc(length(document) * splits[2]))
    pool <- pool[-k$val]
    k$test <- sample(pool, trunc(length(document) * splits[3]))
    k
  }) 
  
  # Splitting registers
  crossValidation <- list(
    training = training,
    validation = validation,
    test = test
  )
  lapply(seq_along(korpus$documents), function(r) {
    message(paste('...splitting', korpus$documents[[r]]$fileDesc))
    sents <- readFile(korpus$documents[[r]])
    lapply(seq_along(crossValidation), function(c) {
      samples <- sents[keys[[r]][[c]]]
      crossValidation[[c]]$documents[[r]]$data <- samples
      saveFile(crossValidation[[c]]$documents[[r]])
    })
  })
  
  # Summarize splits
  message(paste("\nSummarizing splits at ", startTime))
  summary <- rbindlist(lapply(seq_along(korpus$documents), function(x) {
    cv <- list()
    cv$register <- korpus$documents[[x]]$category
    
    # Get Total Sentences
    document <- readFile(korpus$documents[[x]])
    cv$totalSents <- as.numeric(length(document))
    words <- unlist(quanteda::tokenize(document, what = 'word'))
    cv$totalWords <- as.numeric(length(words))
    cv$totalTypes <- as.numeric(length(unique(words)))
    
    # Get Training Set Numbers
    message('...summarizing training data')
    document <- readFile(crossValidation[[1]]$documents[[x]])
    cv$trainSents <- as.numeric(length(document))
    words <- unlist(quanteda::tokenize(document, what = 'word'))
    cv$trainWords <- as.numeric(length(words))
    cv$trainTypes <- as.numeric(length(unique(words)))
    
    # Get Validation Set Numbers
    message('...summarizing validation data')
    document <- readFile(crossValidation[[2]]$documents[[x]])
    cv$validationSents <- as.numeric(length(document))
    words <- unlist(quanteda::tokenize(document, what = 'word'))
    cv$validationWords <- as.numeric(length(words))
    cv$validationTypes <- as.numeric(length(unique(words)))
    
    # Get Test Set Numbers
    message('...summarizing test data')
    document <- readFile(crossValidation[[3]]$documents[[x]])
    cv$testSents <- as.numeric(length(document))
    words <- unlist(quanteda::tokenize(document, what = 'word'))
    cv$testWords <- as.numeric(length(words))
    cv$testTypes <- as.numeric(length(unique(words)))
    cv
  }))
  
  #Format total line
  total <- list()
  total$register <- 'Corpus'
  total$totalSents <- sum(summary$totalSents)
  total$totalWords <- sum(summary$totalWords)
  total$totalTypes <- sum(summary$totalTypes)
  
  total$trainSents <- sum(summary$trainSents)
  total$trainWords <- sum(summary$trainWords)
  total$trainTypes <- sum(summary$trainTypes)
  
  total$validationSents <- sum(summary$validationSents)
  total$validationWords <- sum(summary$validationWords)
  total$validationTypes <- sum(summary$validationTypes)
  
  total$testSents <- sum(summary$testSents)
  total$testWords <- sum(summary$testWords)
  total$testTypes <- sum(summary$testTypes)
  
  # Add total line
  summary <- rbind(summary, as.data.frame(total))
  names(summary) <- c('Register', 'Total Sents', 'Total Words', 'Total Types',
                      'Training Sents', 'Training Words', 'Training Types',
                      'Validation Sents', 'Validation Words', 
                      'Validation Types', 'Test Sents', 'Test Words',
                      'Test Types')
  
  # Save summary
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('cross-validation-summary')),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'crossValidationSummary'
  output$data  <- summary
  saveObject(output)
  
  endTime <- Sys.time()
  message(paste('Corpus Splits Complete', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  # Log and Return results
  logR('splitData', startTime, 'crossValidation', ' ')
  
  return(summary)
  
}
## ---- end
#cv <- splitData(corpora$clean, analysiscleanCorpus, corpora$train, corpora$validation, corpora$test)