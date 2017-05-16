## ---- split_corpora
#------------------------------------------------------------------------------#
#                              splitData                                    #
#------------------------------------------------------------------------------#
#'  splitData 
#' 
#' This function takes as its parameters, the clean data analysis, the 
#' sampling unit and the corpora meta data.  For each register, it splits
#' the document into chunks of sentences, then randomly allocates each 
#' chunk of sentences to the training, validation and test sets which receive
#' 80%, 20%, and 20% of the chunks respectively.
#' 
#' @param analysis - the clean data analysis
#' @param samplingUnit - the number of tokens in each sampling unit
#' @param korpora - the meta data for the all project corpora
#' @author John James
#' @export
splitData <- function(analysis, samplingUnit, korpora) {
  
  startTime <- Sys.time()
  message(paste('\nSplitting Corpora at', startTime))
  
  # Create shortcut variables
  clean <- korpora$clean
  training <- korpora$training
  validation <- korpora$validation
  test <- korpora$test
  
  # Process each document in the corpus
  lapply(seq_along(clean$documents), function(d) {
    message(paste('...processing', clean$documents[[d]]$fileDesc))
    document <- readFile(clean$documents[[d]])

    # Convert sampling units in tokens to chunkSizes in sentences
    chunkSize <- floor(samplingUnit / analysis$featureMatrix$wordsPerSent[d])
    
    message('......splitting document into chunks of sentences')
    numChunks <- floor(length(document) / chunkSize)
    chunks <- sampleData(document, numChunks, chunkSize, format = 'lv')

    # Designate blocks of sentences to training, validation, and test sets
    set.seed(0505)
    pool <- 1:length(chunks)
    idsTraining <- sample(pool,floor(length(chunks) * .8))
    pool <- pool[-idsTraining]
    idsValidation <- sample(pool,floor(length(pool) / 2))
    pool <- pool[-idsValidation]
    idsTest <- sample(pool,length(idsValidation))
    
    message('......allocating samples of sentence chunks to cross validation files')
    training$clean$documents[[d]]$data <- unlist(chunks[idsTraining])
    validation$clean$documents[[d]]$data <- unlist(chunks[idsValidation])
    test$clean$documents[[d]]$data <- unlist(chunks[idsTest])

    # Save Documents
    message('......saving documents')
    saveFile(training$clean$documents[[d]])
    saveFile(validation$clean$documents[[d]])
    saveFile(test$clean$documents[[d]])
  })
  

  # Log Results
  logR('splitData', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpus split at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#splitData(analysis, featureDistributionAnalysis[[length(featureDistributionAnalysis)]]$size, corpora)