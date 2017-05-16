## ---- split_corpora
#------------------------------------------------------------------------------#
#                              splitData                                       #
#------------------------------------------------------------------------------#
#'  splitData 
#' 
#' This function takes as its parameters, the clean corpus, the clean corpus 
#' analysis, and the directory structure. It splits the document into chunks 
#' of sentences, then randomly allocates each chunk of sentences to the 
#' training, validation and test sets  which receive 80%, 20%, and 20% of the 
#' chunks respectively.
#' 
#' @param clean - the meta data for clean corpus
#' @param analysis - the clean corpus analysis
#' @param directories - the project directory structure
#' @author John James
#' @export
splitData <- function(clean, analysis, directories) {
  
  startTime <- Sys.time()
  message(paste('\nSplitting Clean Corpus at', startTime))
  
  # Designate training, validation, and test set directories
  training <- list(
    directory = file.path(directories$trainingCorpora, trainingData$trainingSets[[1]]$name, 
                        trainingData$setType[[1]], trainingData$setDocuments[[1]]),
    fileName = character(0)
  )
  validation <- list(
    directory = file.path(directories$validationCorpus),
    fileName = character(0)
  )
  test <- list(
    directory = file.path(directories$testCorpus),
    fileName = character(0)
  )
  
  # Process each document in the corpus
  lapply(seq_along(clean$documents), function(d) {
    message(paste('...processing', clean$documents[[d]]$fileDesc))
    document <- readFile(clean$documents[[d]])

    # Convert sampling units (2000 tokens) into chunkSize in sentences
    chunkSize <- floor(2000 / analysis$featureMatrix$wordsPerSent[d])
    
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
    
    message('......designate register file names')
    training$fileName <- registers[[d]]$fileName
    validation$fileName <- registers[[d]]$fileName
    test$fileName <- registers[[d]]$fileName
    
    message('......designate training, validation, and test data')
    training$data <- unlist(chunks[idsTraining])
    validation$data <- unlist(chunks[idsValidation])
    test$data <- unlist(chunks[idsTest])

    # Save Documents
    message('......saving documents')
    saveFile(training)
    saveFile(validation)
    saveFile(test)
  })
  

  # Log Results
  logR('splitData', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpus split at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#splitData(corpora$clean, analysis, directories)