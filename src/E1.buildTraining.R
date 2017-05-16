## ---- build_training

#------------------------------------------------------------------------------#
#                              buildTraining                                   #
#------------------------------------------------------------------------------#
#'  buildTraining  
#' 
#' This function takes as its parameters, the corpus design, the meta data for 
#' the clean and training corpora, the analysis of the clean data and
#' a percent sample size, then creates a training corpus by performing a 
#' stratified, proportional random sampling of the clean training corpus, 
#' according to the percent size parameter and the proportional allocation 
#' of registers from the corpus design. The size parameter is increased by
#' 25% since the sampling population is 80% of the corpus size.
#' 
#' @param design - the corpus design
#' @param clean - the clean corpus meta data
#' @param training - the training set meta data
#' @param analysis - the clean corpus analysis
#' @param size- the sample size in terms of percent of the original corpus size
#' @param samplingUnit - the sampling unit size from the lexical
#'                       feature distribution analysis
#' @author John James
#' @export
buildTraining  <- function(design, clean, training, analysis, size, samplingUnit) {
  
  startTime <- Sys.time()
  message(paste('\nBuilding new training corpus at', startTime))
  
  # Calculate total sample size in terms of tokens
  totalSampleSizeTokens <- round(analysis$featureMatrix$tokens[4] * size * 1.25 / 100,0)
  
  # Determine the proportional allocation of registers from the corpus design
  proportions <- analysis$featureMatrix$tokens[1:3] / 
    analysis$featureMatrix$tokens[4]
  proportionalSizeTokens <- totalSampleSizeTokens * proportions
  
  # Convert proportional allocation from tokens to sentences.
  proportionalSizeSents  <- proportionalSizeTokens / analysis$featureMatrix$wordsPerSent[1:3]
  
  # Designate sampling chunk size and number of chunks
  chunkSizes <- samplingUnit / analysis$featureMatrix$wordsPerSent[1:3]
  numChunks <- floor(proportionalSizeSents / chunkSizes)
  
  # Extract samples from each register and store as clean training corpus.
  lapply(seq_along(clean$documents), function(d) {
    message(paste('...processing', clean$documents[[d]]$fileDesc))
    document <- readFile(clean$documents[[d]])
    
    # Randomly sample document into sentence chunks
    training$clean$documents[[d]]$data <- sampleData(document, numChunks[d], 
                                                     chunkSizes[d], format = 'v')
    # Save Documents
    saveFile(training$clean$documents[[d]])
  })  

  # Log Results
  logR('buildTraining', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Training corpus complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
