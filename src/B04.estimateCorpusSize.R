## ---- estimate_corpus_size
#------------------------------------------------------------------------------#
#                            estimateCorpusSize                                #
#------------------------------------------------------------------------------#
#'  estimateCorpusSize 
#' 
#' This function takes the meta data for the corpus, the meta data for the 
#' registers, the posTags and the directory structure and provides an 
#' estimate of total corpus size based upon the distribution of lexical 
#' features per n000-word samples of the text.
#' 
#' @param korpus - the meta data for the corpus
#' @param registers - the meta data for the registers
#' @param posTags - selected POS tags
#' @param directories - the project directory structure
#' @return corpusSize - the corpus size estimate with summary tables
#' @author John James
#' @export
estimateCorpusSize <- function(korpus, registers, posTags, directories) {
  
  startTime <- Sys.time()
  
  message(paste('\nEstimating total corpus size at', startTime))
  
  # Initialize key variables
  sampleSize <- 2000
  numSamples <- 100
  posTags <- subset(posTags, Study == TRUE)
  
  # Read and combine corpus into single document
  message('...loading file')
  filePath <- list()
  filePath$directory <- korpus$directory
  document <- unlist(lapply(seq_along(registers), function(x) {
    filePath$fileName <- registers[[x]]$fileName
    readFile(filePath)
  }))
  
  # Convert to word tokens
  message('...converting document to word tokens')
  document <- unlist(quanteda::tokenize(document, what = 'word'))
  
  # Split data into chunks
  message(paste0('...sampling ', numSamples, ', ', sampleSize, '-word samples'))
  chunks <- sampleData(document, numChunks = numSamples, 
                           chunkSize = sampleSize, format = 'lv')

  # Prepare tag analysis
  message('...conducting POS analysis')
  posAnalysis <- analyzeLexicalFeatures(chunks, posTags)

  # Corpus size estimate is the largest n not including rare tags (< 1%) of total tags
  wordsTagged <- sum(posAnalysis$featureStats$total) 
  n  <- round(max(subset(posAnalysis$featureStats, select = n)$n), 0)
  
  # Format result
  corpusSize <- list(
    n = n,
    analysis = posAnalysis$featureStats
  )
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'corpus-sample-size-estimate',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'corpusSampleSize'
  output$data  <- corpusSize
  saveObject(output)
  
  # Log Results
  logR('corpusSampleSize', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpus sample size estimated at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(corpusSize)
}
## ---- end
#css <- estimateCorpusSize(corpora$clean, posTags, directories)