## ---- reshape_data

#==============================================================================#
#                              reshapeData                                     #
#==============================================================================#
#'  reshapeData
#' 
#' This function reshapes the raw data into sentences
#' 
#' @param raw - meta data for the raw corpus
#' @param reshaped - meta data for the reshaped corpus
#' @param registers - meta data for the corpus registers
#' @author John James
#' @export
reshapeData <- function(raw, reshaped, registers) {
  
  startTime <- Sys.time()
  
  message(paste("\nReshaping data at",  startTime))
  
  # Reshape into sentence tokenized data
  lapply(seq_along(registers), function(x) {
    
    message(paste('...loading', registers[[x]]$fileDesc))
    rawData <- list()
    rawData$directory <- raw$directory
    rawData$fileName <- registers[[x]]$fileName
    document <- readFile(rawData)
    
    message(paste('...reshaping', registers[[x]]$fileDesc))
    korpus <- parallelizeTask(quanteda::corpus, document)
    sents <- parallelizeTask(quanteda::tokenize, korpus, what = 'sentence')
    
    reshapedData <- list()
    reshapedData$directory <- reshaped$directory
    reshapedData$fileName <- registers[[x]]$fileName
    reshapedData$data <- unlist(sents)
    saveFile(reshapedData)
  })
  
  # Log Results
  logR('reshapedData', startTime, reshaped$directory, ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Reshaping Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
 #reshapeData(corpora$raw, corpora$reshaped)