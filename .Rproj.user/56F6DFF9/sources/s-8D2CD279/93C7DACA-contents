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
#' @author John James
#' @export
reshapeData <- function(raw, reshaped) {
  
  startTime <- Sys.time()
  
  message(paste("\nReshaping data at",  startTime))
  
  # Reshape into sentence tokenized data
  lapply(seq_along(raw$documents), function(x) {
    
    message(paste('...loading', raw$documents[[x]]$fileName))
    document <- readFile(raw$documents[[x]])
    
    message(paste('...reshaping', raw$documents[[x]]$fileName))
    korpus <- parallelizeTask(quanteda::corpus, document)
    sents <- parallelizeTask(quanteda::tokenize, korpus, what = 'sentence')
    
    reshaped$documents[[x]]$data <- unlist(sents)
    saveFile(reshaped$documents[[x]])
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