## ---- reshape_data

#==============================================================================#
#                              reshapeData                                     #
#==============================================================================#
#'  reshapeData
#' 
#' This function reshapes the raw data into sentences
#' 
#' @param docSource - indicates whether to use production or development data
#' @param corpora - the corpora meta data
#' @author John James
#' @export
reshapeData <- function(docSource, corpora) {
  
  startTime <- Sys.time()
  
  message(paste("\nReshaping data at",  startTime))
  
  if (docSource == 1) {
    documents <- corpora$raw$documents
  } else {
    documents <- corpora$development$documents
  }
  
  # Reshape into sentence tokenized data
  message('...reshaping corpus into sentences')
  lapply(seq_along(documents), function(x) {
    message(paste('......reshaping', documents[[x]]$fileDesc))
    document <- readFile((documents[[x]]))
    korpus <- parallelizeTask(quanteda::corpus, document)
    sents <- parallelizeTask(quanteda::tokenize, korpus, what = 'sentence')
    corpora$reshaped$documents[[x]]$data <- unlist(sents)
    saveFile(corpora$reshaped$documents[[x]])
  })
  
  # Log Results
  logR('reshapedData', startTime, documents[[1]]$directory, ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Reshaping Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
 #reshapeData(corpora$raw, corpora$reshaped)