## ---- reshape_data

#==============================================================================#
#                              reshapeData                                     #
#==============================================================================#
#'  reshapeData
#' 
#' This function reshapes the raw data into sentences
#' 
#' @param rawCorpus - 'the meta data for the raw corpus
#' @param reshapedCorpus - the meta data for the reshaped corpus
#' @author John James
#' @export
reshapeData <- function(rawCorpus, reshapedCorpus) {
  
  startTime <- Sys.time()
  
  message(paste("\nReshaping", rawCorpus$corpusName, " at ",  startTime))
  
  # Reshape into sentence tokenized data
  message('...reshaping corpus into sentences')
  lapply(seq_along(rawCorpus$documents), function(x) {
    message(paste('......reshaping', rawCorpus$documents[[x]]$fileDesc))
    document <- readFile((rawCorpus$documents[[x]]))
    korpus <- parallelizeTask(quanteda::corpus, document)
    sents <- parallelizeTask(quanteda::tokenize, korpus, what = 'sentence')
    reshapedCorpus$documents[[x]]$data <- unlist(sents)
    saveFile(reshapedCorpus$documents[[x]])
  })
  
  # Log Results
  logR('reshapedData', startTime, rawCorpus$directory, ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Reshaping Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
 #reshapeData(corpora$raw, corpora$reshaped)