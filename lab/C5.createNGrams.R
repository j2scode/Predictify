## ---- create_ngrams
#==============================================================================#
#                               createNGrams                                   #
#==============================================================================#
#'  createNGrams
#' 
#' This function prepares the document feature matrices used for plotting 
#' and language modeling
#' 
#' @param metaData - the meta data for the preprocessed or processed data
#' @param nGrams - the meta data for the nGrams
#' @param source - c('preprocessed', 'processed')
#' @return ngramData - the nGram data
#' @author John James
#' @export
createNGrams <- function(metaData, nGrams, source = 'preprocessed') {
  
  startTime <- Sys.time()
  
  message(paste("\nPreparing nGrams at", startTime))
  
  if (source == 'preprocessed') {
    document <- readFile(metaData)
    nGramData <- lapply(seq_along(nGrams), function(x) {
      message(paste('...creating', nGrams[[x]]$fileDesc))
      nGrams[[x]]$data <- dfm(document, ngrams = x, remove_punct = FALSE, 
                              concatenator = ' ', tolower = FALSE)
      saveObject(nGrams[[x]])
    })
  } else {
    nGramData <- lapply(seq_along(metaData), function(x) {
      message(paste('...creating', nGrams[[x]]$fileDesc))
      document <- readFile(metaData[[x]])
      nGrams[[x]]$data <- dfm(document, ngrams = x, remove_punct = FALSE, 
                              concatenator = ' ', tolower = FALSE)
      saveObject(nGrams[[x]])
    })
  }

  # Log and Return results
  logR('createNGrams', startTime, ' ', ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('nGrams completed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))

}
## ---- end