## ---- condense_corpora
#==============================================================================#
#                            condenseCorpora                                   #
#==============================================================================#
#'  condenseCorpora
#' 
#' This function takes as its parameter, the korpus meta data, combines
#' the registers into a single document and saves document to disc 
#' 
#' @param korpora - the meta data for project corpora
#' @author John James
#' @export
condenseCorpora <- function(korpora) {
  
  startTime <- Sys.time()
  
  message(paste("\nCondensing corpora at", startTime))
  
  cv <- list(
    trainingSet = korpora$train,
    validationSet = korpora$validation,
    testSet = korpora$test
  )
  
  # Read and combine corpus files into a single document
  lapply(seq_along(cv), function(x) {
    message(paste('...condensing', cv[[x]]$corpusName))
    cv[[x]]$preprocessedData$text$data <- 
      unlist(lapply(seq_along(cv[[x]]$documents), function(d) {
        readFile(cv[[x]]$documents[[d]])
      }))
    saveFile(cv[[x]]$preprocessedData$text)
  })
  
  # Log and Return results
  logR('condenseCorpus', startTime, ' ', ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpora condensing complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#condenseCorpora(corpora)