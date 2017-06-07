## ---- create_ngrams

#==============================================================================#
#                               createNGrams                                   #
#==============================================================================#
#'  createNGrams
#' 
#' This function prepares the document feature matrices used for language 
#' modeling
#' 
#' @param korpora - the meta data for the processed word data
#' @param directories - the project directory structure
#' @return nGramSummary - data frame with counts for each n-gram (and type)
#' @author John James
#' @export
createNGrams <- function(korpora, directories) {
  
  startTime <- Sys.time()
  
  nGramSummary <- lapply(seq_along(korpora), function(k) {
    
    message(paste("\nPreparing nGrams for", korpora[[k]]$corpusName, 'at', Sys.time()))
  
    nGrams <- rbindlist(lapply(seq_along(korpora[[k]]$processed), function(x) {
      message(paste('...creating', korpora[[k]]$corpusName,
                    korpora[[k]]$processed[[x]]$fileDesc))
      document <- readFile(korpora[[k]]$processed[[x]])
      korpora[[k]]$nGrams[[x]]$data <- dfm(document, ngrams = x, remove_punct = FALSE, 
                                   concatenator = ' ', tolower = FALSE)
      saveObject(korpora[[k]]$nGrams[[x]])
      s <- list()
      s$Corpus <- korpora[[k]]$corpusName
      s$nGram <- korpora[[k]]$nGrams[[x]]$fileDesc
      s$Features <- nfeature(korpora[[k]]$nGrams[[x]]$data)
      s
    }))
    nGrams
  })
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('create-nGrams-')),
                             korpora$fileName, 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- paste0('nGramSummary', korpora$objName)
  output$data  <- nGramSummary
  saveObject(output)
  
  # Log and Return results
  logR('createNGrams', startTime, ' ', ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('nGrams completed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))

  return(nGramSummary)
}
## ---- end

