## ---- create_ngrams

#==============================================================================#
#                               createNGrams                                   #
#==============================================================================#
#'  createNGrams
#' 
#' This function prepares the document feature matrices used for language 
#' modeling
#' 
#' @param korpus - the meta data for the processed word and POS data
#' @param directories - the project directory structure
#' @return nGramSummary - data frame with counts for each n-gram (and type)
#' @author John James
#' @export
createNGrams <- function(korpus, directories) {
  
  startTime <- Sys.time()
  message(paste("\nPreparing nGrams for", korpus$corpusName, 'at', startTime))

  nGramSummary <- rbindlist(lapply(seq_along(korpus$processed$words), function(x) {
    message(paste('...creating', korpus$processed$words[[x]]$fileDesc))
    document <- readFile(korpus$processed$words[[x]])
    korpus$nGrams$words[[x]]$data <- dfm(document, ngrams = x, remove_punct = FALSE, 
                                 concatenator = ' ', tolower = FALSE)
    saveObject(korpus$nGrams$words[[x]])
    s <- list()
    s$Corpus <- korpus$corpusName
    s$nGram <- korpus$nGrams$words[[x]]$fileDesc
    s$Features <- nfeature(korpus$nGrams$words[[x]]$data)
    s
  }))
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('create-nGrams-')),
                             korpus$fileName, 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- paste0('nGramSummary', korpus$objName)
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

