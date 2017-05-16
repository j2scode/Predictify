## ---- create_ngrams

#==============================================================================#
#                               createNGrams                                   #
#==============================================================================#
#'  createNGrams
#' 
#' This function prepares the document feature matrices used for language 
#' modeling
#' 
#' @param processed - the meta data for the processed word and POS data
#' @param nGrams - the meta data for the  nGrams
#' @param directories - the project directory structure
#' @return nGramSummary - data frame with counts for each n-gram (and type)
#' @author John James
#' @export
createNGrams <- function(processed, nGrams, directories) {
  
  startTime <- Sys.time()
  
  message(paste("\nPreparing nGrams at", startTime))
  
  message('...preparing WORD-based nGrams')
  nGramSummary <- rbindlist(lapply(seq_along(processed$text), function(x) {
    message(paste('......creating nGrams for', processed$text[[x]]$fileDesc))
    document <- readFile(processed$text[[x]])
    nGrams$text[[x]]$data <- dfm(document, ngrams = x, removePunct = FALSE, 
                                 concatenator = ' ', tolower = FALSE)
    saveObject(nGrams$text[[x]])
    s <- list()
    s$Type  <- 'Word'
    s$nGram <- nGrams$text[[x]]$fileDesc
    s$Count <- nfeature((nGrams$text[[x]]$data))
    s
  }))
  
  if ('training' == strsplit(processed$pos$unigram$fileName, '-')[[1]][2]) {
    message('...preparing POS-based nGrams')
    posNGramSummary <- rbindlist(lapply(seq_along(processed$pos), function(x) {
      message(paste('......creating nGrams for', processed$pos[[x]]$fileDesc))
      document <- readFile(processed$pos[[x]])
      nGrams$pos[[x]]$data <- dfm(document, ngrams = x, removePunct = FALSE, 
                                   concatenator = ' ', tolower = FALSE)
      saveObject(nGrams$pos[[x]])
      s <- list()
      s$Type <- 'POS'
      s$nGram <- nGrams$pos[[x]]$fileDesc
      s$Count <- nfeature((nGrams$pos[[x]]$data))
      s
    }))
    nGramSummary <- rbind(nGramSummary, posNGramSummary)
  }

  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'create-nGrams', 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'nGramSummary'
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

