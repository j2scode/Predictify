## ---- analyze_nGram_coverage

#==============================================================================#
#                           analyzeNGramCoverage                               #
#==============================================================================#
#'  analyzeNGramCoverage
#' 
#' This function takes as its parameters, the meta data for the nGrams to be 
#' analyzed and the directory structure, then creates a frequency spectrum
#' data frame.
#' 
#' 
#' @param nGrams - meta data for the nGrams being analyzed
#' @param directories - the project directory structure
#' @return nGramCoverage - a list containing data frames with NGram coverage 
#'                         data
#' @author John James
#' @export
analyzeNGramCoverage <- function(nGrams, directories) {
  
  startTime <- Sys.time()
  message(paste('\nAnalyzing nGram Coverage at', startTime))  
   
  coverage <- lapply(seq_along(nGrams), function(n) {
    dfm <- loadObject(nGrams[[n]])
    nTokens <- sum(unlist(ntoken(dfm)))
    nFeatures <- dfm@Dim[2]
    tf <- topfeatures(dfm, n = dfm@Dim[2])
    data.table(Rank = 1:nFeatures, nGram = names(tf), Freq = unname(tf))[, Coverage := cumsum(Freq) / nTokens * 100]
  })
  
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'nGram-coverage',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'nGramCoverage'
  output$data  <- coverage
  saveObject(output)
  
  # Log Results
  logR('analyzeNGramCoverage', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('nGram Coverage Analyzed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  
  return(coverage)
}
## ---- end

