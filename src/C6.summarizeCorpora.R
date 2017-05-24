## ---- summarize_corpora
#------------------------------------------------------------------------------#
#                              summarizeCorpora                                #
#------------------------------------------------------------------------------#
#'  summarizeCorpora
#' 
#' This function provides summary statistics for the training, validation, and
#' test sets.
#' 
#' 
#' @param training - the meta data for the training corpus
#' @param validation - the meta data for the validation corpus
#' @param test - the meta data for the test corpus
#' @param directories - the project directory structure 
#' @author John James
#' @export
summarizeCorpora <- function(training, validation, test, directories) {
  
  startTime <- Sys.time()
  message(paste('\nSummarizing Model Corpora at', startTime))
  
  corpora <- list(
    alpha <- training$alpha,
    beta <- training$beta,
    gamma <- training $gamma,
    delta <- training$delta,
    validation <- validation,
    test <- test
  )
  
  corporaSummary <- rbindlist(lapply(seq_along(corpora), function(c) {
    message(paste('...summarizing', corpora[[c]]$corpusName))
    document <- unlist(lapply(seq_along(corpora[[c]]$documents), function(d) {
      readFile(corpora[[c]]$documents[[d]])
    }))
    
    stats <- list()
    stats$Corpus <- corpora[[c]]$corpusName
    stats$Sentences <- length(document)
    stats$Tokens <- sum(ntoken(document))
    stats$Types <- sum(ntype(document))
    stats
  }))
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'model-corpora-descriptive-statistics',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'corporaSummary'
  output$data  <- corporaSummary
  saveObject(output)
  
  # Log Results
  logR('summarizeCorpora', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Model corpora summary complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#splitData(analysis, featureDistributionAnalysis[[length(featureDistributionAnalysis)]]$size, corpora)