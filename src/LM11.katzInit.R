## ---- katz_init

#==============================================================================#
#                                katzInit                                      #
#==============================================================================#
#'  katzInit
#' 
#' This function initializes the katz unigram, bigram, trigram,and quadgram
#' tables
#' 
#' @param katz - the meta data for the Katz language model
#' @param nGrams - meta data for training N-grams
#' @param regex - regex patterns
#' @author John James
#' @export
katzInit <- function(katz, nGrams, regex) {
  
  startTime <- Sys.time()
  
  message(paste("\nInitializing Katz language model at",startTime))
  
  lapply(seq_along(nGrams), function(x) {
    message(paste('...initializing', katz$counts[[x]]$fileDesc))
    
    # Initialize with NGrams
    nGram <- loadObject(nGrams[[x]])
    counts <- data.table(nGram = featnames(nGram), key = 'nGram')
    
    # Add Context if x > 1
    if (x > 1) {
      context <- gsub(regex$context[[x-1]], "\\1", counts$nGram, perl = TRUE)
      counts[, 'context' := context]
    }
    katz$counts[[x]]$data <- counts
    saveObject(katz$counts[[x]])
  })

  # Log Results
  logR(katz$mName, startTime, katz$directory, 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('Katz Counts initialized at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
#katzInit(lm$katz$gamma, corpora$training$gamma$nGrams, regexPatterns)