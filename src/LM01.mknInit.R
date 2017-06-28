## ---- mkn_init

#==============================================================================#
#                                mknInit                                       #
#==============================================================================#
#'  mknInit
#' 
#' This function takes as its parameter, the meta data for mkn model, the text
#' nGrams and the regex parameters and initializes the data table with nGrams,
#' context and suffixes as appropriate.
#' 
#' @param mkn - the meta data for the MKN language model
#' @param nGrams - meta data for training N-grams
#' @param regex - regex patterns
#' @author John James
#' @export
mknInit <- function(mkn, nGrams, regex) {
  
  startTime <- Sys.time()
  
  message(paste("\nInitializing MKN language model at",startTime))
  
  lapply(seq_along(nGrams), function(x) {
    message(paste('...initializing', mkn$counts[[x]]$fileDesc))
    
    # Initialize with NGrams
    nGram <- loadObject(nGrams[[x]])
    counts <- data.table(nGram = featnames(nGram), key = 'nGram')
    
    # Add a start-of-sentence token to unigram to store unigram backoff weight
    if (x == 1) {
      counts <- rbindlist(list(counts, list("<s>")))
    }

    # Add Context and suffix if n > 1
    if (x > 1) {
      context <- gsub(regex$context[[x-1]], "\\1", counts$nGram, perl = TRUE)
      suffix  <- gsub(regex$suffix[[x-1]], "\\1", counts$nGram, perl = TRUE)
      counts[, c('context', 'suffix') := list(context, suffix)]
    }
    mkn$counts[[x]]$data <- counts
    saveObject(mkn$counts[[x]])
  })

  # Log Results
  logR(mkn$mName, startTime, mkn$directory, 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('MKN Counts initialized at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
#mknInit(lm$mkn$gamma, corpora$training$gamma$nGrams, regexPatterns)