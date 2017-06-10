## ---- katz_alpha
#==============================================================================#
#                                 katzAlpha                                    #
#==============================================================================#
#'  katzAlpha
#' 
#' This function computes the alpha backoff weights.
#' 
#' @param katz - the meta data for the language model
#' @author John James
#' @export
katzAlpha <- function(katz) {
  
  startTime <- Sys.time()
  message(paste("\nComputing Alpha Backoff Weights for", training$corpusName, 'at', startTime))
  

  lapply(seq_along(katz$counts), function(x) {
    message(paste('...processing', katz$counts[[x]]$fileDesc))
    current <- loadObject(katz$counts[[x]])
    
    if (x == 1) {
      # Compute observed unigram maximum likelihood probability
      current[, pKatzNGram := r / sum(r)] 
    
    } else {
      # Compute katz Observed nGram Probability
      current[, pKatzNGram := cKatzNGram / cContext]
      
      # Compute katz suffix probabilities
      lower <- loadObject(katz$counts[[x-1]])
      lower <- lower[, nGram, pKatzNGram]
      setnames(lower, 'nGram', 'suffix')
      setnames(lower, 'pKatzNGram', 'pKatzSuffix')
      setkey(current, suffix)
      current <- merge(current, lower, by = 'suffix')
      
      # Compute sum of katz ngram probabilities by context
      current[, pKatzNGramSum := sum(pKatzNGram), by = 'context']
      
      # Compute sum of katz suffix probabilities by context
      current[, pKatzSuffixSum := sum(pKatzSuffix), by = 'context']
      
      # Compute alpha by context
      current[, alpha := ((1-pKatzNGramSum) / (1-pKatzSuffixSum))]
    }
    katz$counts[[x]]$data <- current
    saveObject(katz$counts[[x]])
  })


  # Log and Return results
  logR('katzAlpha', startTime, ' ', ' ')

  # Alert User
  endTime <- Sys.time()
  message(paste0('Katz Alpha Backoff Weights Computed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#est <- katzEvaluate(lm$katz, corpora$training$processed$text$quadgram,  corpora$validation$processed$text$quadgram, directories)