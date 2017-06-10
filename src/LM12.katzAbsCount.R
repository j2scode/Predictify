## ---- katz_abs_counts

#==============================================================================#
#                                katzAbsCount                                  #
#==============================================================================#
#'  katzAbsCount
#' 
#' This function calculates the absolute counts of each ngram as well as the
#' frequencies of frequencies of each ngram order.
#' 
#' @param katz - the meta data for the Katz language model
#' @param nGrams - the meta data for the processed training set ngrams
#' @return summaryCounts - the summary counts for each nGram
#' 
#' @author John James
#' @export
katzAbsCount <- function(katz, nGrams) {
  
  startTime <- Sys.time()
  
  message(paste("\nCalculating nGram Counts for Katz model at", startTime))
  
  summaryCounts <- rbindlist(lapply(seq_along(nGrams), function(x) {
    message(paste('...calculating counts for', nGrams[[x]]$fileDesc))
    
    # Load N-grams 
    current <- loadObject(katz$counts[[x]])
    setkey(current, nGram)

    # Get N-Gram counts from N-Gram
    currentNGram <- loadObject(nGrams[[x]])
    counts <- data.table(nGram = featnames(currentNGram), 
                             r = colSums(currentNGram),
                             key = 'nGram')

    # Merge with counts table
    current <- merge(current, counts, by='nGram', all.x = TRUE)
    
    # Compute context counts
    if (x > 1) {
       current[, cContext := sum(r), by = context]
    }

    # Clear all NA values
    for (i in seq_along(current)) set(current, i=which(is.na(current[[i]])), j=i, value=0)
    
    # Save counts
    setkey(current, nGram)
    katz$counts[[x]]$data <- current
    saveObject(katz$counts[[x]])

    # Calculate frequencies of frequencies 
    fof <- table(counts[, r])
    Nr <- as.integer(fof)
    r <- as.integer(as.character(names(fof)))
    freqs <- data.table(r = r, Nr = Nr)
    
    # Add frequency of UNK which replaced hapax legomena for unigrams
    if (x == 1) {
      if (nrow(current[nGram == 'UNK']) > 0 ) {
        freqs <- rbindlist(list(freqs, list(1, current[nGram == 'UNK', r])))
      }
    }
    
    # Save frequencies of frequencies
    setkey(freqs, r)
    katz$freq[[x]]$data <- freqs
    saveObject(katz$freq[[x]])
    
    # Summarize counts 
    s <- list()
    s$nGram <- katz$counts[[x]]$fileDesc
    s$mOrder <- x
    s$Count <- nfeature(currentNGram)
    s
  }))
  
  # Save Summary counts
  katz$summary$data <- summaryCounts
  saveObject(katz$summary)

  # Log Results
  logR(paste(katz$mName, 'counts'), startTime, 
       katz$counts[[1]]$directory, 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('...Katz Absolute Counts completed at ', endTime))
  message(paste('...Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(summaryCounts)
}
## ---- end
#summary <- katzAbsCount(corpora$train$processedData$nGrams$text, lm$katz)