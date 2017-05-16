## ---- mkn_ckn

#==============================================================================#
#                                   mknCKN                                     #
#==============================================================================#
#'  mknCKN
#' 
#' This function takes as its parameter, the meta data for mkn model and 
#' calculates Continuation Counts. For highest order, the continuation count
#' is the count of the sequence in the training set. For lower orders, the 
#' continuation count is the number of single word contexts that preceed
#' the current ngram.                      
#' 
#' @param mkn - the meta data for the MKN language model
#' @author John James
#' @export
mknCKN <- function(mkn) {
  
  startTime <- Sys.time()
  
  message('\nPreparing to calculate continuation counts')
  model <- lapply(seq_along(mkn$args$counts), function(x) {
    loadObject(mkn$args$counts[[x]])
  })

  lapply(seq_along(model), function(x) {
    message(paste('...calculating continuation counts for', 
                  mkn$args$counts[[x]]$fileDesc))
    if (x < 4) {
      # Extract current and higher nGram counts
      current <- model[[x]]
      higher  <- model[[x+1]][,.(suffix, count)]
      setkey(higher, suffix)
      
      # Get counts by suffix
      counts <- higher[,.(cKN = .N), by = .(suffix)]
      
      # Merge Columns into current order ngram
      current <- merge(current, counts, by.x = 'nGram',by.y = 'suffix', all.x = TRUE)
      
      # Clear all NA values
      for (i in seq_along(current)) set(current, i=which(is.na(current[[i]])), j=i, value=0)
    } else {
      current <- model[[x]]
      current <- current[,cKN := count]
    }
    # Save counts
    mkn$args$counts[[x]]$data <- current
    saveObject(mkn$args$counts[[x]])
  })
  

  # Log Results
  logR('MKN Continuation Counts', startTime, mkn$args$counts[[1]]$directory,' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('MKN Continutation Counts Calculated at ', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
#mknAdjustedCounts(lm$mkn)