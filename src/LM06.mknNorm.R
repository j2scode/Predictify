## ---- mkn_norm

#==============================================================================#
#                                 mknNorm                                      #
#==============================================================================#
#'  mknNorm
#' 
#' This function takes as its parameter, the meta data for mkn model and 
#' calculates the normalizing factor, which is either the count for the context
#' in the lower order model, or the number of unique ngrams in the current
#' order model. The normalizing factor is added to the model.
#' 
#' @param mkn - the meta data for the MKN language model
#' @param N - nGram order
#' @author John James
#' @export
mknNorm <- function(mkn, N) {
  
  startTime <- Sys.time()
  
  message(paste('\nCalculating the normalizing factor at', startTime))
  
  message('...loading nGram counts and summary counts')
  model <- lapply(seq_along(mkn$counts), function(x) {
    loadObject(mkn$counts[[x]])
  })
  summary <- loadObject(mkn$summary)

  
  model <- lapply(seq_along(model), function(x) {
    current <- model[[x]]
    message(paste('...calculating normalizing factors for', 
                  mkn$counts[[x]]$fileDesc))
    # Compute normalizing factor
    if (x < N) {
      norm <- rep(summary[x+1,2]$Count, nrow(current))
      current <- current[, norm := norm]
    } else {
      norm <- current[,.(norm = sum(count)), by=context]
      setkey(current, context)
      current <- merge(current, norm, by.x = 'context', by.y = 'context', all.x = TRUE)
      setkey(current, nGram)
    }

    # Save  counts
    mkn$counts[[x]]$data <- current
    saveObject(mkn$counts[[x]])
    
  })
  
  # Log Results
  logR('MKN Normalizers', startTime, mkn$counts[[1]]$directory,' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('MKN Normalizers Calculated at ', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
#mknAdjustedCounts(lm$mkn)