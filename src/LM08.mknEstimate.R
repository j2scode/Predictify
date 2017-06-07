## ---- mkn_estimate
#==============================================================================#
#                                 mknEstimate                                  #
#==============================================================================#
#'  mknEstimate
#' 
#' This function computes the modified kneser-ney probabilities for each nGram
#' in the language model.
#' 
#' @param mkn - the meta data for the language model
#' @author John James
#' @export
mknEstimate <- function(mkn) {
  
  
  startTime <- Sys.time()
  message(paste("\nEstimating", mkn$mDesc, 'at', startTime))
  
  summary <- loadObject(mkn$summary)
  
  lapply(seq_along(mkn$counts), function(x) {
    
    message(paste('...computing', mkn$counts[[x]]$fileDesc, 'probabilities'))
    
    current <- loadObject(mkn$counts[[x]])
    
    if (x == 1) {
      
      current[, Pmkn := cKN / summary[x+1,2]$Count]

    } else {
      
      lower <- loadObject(mkn$counts[[x-1]])

      # Create temporary data table for calculations       
      temp <- current[,.(nGram, context, suffix, alpha)]
      
      # Extract lambda value for nGram context
      setkey(temp, context)
      lambda <- lower[,.(nGram, lambda)]
      temp <- merge(temp, lambda, by.x = 'context', by.y = 'nGram')
      
      # Extract Pmkn for nGram suffix
      setkey(temp, suffix)
      lowerPmkn   <- lower[,.(nGram, Pmkn)]
      temp <- merge(temp, lowerPmkn, by.x = 'suffix', by.y = 'nGram')
      
      # Calculate Pmkn
      temp[, Pmkn1 := alpha + lambda * Pmkn]
      temp <- temp[,.(nGram, Pmkn1)]
      setnames(temp, c('nGram', 'Pmkn'))
      setkey(temp, nGram)
      current <- merge(current, temp, by = 'nGram', all = TRUE)

    }
    
    # Save nGram
    mkn$counts[[x]]$data <- current
    saveObject(mkn$counts[[x]])
  })
  

  # Log and Return results
  logR('mknEstimate', startTime, mkn$directory, mkn$mName)

  # Alert User
  endTime <- Sys.time()
  message(paste('MKN model estimated at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
#mknEstimate(lm$mkn$alpha)