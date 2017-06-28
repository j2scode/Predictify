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
      
      current[, Pmkn := cKN / summary[2,2]$Count]

    } else {
      
      # Obtain lower order probabilities
      lower <- loadObject(mkn$counts[[x-1]])
      lowerPmkn   <- lower[,.(nGram, Pmkn)]
      
      # Obtain current order nGram, suffix, alpha and lambda
      temp <- current[,.(nGram, suffix, alpha, lambda)]
      
      # Join tables
      setkey(lower, nGram)
      setkey(temp, suffix)
      temp <- merge(temp, lowerPmkn, by.x = 'suffix', by.y = 'nGram')
      
      # Calculate Pmkn
      temp[, Pmkn1 := alpha + lambda * Pmkn]
      temp <- temp[,.(nGram, Pmkn1)]
      
      # Add to current order table
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