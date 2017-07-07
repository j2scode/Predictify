## ---- mkn_alpha

#==============================================================================#
#                                   mknAlpha                                   # 
#==============================================================================#
#'  mknAlpha
#' 
#' This function takes at its parameters, the modified Kneser-Ney model and 
#' updates the model with the alpha pseudo probabilities.  These are the 
#' probabilities of each nGram without interpolation. 
#' 
#' @param mkn - the model meta data 
#' @author John James
#' @export
mknAlpha <- function(mkn) {
  
  startTime <- Sys.time()
  
  message(paste('\nCalculation pseudo probabilities alpha ', startTime))
  
  message('...loading models and discounts')
  model <- lapply(seq_along(mkn$counts), function(x) {
    loadObject(mkn$counts[[x]])
  })
  discounts <- loadObject(mkn$discounts)
  summary <- loadObject(mkn$summary)

  lapply(seq_along(model), function(x) {
    message(paste('...calculating alpha probabilities for', 
                  mkn$counts[[x]]$fileDesc))
    if (x > 1) {
      # Load nGram Model n
      current <- model[[x]]
      
      # Select discounts for nGram Order
      dCounts <- t(subset(discounts, nGramOrder == x, select = c(-(1:2))))
      
      # Compute N1count groups based upon continution counts
      current[, group := min(3, (cKN)), by=cKN]
      
      # Determine the discount based upon the count group
      current[, D := dCounts[group+1]]
      
      # Subtract discount from continuation count
      current[, alphaCount := (cKN - D)]
      current[alphaCount < 0, alphaCount := 0]
      
      # Normalize alpha count 
      if (x < mkn$mOrder) {
        
        # Normalize alpha count
        current[, alpha := alphaCount / summary[x+1,2]$Count]
        
      } else {
        
        # Normalize by raw count of context
        current[, alpha := alphaCount / contextCount]
      }
      # Save  counts
      mkn$counts[[x]]$data <- current
      saveObject(mkn$counts[[x]])
    }
  })

  
  # Log Results
  logR('mknAlpha', startTime, ' '  , ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('Alpha calculations completed at ', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#mknAlpha(lm$mkn$alpha)