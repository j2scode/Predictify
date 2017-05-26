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

  lapply(seq_along(model), function(x) {
    message(paste('...calculating alpha probabilities for', 
                  mkn$counts[[x]]$fileDesc))
    
    current <- model[[x]]
    
    if (x == 1) {
      current <- current[, alpha := cKN / norm]
    } else {
      # Calculate discount based upon raw counts of nGram
      dCounts <- t(subset(discounts, nGramOrder == x, select = c(-(1:2))))
      current[, group := min(3, (count)), by=count]
      current[, D := dCounts[group+1]]
      current[, alphaCount := (cKN - D)]
      current[alphaCount < 0, alphaCount := 0]
      current[, alpha := alphaCount / norm]
    }
    # Save  counts
    mkn$counts[[x]]$data <- current
    saveObject(mkn$counts[[x]])
  })
  
  
  # Log Results
  logR('mknAlpha', startTime, ' '  , ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('Alpha calculations completed at ', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#mknTrain(lm$mkn)