## ---- mkn_Estimate

#==============================================================================#
#                                   mknEvaluate                                # 
#==============================================================================#
#'  mknEvaluate
#' 
#' This function calculates the modified Kneser-Ney estimates for the 
#' n = 4-gram model
#' 
#' @param mkn - the model meta data 
#' @author John James
#' @export
mknEvaluate <- function(mkn) {
  
  startTime <- Sys.time()
  
  message(paste('\nEstimating probabilities at ', startTime))
  
  estimate <- function(x) {
    message(paste('...estimating', mkn$counts[[x]]$fileDesc))
    
    # Load nGrams
    lower <- loadObject(mkn$counts[[x-1]])
    current <- loadObject(mkn$counts[[x]])

    # Obtain probability for the suffix 
    pSuffix <- lower[,.(nGram, Pmkn)]
    setnames(pSuffix, 'Pmkn', 'PmknSuffix')
    
    # Merge probability of the suffix into a temporary data table
    temp <- current[,.(nGram, suffix, alpha, lambda)]
    temp <- merge(temp, pSuffix, by.x = 'suffix', by.y = 'nGram', all.x = TRUE)
    
    # Calculate probability
    temp <- temp[,Pmkn := alpha + lambda * PmknSuffix]
    temp <- temp[,.(nGram, PmknSuffix, Pmkn)]
    
    # Merge into current data table
    current <- merge(current, temp, by='nGram', all.x = TRUE)
    
    # Clear all NA values
    for (i in seq_along(current)) set(current, i=which(is.na(current[[i]])), j=i, value=0)
    
    # Save data table
    mkn$counts[[x]]$data <- current
    saveObject(mkn$counts[[x]])
    
  }


  nGrams <- lapply(seq_along(mkn$model), function(x) {
    if (x == 1) {
      message(paste('...estimating', mkn$counts[[x]]$fileDesc))
      current <- loadObject(mkn$counts[[x]])
      current <- current[, Pmkn := alpha]
      mkn$counts[[x]]$data <- current
      saveObject(mkn$counts[[x]])
      
    } else {
      estimate(x)
    }
  })

  # Log Results
  logR('mknEvaluate', startTime, ' '  , ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('MKN Estimates calculated at ', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#mknTrain(lm$mkn)