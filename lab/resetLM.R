## ---- reset_lm
#==============================================================================#
#                                 resetLM                                      #
#==============================================================================#
#'  resetLM
#' 
#' This function resets the language models by removing the probability 
#' estimate column(s)
#' 
#' @param mkn - the meta data for the language model
#' @author John James
#' @export
resetLM <- function(mkn) {
  
  
  startTime <- Sys.time()
  message(paste("\nResetting", mkn$mDesc, 'at', startTime))
  
  message('...loading model')
  model <- lapply(seq_along(mkn$counts), function(x) {
    loadObject(mkn$counts[[x]])
  })
  
  lapply(seq_along(model), function(x) {
    
    model[[x]][ , Pmkn := NULL]
    mkn$counts[[x]]$data <- model[[x]]
    saveObject(mkn$counts[[x]])
    
  })
  
  endTime <- Sys.time()
  # Log and Return results
  logR('lmReset', startTime, mkn$directory, mkn$mName)

  # Alert User
  message(paste('MKN model reset at', endTime))

  
}
## ---- end
#resetLM(lm$mkn$alpha)