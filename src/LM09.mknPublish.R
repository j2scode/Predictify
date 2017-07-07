## ---- mkn_publish
#==============================================================================#
#                                 mknPublish                                   #
#==============================================================================#
#'  mknPublish
#' 
#' This function publishes the counts to the language model for evaluation
#' 
#' @param mkn - the meta data for the language model
#' @param directories - the project directory structure
#' @author John James
#' @export
mknPublish <- function(mkn, directories) {
  
  
  startTime <- Sys.time()
  message(paste("\nPublishing", mkn$mDesc, 'at', startTime))
  
  modelSize <- rbindlist(lapply(seq_along(mkn$counts), function(x) {
    message(paste('...publishing', mkn$counts[[x]]$fileDesc))
    counts <- loadObject(mkn$counts[[x]])
    
    if (x == 1) {
      model <- counts[, c('nGram', 'Pmkn')]
    } else {
      model <- counts[, c('nGram', 'contextCount', 'alphaCount', 'DnNn', 'Pmkn')]
    }
    setkey(model, nGram)
    mkn$model[[x]]$data <- model
    saveObject(mkn$model[[x]])
    s <- list()
    s$Model <- mkn$mDesc
    s$nGram <- mkn$model[[x]]$fileDesc
    s$Before <- object.size(counts)
    s$After <- object.size(model)
    s
  }))

  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('MKN-publish-')),
                             mkn$mName,
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'modelSize'
  output$data  <- modelSize
  saveObject(output)
  
  # Log and Return results
  logR('mknPublish', startTime, mkn$directory, mkn$mName)

  # Alert User
  endTime <- Sys.time()
  message(paste('MKN model published at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(modelSize)
  
}
## ---- end
#ms <- mknPublish(lm$mkn$delta, directories)