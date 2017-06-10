## ---- katz_prune
#==============================================================================#
#                                 katzPrune                                    #
#==============================================================================#
#'  katzPrune
#' 
#' This function prunees the katz language model and publishes it for evaluation
#' 
#' @param katz - the meta data for the language model
#' @param directories - the project directory structure
#' @author John James
#' @export
katzPrune <- function(katz, directories) {
  
  
  startTime <- Sys.time()
  message(paste("\nPruneing", katz$mDesc, 'at', startTime))
  
  modelSize <- rbindlist(lapply(seq_along(katz$counts), function(x) {
    message(paste('...pruneing', katz$counts[[x]]$fileDesc))
    counts <- loadObject(katz$counts[[x]])
    if (x == 1) {
      model <- counts[, c('nGram', 'pKatzNGram')]
    } else {
      model <- counts[, c('nGram', 'context', 'suffix', 'pKatzNGram',
                          'pKatzSuffix', 'alpha')]
    }
    setkey(model, nGram)
    katz$model[[x]]$data <- model
    saveObject(katz$model[[x]])
    s <- list()
    s$Model <- katz$mDesc
    s$nGram <- katz$model[[x]]$fileDesc
    s$Before <- object.size(counts)
    s$After <- object.size(model)
    s
  }))

  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('Katz-prune-')),
                             katz$mName,
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'modelSize'
  output$data  <- modelSize
  saveObject(output)
  
  # Log and Return results
  logR('katzPrune', startTime, katz$directory, katz$mName)

  # Alert User
  endTime <- Sys.time()
  message(paste('Katz model pruneed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(modelSize)
  
}
## ---- end
#ms <- katzPrune(lm$katz$delta, directories)