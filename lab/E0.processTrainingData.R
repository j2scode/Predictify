## ---- process_training_data
#==============================================================================#
#                           processTrainingData                                #
#==============================================================================#
#'  processTrainingData
#' 
#' This function takes as its parameter, the preprocessed training data and 
#' creates multigram versions of the data withsentence boundary annotations.
#' 
#' 
#' @param preprocessedData - the meta data for the preprocessed document
#' @param processed - the meta data for the processed document
#' @param type - c('text', 'pos')
#' @author John James
#' @export
processTrainingData <- function(preprocessedData, processed, type = 'text') {
  
  startTime <- Sys.time()
  
  message(paste("\nProcessing Training Text at", startTime))
  
  message(paste('...reading', preprocessedData$fileDesc))
  document <- readFile(preprocessedData)
  
  if (type == 'text') {
    message('...extracting hapax legomena')
    df <- quanteda::dfm(document)
    df <- quanteda::dfm_trim(df, max_count = 1)
    hapaxLegomena <- featnames(df)
    names(hapaxLegomena) <- rep('UNK', length(hapaxLegomena))
    message(paste('...extracted', length(hapaxLegomena), 'hapax legomena'))
    
    document <- unlist(lapply(seq_along(document), function(x) {
      tokens <- unlist(strsplit(document[[x]], " "))
      i <- fmatch(tokens, hapaxLegomena)
      ind <- !is.na(i)
      i <- na.omit(i)
      tokens[ind] <- names(hapaxLegomena)[i]
      sent <- paste0(tokens, collapse = ' ')
      if (x %% 100000 == 0) { message(paste('......', x, 'sentences processed for OOVs')) }
      sent
    }))
  }  
  
  lapply(seq_along(processed), function(x) {
    message(paste('...annotating', processed[[x]]$fileDesc, 'sentence boundaries'))
    annotatedDoc <- unlist(lapply(seq_along(document), function(s) {
      paste(paste0(rep("BOS", times = x-1), collapse = " "), document[s], "EOS", collapse = " ")
    }))
    processed[[x]]$data <- annotatedDoc
    saveFile(processed[[x]])
  })
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Processing of training text completed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
