## ---- tag_pilot

#==============================================================================#
#                                tagPilot                                      #
#==============================================================================#
#'  tagPilot
#' 
#' This function takes as its parameter, the corpus and register meta data, 
#' then tags each sentence and stores the POS tags 
#' on disk
#' 
#' @param pilot - meta data for the pilot corpus
#' @param registers - meta data for the pilot document registers 
#' @author John James
#' @export
tagPilot <- function(pilot, registers) {
  
  startTime <- Sys.time()
  
  message(paste('\nTagging', pilot$corpusName, 'at', startTime))
  document <- list()
  pos <- list()
  
  document$directory <- file.path(pilot$directory, 'documents')
  pos$directory <- file.path(pilot$directory, 'pos')
  
  lapply(seq_along(registers), function(r) {
    document$fileName <- registers[[r]]$fileName
    pos$fileName <- registers[[r]]$fileName
    
    document$data <- readFile(document)
    posData <- tagDocument(document)
    pos$data <- unlist(posData$tags)
    
    saveFile(pos)
  })
  
  
  # Log Results
  logR('tagPilot', startTime, ' ', 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste(pilot$corpusName, 'POS Tagging Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}