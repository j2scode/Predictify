## ---- build_pilot
#------------------------------------------------------------------------------#
#                              buildPilot                                      #
#------------------------------------------------------------------------------#
#'  buildPilot 
#' 
#' This function takes as its parameters, the meta data for the clean and
#' pilot corpora, the pilot corpus design and the meta data for the registers.
#' The function creates a pilot corpus according to the design and stores it
#' for downsream analysis
#' 
#' @param clean - the clean set meta data
#' @param pilot - the pilot corpus meta data
#' @param design - the pilot corpus design
#' @author John James
#' @export
buildPilot <- function(clean, pilot, design) {
  
  startTime <- Sys.time()
  message(paste('\nBuilding pilot corpus at', startTime))

  message('...reading clean corpus')
  korpus <- lapply(seq_along(clean$documents), function(r) {
    readFile(clean$documents[[r]])
  })
  
  lapply(seq_along(korpus), function(d) {
    
    chunks <- design$`# Chunks`[d]
    chunkSize <- design$`Sentences per Chunk`[d]
    # Sample the data 
    chunks <- sampleData(korpus[[d]], chunks, chunkSize, format = 'v')
    
    # Save the file
    pilot$documents[[d]]$data <- chunks
    saveFile(pilot$documents[[d]])
  })
  

  # Log Results
  logR('buildPilot', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Pilot corpus build complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#css <- estimateRegisterSize(corpora$clean, posTags, directories)