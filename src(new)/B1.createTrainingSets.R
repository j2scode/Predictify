## ---- transform_data

#==============================================================================#
#                              transformData                                   #
#==============================================================================#
#'  transformData
#' 
#' This function takes as its parameters, the meta data for the raw and cleaned
#' files and the regex patterns, then saves the cleaned version of the file in 
#' the designated cleaned file directory.
#' 
#' @param cleanData <- the meta data for the cleaned corpus
#' @param directories - the project directory structure
#' @return compendium - 
#' @author John James
#' @export
transformData <- function(cleanData, directories)

  startTime <- Sys.time()
  message(paste("\nCleaning Raw Corpus at"), startTime)
  
  message('...loading reference data')
  badWords <- readFile(reference$badWordsFile)
  corrections <- readFile(reference$corrections)
  
  # Cleaning File
  lapply(seq_along(rawCorpus$documents), function(x) {
    cleanFile(rawCorpus$documents[[x]], cleanCorpus$documents[[x]], regex, 
              badWords, corrections)
  })
  
  # Log
  endTime <- Sys.time()
  message(paste("Raw Corpus Cleaning Complete at "), endTime)
  message(paste("Elapsed time is ", difftime(endTime, startTime, units = 'auto')))
  logR('cleanData', startTime, cleanCorpus$directory, '*.txt')
}
## ---- end
# cleanData(schema$corpora$crossValidation$train$raw,
#           schema$corpora$crossValidation$train$clean$text,
#           schema$referenceFiles, schema$regexPatterns)