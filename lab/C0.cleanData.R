## ---- clean_data

#==============================================================================#
#                              cleanFile                                       #
#==============================================================================#
#'  cleanFile
#' 
#' This function takes as its parameters, the meta data for the raw and cleaned
#' files and the regex patterns, then saves the cleaned version of the file in 
#' the designated cleaned file directory.
#' 
#' @param rawDocument <- meta data for raw data file
#' @param cleanDocument <- meta data for cleaned file
#' @param regex <- regex patterns
#' @param badWords <- list of profane words
#' @param corrections <- the corrections file
#' @author John James
#' @export
cleanFile <- function(rawDocument, cleanDocument, regex, badWords, 
                      corrections) {
  
  message(paste("\n...Cleaning", rawDocument$fileDesc, "at", Sys.time()))
  
  message("......reading file")
  textData <- unlist(readFile(rawDocument))
  
  message('......lower casing')
  textData <- tolower(textData)
  
  message("......encoding file")
  textData <- iconv(textData, "UTF-8", "ASCII", sub = "")
  
  message("......normalizing text")
  key <- paste("\\b", corrections$key, "\\b", sep = "")
  value <- corrections$value
  for (i in 1:length(key)) {
    textData <- gsub(key[i], value[i], textData, perl = TRUE)
  }
  
  message("......removing profane words")
  key <- paste("\\b", badWords$key, "\\b", sep = "")
  for (i in 1:length(key)) {
    textData <- gsub(key[i], ' ', textData, perl = TRUE)
  }
  
  normalize <- list(
    nonAscii = list(
      pattern = regex$nonAscii,
      desc    = 'non-Ascii characters',
      replace = ' '),
    nonPrintable = list(
      pattern = regex$nonPrintable,
      desc    = 'non-printable characters',
      replace = ' '),
    control = list(
      pattern = regex$control,
      desc    = 'control characters',
      replace = ' '),
    emails = list(
      pattern = regex$emails,
      desc    = 'email addresses',
      replace = ' '),
    urls = list(
      pattern = regex$urls,
      desc    = 'urls',
      replace = ' '),
    twitter = list(
      pattern = regex$twitter,
      desc    = 'twitter hashtags',
      replace = ' '),
    digits = list(
      pattern = regex$digits,
      desc    = 'digits',
      replace = ' '),
    punct = list(
      pattern = regex$punctSansApos,
      desc    = 'punctuation (except apostrophe)',
      replace = ' '),
    strays = list(
      pattern = regex$strayApostrophe,
      desc    = 'stray apostrophes',
      replace = ''),
    repeats = list(
      pattern = regex$repeatedChars,
      desc    = 'repeated character patterns',
      replace = '\\1'),
    longWords = list(
      pattern = regex$longWords,
      desc    = 'words > 40 characters',
      replace = '')
    
  )
  
  for (i in 1:length(normalize)) {
    message(paste('......removing', normalize[[i]]$desc))
    textData <- gsub(normalize[[i]]$pattern, 
                     normalize[[i]]$replace, 
                     textData, perl = TRUE)
  }
  
  message("......removing extra whitespace")
  textData <- str_replace(gsub(regex$whiteSpace, " ", str_trim(textData)), "B", "b")
  
  message("......removing empty sentences and sentences with just punctuation")
  textData <- textData[textData != ""]
  textData <- textData[textData != "'"]
  
  message("......saving")
  cleanDocument$data <- textData
  saveFile(cleanDocument)
}


#==============================================================================#
#                              cleanData                                       #
#==============================================================================#
#'  cleanData
#' 
#' This function cleans the raw corpus data and stores it in the designated 
#' directory
#' 
#' @param rawCorpus - the meta data for the raw corpus
#' @param cleanCorpus - the meta data for the clean corpus
#' @param reference - the meta data for the reference data which includes
#'                    abbreviations, contractions, profane words, & emoticons
#' @param regex  - regex patterns
#' @return dateCleaned - the current system time.
#' @author John James
#' @export
cleanData <- function(rawCorpus, cleanCorpus, reference, regex) {
  
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