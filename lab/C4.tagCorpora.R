## ---- tag_corpora

#==============================================================================#
#                                tagSentence                                   #
#==============================================================================#
#'  tagSentence
#' 
#' This function takes a sentence as a parameter, as well as the sentence, word, 
#' and POS tag annotators, and saves the pos tags and word/tag pairs to disc
#' 
#' @param sentence - a chunk (list) of text to be tagged
#' @param sentenceNum - the sentence number
#' @param sentAnnotator - the openNLP sentence annotator
#' @param wordAnnotator - the openNLP word annotator
#' @param posAnnotator - the openNLP POS annotator
#' @author John James
#' @export
tagSentence <- function(sentence, sentenceNum, sentAnnotator, wordAnnotator,
                        posAnnotator) {
  
  s <- as.String(sentence)
  
  # Format sentence and word annotator
  a2 <- NLP::annotate(s, list(sentAnnotator, wordAnnotator))
  
  # Format POS Annotator
  a3 <- NLP::annotate(s, posAnnotator, a2)
  
  # Create Annotations
  a2 <- annotate(s, list(sentAnnotator, wordAnnotator))
  a3 <- annotate(s, posAnnotator, a2)
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  pairs <- sprintf("%s/%s", s[a3w], tags)
  
  # Format results
  posData <- list(
    tags = paste0(tags, collapse = ' '),
    pairs = pairs
  )
  
  return(posData)
}

#==============================================================================#
#                                tagCorpus                                     #
#==============================================================================#
#'  tagCorpus
#' 
#' This function takes as its parameters, the meta data for the corpus to be
#' tagged and the POS tagged data, then conducts POS tagging of the corpus 
#' and saves it to disc according to the POS meta data. 
#' 
#' @param corpusText - meta data for text to be tagged
#' @param corpusPOS - the meta data for the POS tags
#' @param corpusPairs - the meta data for the Word/POS Pair tags
#' @author John James
#' @export
tagCorpus <- function(corpusText, corpusPOS, corpusPairs) {
  
  startTime <- Sys.time()
  
  # Initialize Annotators
  sentAnnotator <- Maxent_Sent_Token_Annotator()
  wordAnnotator <- Maxent_Word_Token_Annotator()
  posAnnotator <- Maxent_POS_Tag_Annotator()
  
  # Read Document
  document <- readFile(corpusText)
  
  # Annotate document
  posData <- lapply(seq_along(document), function(x) {
    if (x %in% c(1000, 20000, 50000, 100000, 200000, 300000, 400000, 500000)) { 
      elapsed <- round(difftime(Sys.time(), startTime,  units = 'mins'))
      elapsed <- as.numeric(elapsed) + 1
      rate <- x / elapsed 
      remaining <- length(document) - x
      timeMin <- round(remaining / rate, digits = 1)
      timeHrs <- round(timeMin / 60, digits = 1)
      message(paste('......',x,'out of',length(document), 'sentences processed in', 
                    elapsed, 'minutes.', timeMin,'minutes remaining (', timeHrs, 'hours)'))
    }
    
    tagSentence(document[x], x, sentAnnotator, wordAnnotator,
                posAnnotator)
  })

  # Combine pos tagged sentences into single document
  corpusPOS$data <- unlist(lapply(seq_along(posData), function(x) {
    unlist(posData[[x]]$tags)
  }))
  saveFile(corpusPOS)
  
  # Combine word / tag pairs into single list and save
  corpusPairs$data <- unlist(lapply(seq_along(posData), function(x) {
    unlist(posData[[x]]$pairs)
  }))
  saveFile(corpusPairs)
  
}

## ---- end  

#==============================================================================#
#                                tagCorpora                                    #
#==============================================================================#
#'  tagCorpora
#' 
#' This function takes as its parameters, the meta data for the project corpora,
#' an indicator of which corpus to tag, and for training sets, whether to tag
#' the preprocessed or processed corpus, then parallelizes the task that 
#' performs the POS tagging.
#' 
#' @param korpora - the meta data for the project corpora
#' @param korpus - c('training', 'validation', 'test' )
#' @param what - c('preprocessed', 'processed')
#' @author John James
#' @export
tagCorpora <- function(korpora, korpus, what = 'preprocessed') {
  
  # Set Start Time
  startTime <- Sys.time()
  
  message(paste('\nTagging', what, korpus, 'corpus at', startTime))
  
  if (korpus == 'training' & what == 'preprocessed') {
    corpusText   <- korpora$train$preprocessedData$text
    corpusPOS    <- korpora$train$preprocessedData$pos
    corpusPairs  <- korpora$train$preprocessedData$pairs
  } else if (korpus == 'training' & what == 'processed') {
    corpusText   <- korpora$train$processedData$text$trigram
    corpusPOS    <- korpora$train$processedData$pos$trigram
    corpusPairs  <- korpora$train$processedData$pairs$trigram
  } else if (korpus == 'validation') {
    corpusText   <- korpora$validation$processedData$text$trigram
    corpusPOS    <- korpora$validation$processedData$pos$trigram
    corpusPairs  <- korpora$validation$processedData$pairs$trigram
  } else if (korpus == 'test') {
    corpusText   <- korpora$test$processedData$text$trigram
    corpusPOS    <- korpora$test$processedData$pos$trigram
    corpusPairs  <- korpora$test$processedData$pairs$trigram
  }
  
  parallelizeTask(tagCorpus, corpusText, corpusPOS, corpusPairs) 
 
 # Log Results
 logR('tagCorpora', startTime, ' ', 'various')
 
 # Alert User
 endTime <- Sys.time()
 message(paste(corpusText$fileDesc, 'POS Tagging Complete at', endTime))
 message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
#tagCorpora(corpora, 'training', what = 'preprocessed')