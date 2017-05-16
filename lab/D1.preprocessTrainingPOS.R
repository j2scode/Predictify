## ---- preprocess_training_pos

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
#                         preprocessTrainingPOS                                #
#==============================================================================#
#'  preprocessTrainingPOS
#' 
#' This function takes as its parameters, a document to be tagged and a list
#' of POS tags 
#' 
#' @param document - the document to be tagged
#' @param korpus - meta data for corpus being processed 
#' @return posData - list containing the pos tagged document and word/tag pairs
#' @author John James
#' @export
preprocessTrainingPOS <- function(document, korpus) {
  
  startTime <- Sys.time()
  
  message(paste("\nPreprocessing training POS data at", startTime))
  
  # Initialize Annotators
  sentAnnotator <- Maxent_Sent_Token_Annotator()
  wordAnnotator <- Maxent_Word_Token_Annotator()
  posAnnotator <- Maxent_POS_Tag_Annotator()
  
  # Annotate document
  posData <- lapply(seq_along(document), function(x) {
    #cat("\r...tagging sentence", x, "out of", length(document), "sentences.                 ")
    tagSentence(document[x], x, sentAnnotator, wordAnnotator,
                posAnnotator)
  })

  # Combine pos tagged sentences into single document
  korpus$preprocessedData$pos$data <- unlist(lapply(seq_along(posData), function(x) {
    posData[[x]]$tags
  }))
  saveFile(korpus$preprocessedData$pos)
  
  # Combine word / tag pairs into single list and save
  korpus$preprocessedData$pairs$data <- unlist(lapply(seq_along(posData), function(x) {
    posData[[x]]$pairs
  }))
  saveFile(korpus$preprocessedData$pairs)
  
  # Log Results
  logR('tagCorpus', startTime, korpus$directory, 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('POS Preprocessing Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))

}
## ---- end
#processPOS(training, posTags)