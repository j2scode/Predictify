## ---- tag_document

#==============================================================================#
#                                tagSentence                                   #
#==============================================================================#
#'  tagSentence
#' 
#' This function takes a sentence as a parameter, as well as the sentence, word, 
#' and POS tag annotators, and saves the pos tags and word/tag pairs to disc
#' 
#' @param sentence - a chunk (list) of text to be tagged
#' @param sentAnnotator - the openNLP sentence annotator
#' @param wordAnnotator - the openNLP word annotator
#' @param posAnnotator - the openNLP POS annotator
#' @author John James
#' @export
tagSentence <- function(sentence, sentAnnotator, wordAnnotator,
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
#                                tagDocument                                   #
#==============================================================================#
#'  tagDocument
#' 
#' This function takes as its parameter, a corpus document including the meta 
#' data and the content in the form of vectors of sentences, and returns the 
#' tagged sentences as well as WORD/POS pairs.
#' 
#' @param document - the document object with meta data and content
#' @return posData -  a list containing the tagged text and the Word/Pos pairs
#' @author John James
#' @export
tagDocument <- function(document) {
  
  message(paste('...pos tagging', document$fileDesc))
  
  # Initialize Annotators
  sentAnnotator <- Maxent_Sent_Token_Annotator()
  wordAnnotator <- Maxent_Word_Token_Annotator()
  posAnnotator <- Maxent_POS_Tag_Annotator()
  
  startTime <- Sys.time()  
  sentences <- length(document$data)
  tagData <- lapply(seq_along(document$data), function(x) {
    if (x %in% c(10, 100, 500,1000,2000,5000,10000,20000,40000,80000,120000,140000,180000)) { 
      elapsed <- round(difftime(Sys.time(), startTime,  units = 'mins'))
      elapsed <- as.numeric(elapsed) + 1
      rate <- x / elapsed 
      remaining <- sentences - x
      timeMin <- round(remaining / rate, digits = 1)
      timeHrs <- round(timeMin / 60, digits = 1)
      message(paste('......',x,'out of',sentences, 'sentences processed in', 
                    elapsed, 'minutes.', timeMin,'minutes remaining (', timeHrs, 'hours)'))
    }
    
    tagSentence(document$data[x], sentAnnotator, wordAnnotator,
                posAnnotator)
  })
  
  # Extract POS tags and save
  tags <- lapply(seq_along(tagData), function(x) {
    unlist(tagData[[x]]$tags)
  })

  # Extract POS/Word pairs and save
  pairs <- lapply(seq_along(tagData), function(x) {
    unlist(tagData[[x]]$pairs)
  })
  
  posData <- list(
    tags = tags,
    pairs = pairs
  )
  
  return(posData)
}