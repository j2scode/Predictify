## ---- tag_data

#==============================================================================#
#                                tagChunk                                      #
#==============================================================================#
#'  tagChunk
#' 
#' This function takes a sentence as a parameter, as well as the sentence, word, 
#' and POS tag annotators, and returns the POS tags and POS tag/word pairs
#' 
#' @param chunk - a chunk (list) of text to be tagged
#' @return posData - a list containing pos tags and pos tag/word pairs 
#' @author John James
#' @export
tagChunk <- function(chunk, sentAnnotator, wordAnnotator, posAnnotator) {
  
  s <- as.String(paste(chunk, collapse = ''))
  
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
    tags = tags,
    pairs = pairs
  )
  
  return(posData)
}

tagDocument <- function(document, chunkSize, sentAnnotator,
                        wordAnnotator, posAnnotator) {
  startTime <- Sys.time()
  chunks <- chunkDocument(document, chunkSize)
  message(paste('...tagging' , length(chunks), 'chunks/', chunkSize, 'sentences per chunk'))
  lapply(seq_along(chunks), function(x) {
    cat("\r......tagging chunk", x, "out of", length(chunks),'                    ' )
    tagChunk(chunks[[x]], sentAnnotator,
             wordAnnotator, posAnnotator)
  })

  logR(paste('tag chunksize = ', chunkSize), startTime, ' ', ' ')
}


# Initialize Annotators
sentAnnotator <- Maxent_Sent_Token_Annotator()
wordAnnotator <- Maxent_Word_Token_Annotator()
posAnnotator <- Maxent_POS_Tag_Annotator()


document <- readFile(schema$corpora$crossValidation$train$clean$documents$blogs)
document <- document[1:10000]

chunkSizes <- c(10000)

lapply(seq_along(chunkSizes), function(x) {
  tagDocument(document, chunkSizes[x], sentAnnotator,
              wordAnnotator, posAnnotator)
})
