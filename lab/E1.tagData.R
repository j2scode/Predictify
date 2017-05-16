## ---- tag_data

#==============================================================================#
#                                tagDocument                                   #
#==============================================================================#
#'  tagDocument
#' 
#' This function takes a sentence as a parameter, as well as the sentence, word, 
#' and POS tag annotators, and returns the POS tags and POS tag/word pairs
#' 
#' @param document - a chunk (list) of text to be tagged
#' @return posData - a list containing pos tags and pos tag/word pairs 
#' @author John James
#' @export
tagDocument <- function(document) {
  
  # Initialize Annotators
  sentAnnotator <- Maxent_Sent_Token_Annotator()
  wordAnnotator <- Maxent_Word_Token_Annotator()
  posAnnotator <- Maxent_POS_Tag_Annotator()
  
  s <- as.String(paste(document, collapse = ''))
  
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

#==============================================================================#
#                                tagData                                       #
#==============================================================================#
#'  tagData
#' 
#' This function annotates a corpus with POS tags 
#' 
#' @param korpus - the meta data for korpus to be tagged
#' @param korpusTags - the meta data for the korpus POS tags
#' @param korpusTagPairs - the meta data for the korpus Word/Tag pairs
#' @author John James
#' @export
tagData <- function(korpus, korpusTags, korpusTagPairs) {
  
  startTime <- Sys.time()
  
  message(paste("\nTagging", korpus$corpusName, "at", startTime))
  
  # Establish clusters
  cl <- makeCluster(mc <- getOption("cl.cores", detectCores()-2))
  clEval <- clusterEvalQ(cl, {
    library(openNLP)
    library(NLP)
    PTA <- Maxent_POS_Tag_Annotator()
  })
  
  # Annotate korpus
  korpusPOSData <- lapply(seq_along(korpus$documents), function(d) {
    message(paste('...tagging', korpus$documents[[d]]$fileDesc))
    document <- unlist(readFile(korpus$documents[[d]]))
    posData <- parLapply(cl, document, tagDocument)
  })
  
  # Save POS Tag Data
  lapply(seq_along(korpusPOSData), function(x) {
    korpusTags[[x]]$data <- unlist(korpusPOSData[[x]]$tags)
    saveFile(korpusTags[[x]])
  })

  # Save Word POS Tag Pair Data
  lapply(seq_along(korpusPOSData), function(x) {
    korpusTagPairs[[x]]$data <- unlist(korpusPOSData[[x]]$tags)
    saveFile(korpusTagPairs[[x]])
  })
  
  # Log Results
  logR('tagData', startTime, korpusTags$directory, 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('POS Staging Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}

## ---- end
# tagData(schema$corpora$crossValidation$train$clean, 
#         schema$corpora$crossValidation$train$tagged, 
#         schema$corpora$crossValidation$train$wordTags)