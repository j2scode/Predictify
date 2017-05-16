## ---- analyze_doc_density

#==============================================================================#
#                             analyzeDocDensity                                #
#==============================================================================#
#'  analyzeDocDensity
#' 
#' This function takes as its parameter, the document in word token format, the
#' document meta data, the number of chunks and the chunk size for sampling, 
#' and returns an analysis of the density of the document.  The  analysis includes 
#' descriptive statistics on the POS tags in the document as a list of tag/word 
#' pairs.
#' 
#' @param document - the document to be analyzed
#' @param metaData - the document meta data
#' @param numChunks - the number of chunks to be sampled from document
#' @param chunkSize - size of each chunk sampled from the document
#' @param regex - regex patterns
#' @param posTags - pos tags and their descriptions
#' @return analysis - analysis of lexical density for the file
#' @author John James
#' @export
analyzeDocDensity <- function(document, metaData, numChunks, chunkSize, regex,
                              posTags) {
  
  # Notify user
  message(paste('\n...conducting density analysis of ', metaData$fileDesc, 
                'at', Sys.time()))
  
  # Split data into chunks
  message(paste('......sampling', numChunks, 'chunks from document'))
  textChunks <- sampleData(document, numChunks = numChunks, 
                           chunkSize = chunkSize, format = 'lv')
  
  # Extract tokens and Words
  message('......extracting words from samples')
  sampleTokens <- quanteda::tokenize(unlist(textChunks))
  numTokens <- length(sampleTokens)
  words <- sampleTokens[nchar(sampleTokens) > 0]
  numWords <- length(words)
  
  # Prepare tag analysis
  message('......conducting document tag analysis')
  tagAnalysis <- analyzeLexicalFeatures(textChunks, posTags)
  
  # Format results
  analysis <- list(
    title = metaData$category,
    numTokens = numTokens,
    numWords = numWords,
    sampleSize = numChunks,
    sampleLength = chunkSize,
    avgVc = tagAnalysis$avgVc,
    chunkMatrix = tagAnalysis$chunkMatrix,
    featureStats = tagAnalysis$featureStats,
    tagPairs = tagAnalysis$tagPairs
  )
  
  # Notify user
  message(paste('...completing density analysis of ', metaData$fileDesc, 
                'at', Sys.time()))  
  return(analysis)
}
## ---- end