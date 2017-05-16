## ---- analyze_density
#==============================================================================#
#                             analyzeDensity                                   #
#==============================================================================#
#'  analyzeDensity
#' 
#' This function conducts a lexical density analysis.  The corpus is tagged
#' and descriptive statistics are calculated for each part-of-speech for 
#' each register as well as the combined corpus. 
#' 
#' @param method - full - entire corpus and registers or fast, just combined 
#'                 corpus
#' @param korpus - the corpus meta data object
#' @param analysis - the corpus analysis including words per register 
#' @param directories - the directory structure
#' @param regex - the regex patterns
#' @param posTags - pos tags and their descriptions
#' @param chunkSize - the size of the chunks to be tagged at a time
#' @return analysis - descriptive statistics for parts-of-speech across
#'                    the three registers.  
#' @author John James
#' @export
analyzeDensity <- function(method = 'full', korpus, analysis, directories, 
                           regex,  posTags, chunkSize = 2000) {
  
  startTime <- Sys.time()
  
  # Message User
  message(paste('\nCommencing a density analysis at', Sys.time()))
  
  sampleSize <- rep(min(analysis$featureMatrix$words[1:4]),4)
  
  documentAnalyses <- list()
  if (method == 'full') {
    # Conduct analysis on individual registers
    documentAnalyses <- lapply(seq_along(korpus$documents), function(x) {
      document <- readFile(korpus$documents[[x]])
      document <- unlist(quanteda::tokenize(document, what = 'word'))
      numChunks <- min(75, floor(sampleSize[x] / chunkSize))
      parallelizeTask(analyzeDocDensity,document, korpus$documents[[x]], numChunks, chunkSize, 
                        regex, posTags)
    })
  }
  # Combine Corpus
  combinedMetaData <- list()
  combined <- unlist(lapply(seq_along(korpus$documents), function(x) {
    combinedMetaData$directory <- korpus$documents[[x]]$directory
    combinedMetaData$fileName <- korpus$documents[[x]]$fileName
    readFile(combinedMetaData)
  }))
  
  # Perform analysis on entire corpus
  combined <- unlist(quanteda::tokenize(combined, what = 'word'))
  numChunks <- min(75, floor(sampleSize[4] / chunkSize))
  combinedMetaData$category <- korpus$corpusName
  combinedMetaData$fileDesc <- korpus$corpusName
  korpusAnalysis <- parallelizeTask(analyzeDocDensity, combined, combinedMetaData, numChunks, 
                                      chunkSize, regex, posTags)
  korpusAnalysis$title <- 'Clean Corpus (Words)'
  
  # Format meta data
  metaData <- list()
  metaData$corporaName <- korpus$corpusName
  metaData$objName     <- korpus$objName
  metaData$fileName    <- korpus$fileName
  
  # Format output
  densityAnalysis <- list(
    metaData = metaData,
    documentAnalyses = documentAnalyses,
    korpusAnalysis = korpusAnalysis)
  
  # Save Analysis
  if (method != 'full') { method <- 'fast' }
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '',''),
                             method, '-density-analysis-', korpus$fileName, 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- paste0('densityAnalysis', korpus$objName)
  output$data  <- densityAnalysis
  saveObject(output)
  
  # Log Results
  logR('densityAnalysis', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Density Analysis Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(densityAnalysis)
}
## ---- end
#densityAnalysis <- analyzeDensity(method = 'full', clean, directories, regexPatterns, posTags)