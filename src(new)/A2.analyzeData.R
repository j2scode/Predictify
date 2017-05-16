## ---- analysis_data
#==============================================================================#
#                               getFeatures                                    #
#==============================================================================#
#'  getFeatures
#' 
#' This function takes as its parameters, the meta data for the corpus file,
#' the regex patterns and reference data. The function then returns a 
#' featurevector containing various feature measures.
#' 
#' @param type - type of analysis 'full' or 'fast'
#' @param korpus - the meta data for the corpus
#' @param docId - the document sequence number (1=blog, 2=news, 3=twitter)
#' @param regex  - the regex patterns
#' @param referenceData - abbreviations, contractions, emoticons, etc...
#' @return analysis - a list containing a vector of feature measures 
#'                    and a list of samples
#' @author John James
#' @export
getFeatures <- function(type, korpus, docId, regex, referenceData) {
  
  message(paste("...analyzing", korpus$documents[[docId]]$fileDesc,
                'at', Sys.time()))
  
  # Initialize inspection objects and key variables
  featureVector  <- list()
  featureSamples <- list()
  featureTables  <- list()
  wordLengths    <- list()
  
  
  # Read Data
  message('......loading the text')
  sents <- readFile(korpus$documents[[docId]])
  tokens <- unlist(quanteda::tokenize(unlist(sents), what = 'word'))
  
  # Extract Words
  message("......extracting words")
  words <- grep(regex$words, tokens, value = TRUE, perl = TRUE)
  words <- gsub(regex$punct, '', words, perl = TRUE)
  words <- words[nchar(words) > 0]
  
  #===========================================================================#
  #                     Extract Descriptive Statistics                        #        
  #===========================================================================#
  message("......extracting basic features")
  featureVector$category   <- korpus$documents[[docId]]$category
  featureVector$objectSize <- as.numeric(round((object.size(tokens) / 1000000), 1))
  featureVector$sentences  <- length(sents)
  featureVector$tokens     <- length(tokens)
  featureVector$words      <- length(words)
  featureVector$wordTypes  <- length(unique(words))
  featureVector$chars      <- sum(nchar(words))
  featureVector$wordsPerSent    <- featureVector$words / featureVector$sentences
  featureVector$minWordLength   <- min(nchar(words))
  featureVector$maxWordLength   <- max(nchar(words))
  featureVector$meanWordLength  <- mean(nchar(words))
  
  if (type == 'full') {  
    #=========================================================================#
    #                 Extract Noisy Character Classes                         #        
    #=========================================================================#
    message("......extracting tokens containing control characters")
    control <- grep(regex$control, tokens, value = TRUE, perl = TRUE)
    featureVector$control <- length(control)
    controlsTable <- as.data.frame(table(control))
    featureSamples$control <- head(controlsTable[order(-controlsTable$Freq),], 20)
    
    
    message("......extracting tokens containing non-ASCII characters")
    nonAscii <- grep(regex$nonAscii, tokens, value = TRUE, perl = TRUE)
    featureVector$nonAscii <- length(nonAscii)
    nonAscii <- nonAscii[nchar(nonAscii) == 1]
    nonAsciiTable  <- as.data.frame(table(nonAscii))
    featureSamples$nonAscii  <- nonAsciiTable[order(-nonAsciiTable$Freq),]
    
    message("......extracting tokens containing non-Printable characters")
    nonPrint <- grep(regex$nonPrintable, tokens, value = TRUE, perl = TRUE)
    featureVector$nonPrint <- length(nonPrint)
    nonPrintTable  <- as.data.frame(table(nonPrint))
    featureSamples$nonPrint  <- nonPrintTable[order(-nonPrintTable$Freq),]
    
    message("......extracting tokens containing non-UTF8 characters")
    nonUTF8i  <- grep("tokens", iconv(tokens, "latin1", "ASCII", sub="tokens"))
    nonUTF8   <- tokens[nonUTF8i]
    featureVector$nonUTF8 <- length(nonUTF8)
    nonUTF8Table  <- as.data.frame(table(nonUTF8))
    featureSamples$nonUTF8 <- nonUTF8Table[order(-nonUTF8Table$Freq),]
    
    #=========================================================================#
    #                      Extract Linguistic Features                        #        
    #=========================================================================#
    message("......extracting tokens containing contractions")
    featureSamples$contractions <- tokens[tokens %in% referenceData$contractions$key]
    featureVector$contractions	<- length(featureSamples$contractions)
    featureTables$contractions  <- as.data.frame(table(featureSamples$contractions))
    featureTables$contractions <- featureTables$contractions[
      order(-featureTables$contractions$Freq),]
    featureTables$contractions$category <- korpus$documents[[docId]]$category
    
    message("......extracting tokens containing abbreviations")
    featureSamples$abbreviations <- tokens[tokens %in% referenceData$abbreviations$key]
    featureVector$abbreviations	 <- length(featureSamples$abbreviations)
    featureTables$abbreviations  <- as.data.frame(table(featureSamples$abbreviations))
    featureTables$abbreviations <- featureTables$abbreviations[
      order(-featureTables$abbreviations$Freq),]
    featureTables$abbreviations$category  <- korpus$documents[[docId]]$category
    
    message("......extracting tokens containing profanity")
    featureSamples$badWords <- tokens[tokens %in% referenceData$badWordsFile$key]
    featureVector$badWords	<- length(featureSamples$badWords)
    featureTables$badWords  <- as.data.frame(table(featureSamples$badWords))
    featureTables$badWords <- featureTables$badWords[
      order(-featureTables$badWords$Freq),]
    featureTables$badWords$category  <- korpus$documents[[docId]]$category
    
    message("......extracting tokens requiring normalization")
    featureSamples$corrections <- tokens[tokens %in% referenceData$corrections$key]
    featureVector$corrections	<- length(featureSamples$corrections)
    featureTables$corrections  <- as.data.frame(table(featureSamples$corrections))
    featureTables$corrections <- featureTables$corrections[
      order(-featureTables$corrections$Freq),]
    if (featureVector$corrections > 0) { 
      featureTables$corrections$category  <- korpus$words$documents[[docId]]$category
    }
  }  
  analysis <- list(featureVector  = featureVector,
                   featureTables = featureTables,
                   featureSamples = featureSamples,
                   wordLengths = wordLengths)
  
  message(paste("...analysis for ", korpus$documents[[docId]]$fileDesc,
                'complete at', Sys.time()))
  return(analysis)
}

#==============================================================================#
#                             summarizeAnalysis                                   #
#==============================================================================#
#'  summarizeAnalysis
#' 
#' This function takes as its parameter, the korpus meta data, the featureMatrix
#' feature samples and the regex patterns, then produces a list of corpus total 
#' figures for the featureMatrix.
#' 
#' @param type - indicates type of analysis 'full', or 'fast'
#' @param korpus - the corpus meta data
#' @param featureMatrix - the feature matrix for the 3 registers
#' @param regex - the regex patterns
#' @return totals - a data frame containing total feature counts for the corpus
#' @author John James
#' @export
summarizeAnalysis <- function(type, korpus, featureMatrix, 
                              featureSamples, regex) {
  
  message("...summarizing corpus feature totals")
  gc()
  
  # Initialize Structures
  featureTables <- list()
  wordLengths   <- list()
  
  # Obtain word types across the combined corpus
  message("......reading combined tokenized corpus")
  sents <- unlist(lapply(seq_along(korpus$documents), function(x) {
    readFile(korpus$documents[[x]])
  }))
  tokens <- unlist(quanteda::tokenize(sents, what = 'word'))
  
  # Extract Word Types
  message("......extracting total word ")
  words <- grep(regex$words, tokens, value = TRUE, perl = TRUE)
  words <- gsub(regex$punct, '', words, perl = TRUE)
  words <- words[nchar(words) > 0]
  
  #===========================================================================#
  #                     Summarize Descriptive Statistics                      #        
  #===========================================================================#
  message("......summarizing Descriptive Statistics")
  featureVector <- list()
  featureVector$category	  <- 	"Corpus"
  featureVector$objectSize <- sum(featureMatrix$objectSize)
  featureVector$sentences  <- 	sum(featureMatrix$sentences)
  featureVector$tokens	  <- 	sum(featureMatrix$tokens)
  featureVector$words	  <- 	sum(featureMatrix$words)
  featureVector$wordTypes	  <- 	length(unique(words))
  featureVector$chars 	  <- 	sum(featureMatrix$chars)
  featureVector$wordsPerSent    <- length(words) / sum(featureMatrix$sentences)
  featureVector$minWordLength	  <- min(nchar(words))
  featureVector$maxWordLength	  <- max(nchar(words))
  featureVector$meanWordLength	<- mean(nchar(words))
  
  if (type == 'full') {
    #=========================================================================#
    #                        Summarize Noisy Features                         #        
    #=========================================================================#
    message("......summarizing noisy features")
    featureVector$nonAscii      <- sum(featureMatrix$nonAscii)
    featureVector$control      <- sum(featureMatrix$control)
    featureVector$nonPrint      <- sum(featureMatrix$nonPrint)
    featureVector$nonUTF8      <- sum(featureMatrix$nonUTF8)
    
    #=========================================================================#
    #                    Summarize Linguistic Features                        #        
    #=========================================================================#
    message("......summarizing linguistic features")
    
    featureVector$contractions  <- 	sum(featureMatrix$contractions)
    allContractions <- unlist(lapply(seq_along(featureSamples), function(x) {
      featureSamples[[x]]$contractions
    }))
    featureTables$contractions  <- as.data.frame(table(allContractions))
    if (nrow(featureTables$contractions) > 0) {
      featureTables$contractions$category  <- 'Corpus'
    }
    
    featureVector$abbreviations  <- 	sum(featureMatrix$abbreviations)
    allabbreviations <- unlist(lapply(seq_along(featureSamples), function(x) {
      featureSamples[[x]]$abbreviations
    }))
    featureTables$abbreviations  <- as.data.frame(table(allabbreviations))
    if (nrow(featureTables$abbreviations) > 0) {
      featureTables$abbreviations$category  <- 'Corpus'
    }  
    
    featureVector$badWords  <- 	sum(featureMatrix$badWords)
    allBadWords <- unlist(lapply(seq_along(featureSamples), function(x) {
      featureSamples[[x]]$badWords
    }))
    featureTables$badWords  <- as.data.frame(table(allBadWords))
    if (nrow(featureTables$badWords) > 0) {
      featureTables$badWords$category  <- 'Corpus'
    }  
    
    featureVector$corrections  <- 	sum(featureMatrix$corrections)
    allcorrections <- unlist(lapply(seq_along(featureSamples), function(x) {
      featureSamples[[x]]$corrections
    }))
    featureTables$corrections  <- as.data.frame(table(allcorrections))
    if (nrow(featureTables$corrections) > 0) {
      featureTables$corrections$category  <- 'Corpus'
    }  
    
  }
  featureVector <- as.data.frame(featureVector)
  
  totals <- list(featureVector = featureVector,
                 featureTables = featureTables,
                 wordLengths = wordLengths)
  return(totals)
  
}
#==============================================================================#
#                              analyzeData                                     #
#==============================================================================#
#'  analyzeData
#' 
#' This function takes as its parameter, meta data initiates a series of 
#' vectorized calls to a function that analyzes each file and returns a list 
#' of feature measures and feature samples.  The feature measures are combined 
#' into a single data frame, summary corpus level feature measures are calculated 
#' and the list, containing the feature measure data frame and the selected 
#' samples is returned to the calling environment.
#' 
#' @param type - 'full' analysis or 'fast' (abbreviated) analysis
#' @param korpus - the meta data for the corpora to be analyzed
#' @param reference - the meta data for the reference files which contain
#'                    lists of profane words, abbreviations, emoticons
#'                    and contractions.
#' @param directories - the directory structure 
#' @param regex - the regex patterns
#' @return analysis - a list containing a matrix of feature counts and 
#'          measures, and a list of selected sample features from each document.
#' @author John James
#' @export
analyzeData <- function(type, korpus, reference, directories, regex) {
  
  startTime <- Sys.time()
  
  message(paste("\nConducting a Analysis of "), korpus$corpusName, " at ",  startTime)
  
  # Load reference data and address regex patterns
  referenceData <- loadReferenceData(reference)
  
  # Inspect Individual Corpus Files
  featureVectors <- lapply(seq_along(korpus$documents),function(x) {
    getFeatures(type, korpus, x, regex, referenceData)})
  
  # Combine vectors into feature matrix
  featureMatrix <- rbindlist(lapply(seq_along(featureVectors), function(x) {
    featureVectors[[x]]$featureVector}))
  
  # Combine feature samples into list
  featureSamples <- lapply(seq_along(featureVectors), function(x) {
    featureVectors[[x]]$featureSamples})
  
  # Combine word length distributions into list of data frames
  wordLengths <- lapply(seq_along(featureVectors), function(x) {
    featureVectors[[x]]$wordLengths})
  
  # Combine feature tables into list of data frames
  featureTables <- lapply(seq_along(featureVectors), function(x) {
    featureVectors[[x]]$featureTables})
  
  # Get totals for corpus
  totals <- summarizeAnalysis(type, korpus, featureMatrix, featureSamples, regex)
  featureMatrix <- rbind(featureMatrix, totals$featureVector)
  totalWordLengths <- totals$wordLengths
  totalFeatureTables <- totals$featureTables
  
  # Format Summary Data
  metaData <- list(corpus = korpus$corpusName,
                   objName = korpus$objName,
                   fileName = korpus$fileName,
                   analysisDate = Sys.time()) 
  
  analysis <- list(metaData = metaData,
                   featureMatrix = featureMatrix,
                   featureSamples = featureSamples,
                   featureTables = featureTables,
                   totalWordLengths = totalWordLengths,
                   totalFeatureTables = totalFeatureTables)
  
  # Save Results
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0(type, '-analysis-of-')),
                             korpus$fileName, 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- paste0('analysis', korpus$objName)
  output$data  <- analysis
  saveObject(output)
  
  # Log Results
  logR('analyzeData', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Analysis Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(analysis)
}
## ---- end
#rawCorpusAnalysis <- analyzeData('full', schema$corpora$raw, schema$referenceFiles, schema$directories, schema$regexPatterns)
