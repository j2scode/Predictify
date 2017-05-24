## ---- oov_corpora

#==============================================================================#
#                                 oovTagDocument                               #
#==============================================================================#
#'  oovTagDocument
#' 
#' This function tags the OOV words with UNK, then returns the document to the 
#' calling environment.
#'  
#' @param document - the document to be tagged
#' @param oov - the oov vocabulary to be replaced with UNK
#' @return document - returns the document once tagged
#' @author John James
#' @export
oovTagDocument <- function(document, oov) {
  
  # Tag document OOV words
  document <- unlist(lapply(seq_along(document), function(x) {
    tokens <- unlist(strsplit(document[[x]], " "))
    i <- fmatch(tokens, oov)
    ind <- !is.na(i)
    i <- na.omit(i)
    tokens[ind] <- names(oov)[i]
    sent <- paste0(tokens, collapse = ' ')
    if (x %% 100000 == 0) { message(paste('......', x, 'sentences processed for OOVs')) }
    sent
  }))
  
  return(document)
}
  

#==============================================================================#
#                                 oovTestSets                                  #
#==============================================================================#
#'  oovTestSets
#' 
#' This function reads the test set and  replaces all words in the test set 
#' that are not in the training set with UNK.  Finally, it saves the file to 
#' the appropriate processed subdirectory.  
#' 
#' 
#' @param test - the meta data for the test corpus (either validation or test)
#' @param vTraining - the training set vocabulary
#' @return summary - list containing summary of words, oov and oov rates and
#'                   the vocabulary
#' @author John James
#' @export
oovTestSets <- function(test, vTraining) {
  
  message(paste('...processing OOV for', test$corpusName))
  
  # Read document
  message('......loading document')
  document <- unlist(lapply(seq_along(test$documents), function(d) {
    unlist(readFile(test$documents[[d]]))
  }))
  
  # Get vocabulary and token statistics
  message('......creating document feature matrix')
  df <- quanteda::dfm(document, tolower = FALSE)
  vocabulary <- featnames(df)
  V <- length(vocabulary)
  N <- sum(colSums(df))
  
  # Determine OOV words not in training vocabulary
  message('......extracting OOV words')
  oovWords <- dfm_select(df, features = vTraining, selection = 'remove', 
                         valuetype = 'fixed') 
  oovWords <- featnames(oovWords)
  names(oovWords) <- rep('UNK', length(oovWords)) 
  message(paste('......extracted', length(oovWords), 'OOV Words'))
  
  # Tag and save document
  test$processed$words[[1]]$data <- oovTagDocument(document, oovWords)
  saveFile(test$processed$words[[1]])
  
  # Summarize Statistics 
  oov <- length(oovWords)
  summary <- data.frame(corpus = test$corpusName,
                        V = V, N = N, oov = oov, oovrate = oov / N)
  names(summary) <- c('Corpus', 'Vocabulary','Tokens' ,'OOV', 'OOV Rate')
  
  message(paste('...processing OOV for', test$corpusName, 'complete'))
  
  return(summary)
  
}

#==============================================================================#
#                                oovTrainingSet                                #
#==============================================================================#
#'  oovTrainingSet
#' 
#' This function takes as its parameter, the input corpus and output corpus
#' meta data, converts all hepax legomena in the input corpus to UNK psuedo-word,
#' and returns a list containing summary OOV statistics and the training
#' set vocabulary to the calling environment.
#' 
#' 
#' @param training - the meta data for training set
#' @return summary - list containing summary of words, oov and oov rates and
#'                   the vocabulary
#' @author John James
#' @export
oovTrainingSet <- function(training) {
  
  message(paste('...processing OOV for', training$corpusName))
  
  message('......loading document')
  document <- unlist(lapply(seq_along(training$documents), function(d) {
    unlist(readFile(training$documents[[d]]))
  }))

  # Get vocabulary and token statistics
  message(paste('......creating document feature matrix'))
  df <- quanteda::dfm(document, tolower = FALSE)
  vocabulary <- featnames(df)
  V <- length(vocabulary)
  N <- sum(colSums(df))
  
  # Capture hapax legomena and extract from vocabulary
  message('......extracting hapax legomena')
  oov <- quanteda::dfm_trim(df, max_count = 1)
  hapaxLegomena <- featnames(oov)
  vocabulary <- dfm_select(df, features = hapaxLegomena, selection = 'remove', 
                         valuetype = 'fixed') 
  vocabulary <- featnames(vocabulary)
  
  # Format UNK pseudoword
  names(hapaxLegomena) <- rep('UNK', length(hapaxLegomena))
  message(paste('......extracted', length(hapaxLegomena), 'hapax legomena'))
 
  # Tag and save document
  training$processed$words[[1]]$data <- oovTagDocument(document, hapaxLegomena) 
  saveFile(training$processed$words[[1]])

  # Summarize Statistics 
  oov <- length(hapaxLegomena)
  oovRate <- oov / N
  summary <- data.frame(corpus = training$corpusName,
                        V = V, N = N, oov = oov, oovRate = oovRate)
  names(summary) <- c('Corpus', 'Vocabulary','Tokens' ,'OOV', 'OOV Rate')
  
  # Format and return results
  res <- list(
    summary = summary,
    vocabulary = vocabulary)
  
  message(paste('...processing OOV for', training$corpusName, 'complete'))
  return(res)
}
  


#==============================================================================#
#                                oovCorpora                                    #
#==============================================================================#
#'  oovCorpora
#' 
#' This function processes the training and test set for OOV words. For the 
#' training set all hepax legomena will be replaced with "UNK", the unkown 
#' pseudo tag. For the test set all words not in the training set will 
#' be similarly replaced with the an unknown pseudo-word.
#' 
#' @param training - the meta data for the training set 
#' @param test - the meta data for the test set
#' @return summary - summary of words, oov and oov rates for each corpus
#' @author John James
#' @export
oovCorpora <- function(training, test) {
    
  # Performing OOV processing
  trainSummary <- oovTrainingSet(training)
  testSummary <- oovTestSets(test, trainSummary$vocabulary)
  
  summary <- rbind(trainSummary$summary, testSummary)
  
  return(summary)
}

#==============================================================================#
#                                preprocessCorpora                             #
#==============================================================================#
#'  preprocessCorpora
#' 
#' This function preprocesses the word and POS-based ngram data. The word based 
#' data includes the training set, validation and test sets. The preprocessing 
#' for the word data includes the  following:
#'   1. Mark all hapax legomena in the training set as an unknown word with
#'      the 'UNK' pseudo token and save in the first slot in the processed
#'      words subdirectory of the training set.
#'   2. Mark all words in the test and validation sets that are not in the 
#'      training set as an unknown word, with the 'UNK' pseudo token and save in 
#'      the appropriate processed subdirectory.
#'      
#' The POS data will be combined into a single file and stored in the first
#' slot of the processed pos subdirectory of the training set. The POS Word
#' pairs will similarly be combined into a single file and stored in the 
#' processed pos word pairs directory
#' 
#' 
#' @param training - the meta data for the training set
#' @param test - the meta data for the test set (validation or test set)
#' @param directories - the project directory structure
#' @return summary - summary of words, oov and oov rates for each corpus
#' @author John James
#' @export
preprocessCorpora <- function(training, validation, directories) {
  
  startTime <- Sys.time()
  
  message(paste("\nPreprocessing", training$corpusName, 'at', startTime))
  
  oov <- oovCorpora(training, test)
  
  # Save Results
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('preprocess-corpora')),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'preprocessCorpora'
  output$data  <- oov
  saveObject(output)
  
  # Log and Return results
  logR('preprocessCorpora', startTime, ' ', ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpora preprocessed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(oov)
}
  
## ---- end
#oov <- preprocessCorpora(corpora, directories)