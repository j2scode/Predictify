## ---- oov_corpora

#==============================================================================#
#                                 oovTagDocument                               #
#==============================================================================#
#'  oovTagDocument
#' 
#' This function takes as its parameter, document to be tagged, its meta data
#' and the oov vocabulary and tags the OOV words with UNK, then saves the 
#' document to disc.
#' 
#' 
#' @param document - the document to be tagged
#' @param outKorpus - the meta data for the oov tagged (preprocessed) document
#' @param oov - the oov vocabulary to be replaced with UNK
#' @author John James
#' @export
oovTagDocument <- function(document, outKorpus, oov) {
  
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
  
  # Save Document
  outKorpus$document$data <- document
  saveFile(outKorpus$document)
}
  

#==============================================================================#
#                                 oovTestSets                                  #
#==============================================================================#
#'  oovTestSets
#' 
#' This function takes as its parameter, the meta data for the test (or 
#' validation) set and the training set vocabulary. It reads the test set and 
#' replaces all words in the test corpus that are not in the training set 
#' with UNK.  Finally, it saves the file to disc.
#' 
#' 
#' @param inKorpus - the meta data for the input (clean) corpus
#' @param outKorpus - the meta dat for the output (preprocessed) corpus
#' @param vTraining - the training set vocabulary
#' @return summary - list containing summary of words, oov and oov rates and
#'                   the vocabulary
#' @author John James
#' @export
oovTestSets <- function(inKorpus, outKorpus, vTraining) {
  
  message(paste('...processing OOV for', inKorpus$corpusName))
  
  # Read document
  message('......loading document')
  document <- unlist(lapply(seq_along(inKorpus$documents), function(d) {
    unlist(readFile(inKorpus$documents[[d]]))
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
  
  # Tag Document
  oovTagDocument(document, outKorpus, oovWords)
  
  # Summarize Statistics 
  oov <- length(oovWords)
  summary <- data.frame(corpus = outKorpus$corpusName,
                        V = V, N = N, oov = oov, oovrate = oov / N)
  names(summary) <- c('Corpus', 'Vocabulary','Tokens' ,'OOV', 'OOV Rate')
  
  message(paste('...processing OOV for', outKorpus$corpusName, 'complete'))
  
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
#' @param inKorpus - the meta data for input (clean) corpus
#' @param outKorpus - the meta data for the output (preprocessed) corpus
#' @return summary - list containing summary of words, oov and oov rates and
#'                   the vocabulary
#' @author John James
#' @export
oovTrainingSet <- function(inKorpus, outKorpus) {
  
  message(paste('...processing OOV for', inKorpus$corpusName))
  
  message('......loading document')
  document <- unlist(lapply(seq_along(inKorpus$documents), function(d) {
    unlist(readFile(inKorpus$documents[[d]]))
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
 
  # Tag document
  oovTagDocument(document, outKorpus, hapaxLegomena) 

  # Summarize Statistics 
  oov <- length(hapaxLegomena)
  oovRate <- oov / N
  summary <- data.frame(corpus = outKorpus$corpusName,
                        V = V, N = N, oov = oov, oovRate = oovRate)
  names(summary) <- c('Corpus', 'Vocabulary','Tokens' ,'OOV', 'OOV Rate')
  
  # Format and return results
  res <- list(
    summary = summary,
    vocabulary = vocabulary)
  
  message(paste('...processing OOV for', inKorpus$corpusName, 'complete'))
  return(res)
}
  


#==============================================================================#
#                                oovCorpora                                    #
#==============================================================================#
#'  oovCorpora
#' 
#' This function takes the corpora meta data as its parameter and processes
#' the training, validation and test sets for OOV words. For the training set
#' all hepax legomena will be replaced with "UNK", the unkown word pseudo tag.
#' For the validation and test sets all words not in the training set will 
#' be similarly replaced with the an unknown pseudo-word.
#' 
#' @param korpora - the meta data for the project corpora
#' @return summary - summary of words, oov and oov rates for each corpus
#' @author John James
#' @export
oovCorpora <- function(korpora) {
    
  message(paste("...processing OOV for Corpora"))
  
  # Define input and output corpora
  inKorpora <- list(
    training = korpora$train$clean,
    validation = korpora$validation$clean,
    test = korpora$test$clean
  )
  
  outKorpora <- list(
    training = korpora$training$preprocessed,
    validation = korpora$validation$preprocessed,
    test = korpora$test$preprocessed
  )
  
  # Performing OOV processing
  trainSummary <- oovTrainingSet(inKorpora$training, outKorpora$training)
  valSummary <- oovTestSets(inKorpora$validation, outKorpora$validation, 
                            trainSummary$vocabulary)
  testSummary <- oovTestSets(inKorpora$test, outKorpora$test, 
                             trainSummary$vocabulary)
  
  summary <- rbind(trainSummary$summary, valSummary, testSummary)
  
  return(summary)
}

#==============================================================================#
#                                preprocessCorpora                             #
#==============================================================================#
#'  preprocessCorpora
#' 
#' This function takes the corpora meta data as its parameter and preprocesses
#' the word and POS-based ngram data. The word based data in the training,
#' validation and test sets are treated for OOV words.  The POS based training
#' data, currently stored at the register level, are combined into a single
#' documents including the POS tagged sentences and the POS/Word pairs. 
#' 
#' @param korpora - the meta data for the project corpora
#' @return summary - summary of words, oov and oov rates for each corpus
#' @author John James
#' @export
preprocessCorpora <- function(korpora, directories) {
  
  startTime <- Sys.time()
  
  message(paste("\nPreprocessing Corpora at", startTime))
  
  oov <- oovCorpora(korpora)
  
  message('...preprocessing POS tag training data')
  document <- unlist(lapply(seq_along(korpora$training$clean$pos), function(d) {
    readFile(korpora$training$clean$pos[[d]])
  }))
  korpora$training$preprocessed$pos$data <- document
  saveFile(korpora$training$preprocessed$pos)
  
  message('...preprocessing POS/Word pair training data')
  document <- unlist(lapply(seq_along(korpora$training$clean$pairs), function(d) {
    readFile(korpora$training$clean$pairs[[d]])
  }))
  korpora$training$preprocessed$pairs$data <- document
  saveFile(korpora$training$preprocessed$pairs)
  
  
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