## ---- verify_corpus_features

#==============================================================================#
#                          verifyCorpusLexicalFeatures                         #
#==============================================================================#
#'  verifyCorpusLexicalFeatures
#' 
#' This function takes the pos tags and a corpus' meta data as its paramters,
#' then reads, samples, and analyzes the lexical feature distribution for the 
#' corpus.
#' 
#' @param korpus - the meta data for the corpus
#' @param posTags - the POS Tags 
#' @param sampleSize - the sample size in tokens and number of samples
#' @return analysis - the lexical feature distribution analysis.
#' @author John James
#' @export
verifyCorpusLexicalFeatures <- function(korpus, posTags, sampleSize) {
  
  message(paste('...loading', korpus$corpusName))
  document <- unlist(lapply(seq_along(korpus$documents), function(d) {
    unlist(tokenize(readFile(korpus$documents[[d]]), what = 'word'))
  }))
  
  message(paste('...sampling', korpus$corpusName))
  samples <- sampleData(document, numChunks = sampleSize$chunks, 
                        chunkSize = sampleSize$chunkSize, format = 'lv')
  
  message('...conducting lexical feature analysis')
  analysis <-  analyzeLexicalFeatures(samples, posTags)
  
  return(analysis)
  
}
  
  

#==============================================================================#
#                              verifyLinguistics                               #
#==============================================================================#
#'  verifyLinguistics
#' 
#' This function compares the lexical feature distribution of the HC Corpus
#' with that of the training corpus.
#' 
#' @param hcCorpus - the meta data for the HC Corpus
#' @param pilot - the meta data for the pilot corpus
#' @param posTags - the pos tags
#' @param directories - the directory structure for the project
#' @return x2 - results from chi-squared test
#' @author John James
#' @export
verifyLinguistics <- function(hcCorpus, pilot, posTags, directories) {
  
  startTime <- Sys.time()
  
  message(paste("\nComparing HC and model corpora at", startTime))
  
  # Initialize sample size
  sampleSize <- list()
  sampleSize$chunkSize <- 2000
  sampleSize$chunks <- 100
  
  # Select POS Tags of interest
  posTags <- subset(posTags, Study == TRUE)

  # Format corpora list
  korpora <- list()
  korpora$hc <- hcCorpus
  korpora$pc <- pilot
  
  # Conduct analyses
  analyses <- lapply(seq_along(korpora), function (c) {
    verifyCorpusLexicalFeatures(korpora[[c]], posTags, sampleSize)
  })

  # Calculate means of lexical feature counts  
  aMeans <- mean(as.data.frame(analyses[[1]]$featureMatrix)[,1])
  bMeans <- mean(as.data.frame(analyses[[2]]$featureMatrix)[,1])
  
  for (i in 2:nrow(posTags)) {
    aMeans <- rbind(aMeans, mean(as.data.frame(analyses[[1]]$featureMatrix)[,i]))
    bMeans <- rbind(bMeans, mean(as.data.frame(analyses[[2]]$featureMatrix)[,i]))
  }


  # Create summary data table
  tagMeans <- data.table(Tag = posTags[,('Tag')], 
                         Description = posTags[,('Description')],
                         'HC Corpus' = aMeans[,1], 'Training Corpus' = bMeans[,1])
  
  
  # Compute chi-squared statistic
  x2 <- chisq.test(aMeans, bMeans)
  
  # Summarize results
  features <- list(
    analyses = analyses,
    means = tagMeans,
    pValue = x2$p.value
  )
  
  # Save Results
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('verify-feature-distribution')),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'featureDistributionVerification'
  output$data  <- features
  saveObject(output)

  # Alert User
  endTime <- Sys.time()
  message(paste('Feature Distribution Verification Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(features)  
  
}
## ---- end
#features <- verifyLinguistics(corpora$clean, corpora$training, posTags, directories)