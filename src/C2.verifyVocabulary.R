## ---- verify_vocabulary

#==============================================================================#
#                         verifyVocabulary                                     #
#==============================================================================#
#'  verifyVocabulary
#' 
#' This function checks the out of vocabulary (OOV) rates of a sample corpus 
#' vis-a-vis a "test corpus".  Words in the test corpus not found in the
#' sample corpus are "OOV"
#' 
#' @param testCorpus - the meta data for the test corpus 
#' @param sampleCorpus - the meta data for the sample corpus
#' @param directories - the project directory structure
#' @return oovRates - the OOV Rate for each corpus
#' @author John James
#' @export
verifyVocabulary <- function(testCorpus, sampleCorpus, directories) {
  
  startTime <- Sys.time()
  
  message(paste("\nVerifying", sampleCorpus$corpusName, "vocabulary", startTime))
  
 
  coverage <- rbindlist(lapply(seq_along(testCorpus$documents), function(d) {
    
    message(paste('\n...obtaining test corpus', testCorpus$documents[[d]]$fileDesc))
    testCorpusDoc <- unlist(readFile(testCorpus$documents[[d]]))
    testCorpusDocDfm <- quanteda::dfm(testCorpusDoc)
    testCorpusDocV  <- ntype(testCorpusDocDfm)
    testCorpusDocN  <- sum(ntoken(testCorpusDocDfm))

    message(paste('...obtaining sample corpus', sampleCorpus$documents[[d]]$fileDesc))
    sampleDoc <- paste(unlist(readFile(sampleCorpus$documents[[d]])), collapse = ' ')
    sampleDoc <- unlist(tokenize(sampleDoc, what = 'word'))
    sampleDocDfm <- quanteda::dfm(sampleDoc)
    sampleDocV <- ntype(sampleDocDfm)
    sampleDocN <- sum(ntoken(sampleDocDfm))
    
    message('...obtaining OOV Words')
    testCorpusDoc <- quanteda::tokenize(testCorpusDoc, what = 'word')
    oov <- quanteda::dfm(testCorpusDoc, remove = sampleDoc, valuetype = 'fixed')
    Voov <- ntype(oov)
    Noov <- sum(ntoken(oov))

    message('...calculating OOV rate and coverage')
    oovRate  <- Noov / testCorpusDocN * 100
    coverage <- 100 - oovRate
    
    # Format output
    oov <- list()
    oov$register <- registers[[d]]$fileDesc
    oov$testCorpusV <- testCorpusDocV
    oov$testCorpusN <- testCorpusDocN
    oov$docV <- sampleDocV
    oov$docN <- sampleDocN
    oov$Noov <- Noov
    oov$rate  <- oovRate
    oov$coverage <- coverage
    oov
  }))
  
  # Format total line and variable names
  coverage <- as.data.frame(coverage)
  
  testCorpusV <- sum(coverage$testCorpusV)
  testCorpusN <- sum(coverage$testCorpusN)
  docV <- sum(coverage$docV)
  docN <- sum(coverage$docN)
  Noov <- sum(coverage$Noov)
  rate  <- mean(coverage$rate)
  cover <- mean(coverage$coverage)
  ttl <- data.frame(register = 'Corpus', testCorpusV = testCorpusV, testCorpusN = testCorpusN, 
                    docV = docV, docN = docN, Noov = Noov, rate = rate, coverage = cover,
                    stringsAsFactors = FALSE)
  coverage <- rbind(coverage, ttl)
  names(coverage) <- c('Registers', 'HC Corpus Vocabulary', 'HC Corpus Tokens',
                       'Pilot Corpus Vocabulary', 'Pilot Corpus Tokens', 
                       '# OOV Words', 'OOV Rate', 'Coverage')
  
  # Save Results
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('verify-vocabulary')),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'verifyVocabulary'
  output$data  <- coverage
  saveObject(output)
  
  # Log and Return results
  logR('verifyVocabulary', startTime, ' ', ' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Vocabulary verified at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))

  return(coverage)
}
## ---- end
#coverage <- verifyVocabulary(testCorpus, )