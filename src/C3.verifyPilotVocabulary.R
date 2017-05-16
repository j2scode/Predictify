## ---- verify_corpus_vocabulary

#==============================================================================#
#                         verifyPilotVocabulary                                #
#==============================================================================#
#'  verifyPilotVocabulary
#' 
#' This function takes as its parameters, the directory structure and the 
#' meta data for the hc and training corpora and determines the OOV rate
#' of the training corpus on the HC corpus.
#' 
#' @param hc - the meta data for the hc corpus
#' @param analysis - the analysis of the hc data
#' @param pilot - the meta data for the pilot corpus
#' @param registers - the meta data for the registers
#' @param directories - the project directory structure
#' @return oovRates - the OOV Rate for each corpus
#' @author John James
#' @export
verifyPilotVocabulary <- function(hc, analysis, pilot, registers, directories) {
  
  startTime <- Sys.time()
  
  message(paste("\nVerifying corpora vocabulary", startTime))
  
  hcCorpus <- list()
  hcCorpus$directory <- hc$directory
  
  pilotCorpus <- list()
  pilotCorpus$directory <- file.path(pilot$directory, 'documents')
  
  coverage <- rbindlist(lapply(seq_along(registers), function(d) {
    
    message(paste('...obtaining HC Corpus', registers[[d]]$fileDesc))
    hcCorpus$fileName <- registers[[d]]$fileName
    hcDoc <- unlist(readFile(hcCorpus))
    hcDocDfm <- quanteda::dfm(hcDoc)
    hcDocV  <- ntype(hcDocDfm)
    hcDocN  <- sum(ntoken(hcDocDfm))

    message(paste('...obtaining vocabulary for', registers[[d]]$fileDesc))
    pilotCorpus$fileName <- registers[[d]]$fileName
    modelDoc <- paste(unlist(readFile(pilotCorpus)), collapse = ' ')
    modelDoc <- unlist(tokenize(modelDoc, what = 'word'))
    modelDocDfm <- quanteda::dfm(modelDoc)
    modelDocV <- ntype(modelDocDfm)
    modelDocN <- sum(ntoken(modelDocDfm))
    
    message('...obtaining OOV Words')
    hcDoc <- quanteda::tokenize(hcDoc, what = 'word')
    oov <- quanteda::dfm(hcDoc, remove = modelDoc, valuetype = 'fixed')
    Voov <- ntype(oov)
    Noov <- sum(ntoken(oov))

    message('...calculating OOV rate and coverage')
    oovRate  <- Noov / hcDocN * 100
    coverage <- 100 - oovRate
    
    # Format output
    oov <- list()
    oov$register <- registers[[d]]$fileDesc
    oov$hcV <- hcDocV
    oov$hcN <- hcDocN
    oov$docV <- modelDocV
    oov$docN <- modelDocN
    oov$Noov <- Noov
    oov$rate  <- oovRate
    oov$coverage <- coverage
    oov
  }))
  
  # Format total line and variable names
  coverage <- as.data.frame(coverage)
  
  hcV <- sum(coverage$hcV)
  hcN <- sum(coverage$hcN)
  docV <- sum(coverage$docV)
  docN <- sum(coverage$docN)
  Noov <- sum(coverage$Noov)
  rate  <- mean(coverage$rate)
  cover <- mean(coverage$coverage)
  ttl <- data.frame(register = 'Corpus', hcV = hcV, hcN = hcN, 
                    docV = docV, docN = docN, Noov = Noov, rate = rate, coverage = cover,
                    stringsAsFactors = FALSE)
  coverage <- rbind(coverage, ttl)
  names(coverage) <- c('Corpus', 'HC Corpus Vocabulary', 'HC Corpus Tokens',
                       'Training Corpus Vocabulary', 'Training Corpus Tokens', 
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
#coverage <- verifyPilotVocabulary(hc, )