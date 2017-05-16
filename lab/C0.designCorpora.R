## ---- design_corpus

#==============================================================================#
#                              designCorpora                                    #
#==============================================================================#
#'  designCorpora
#' 
#' This function designs the training, val, and test sets based upon
#' the diversity and density based sample size estimations.
#' 
#' @param sampleSizeEstCoverage - the coverage-based sample estimate
#' @param sampleSizeEstDensity - the density-based sample estimate
#' @param cleanCorpusAnalysis - the clean data analysis
#' @param directories - the directory structure
#' @return design - the language model corpus design 
#' @author John James
#' @export
designCorpora <- function(sampleSizeEstCoverage, sampleSizeEstDensity,
                         cleanCorpusAnalysis, directories) {
  
  startTime <- Sys.time()
  
  stopifnot(length(sampleSizeEstCoverage) > 0 &
              length(sampleSizeEstDensity) > 0 &
              length(cleanCorpusAnalysis) > 0 &
              length(directories) > 0) 
  
  register <- sampleSizeEstCoverage$tokens$Document[1:3]
  n <- cleanCorpusAnalysis$featureMatrix$words[1:3]
  sents <- cleanCorpusAnalysis$featureMatrix$sentences[1:3]
  sentLength <- round(cleanCorpusAnalysis$featureMatrix$wordsPerSent[1:3], digits = 1)
  coverageEst <- round(subset(sampleSizeEstCoverage$tokens[1:3,], 
                        select =  `95%`)$`95%`, digits = 0)
  densityEst <- sampleSizeEstDensity$summary$Tokens
  trainWords <- pmax(coverageEst, densityEst)
  trainSent  <- round(trainWords / sentLength, digits = 0)
  valWords   <- round(min(n * .1, (n - trainWords) / 2), digits = 0)
  valSents  <- round(min(sents * .1, (sents - trainSent) / 2), digits = 0)
  testWords <- round(min(n * .1, (n - trainWords) / 2), digits = 0)
  testSents <- round(min(sents * .1, (sents - trainSent) / 2), digits = 0)
  
  design <- data.frame(register = register, n = n, sents = sents,
                       sentLength = sentLength, coverageEst = coverageEst,
                       densityEst = densityEst, trainWords = trainWords,
                       trainSent = trainSent, valWords = valWords,
                       valSents = valSents, testWords =testWords,
                       testSents = testSents)
  
  designTotal <- data.frame(register = 'Corpus',
                            n = sum(design$n),
                            sents = sum(design$sents),
                            sentLength = mean(design$sentLength),
                            coverageEst = sum(design$coverageEst),
                            densityEst = sum(design$densityEst),
                            trainWords = sum(design$trainWords),
                            trainSent = sum(design$trainSent),
                            valWords = sum(design$valWords),
                            valSent = sum(design$valSent),
                            testWords = sum(design$testWords),
                            testSent = sum(design$testSent))
  
  cNames <- c('Register', 'Words', 'Sentences', 'Words per Sent',
              'Coverage', 'Density',  'Train (Words)', 'Train (Sent)',
              'Validation (Words)', 'Validation (Sent)', 
              'Test (Words)', 'Test (Sent)')
  names(design) <- cNames
  names(designTotal) <- cNames
  design <- rbind(design, designTotal)
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', ''),
                             'corpus-design-', 
                             cleanCorpusAnalysis$metaData$fileName, 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'corpusDesign'
  output$data  <- design
  saveObject(output)
  
  # Log Results
  logR('designCorpora', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpus Design Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(design)
  
}
## ---- end
#corpusDesign <- designCorpora(sampleSizeEstCoverage, sampleSizeEstDensity, analysiscleanCorpus, directories)