## ---- rq01_pipeline

#------------------------------------------------------------------------------#
#                               rq01Pipeline                                   #
#------------------------------------------------------------------------------#
#'  rq01Pipeline  
#' 
#' This purpose of this function is to address the first research question, 
#' what is the effect of training set size on model performance.  It takes as 
#' its parameters, the corpus design, the clean data analysis, and the corpora
#' meta data. It executes the corpus processing and language modeling pipelines
#' for various sample sizes and returns performance results
#' 
#' @param design - the corpus design
#' @param analysis - the clean corpus analysis
#' @param korpora - the meta data for all project corpora
#' @param samplingUnit - the sampling unit in terms of tokens
#' @param directories - the project directory structure
#' @author John James
#' @export
rq01Pipeline  <- function(design, analysis, korpora, samplingUnit, 
                          regex, directories, mkn) {
  
  startTime <- Sys.time()
  message(paste('\nExecuting RQ01 Pipeline at', startTime))
  
  # Initialize the sample sizes for which the analysis will be done
  pctSamples <- c(5,10,20,35, 50)
  
  # Cycle through training sets of various sizes
  evaluations <- rbindlist(lapply(seq_along(pctSamples), function(s) {
  
    # Building training corpus
    parallelizeTask(buildTraining, design, korpora$clean, korpora$training, 
                    analysis, pctSamples[s], samplingUnit)
    

    # Preprocess Corpora - Treat OOV Words
    oov <- parallelizeTask(preprocessCorpora, korpora, directories)
    
    # Process Corpora - Create multigram texts with appropriate sentence boundaries
    parallelizeTask(processCorpora, korpora)
    
    # Get training corpus descriptive statistics
    message('Obtaining descriptive statistics for training corpus')
    trainingStats <- list()
    korpus <- readFile(korpora$training$processed$text$quadgram)
    trainingStats$sents <- length(korpus)
    df <- quanteda::dfm(korpus, tolower = FALSE, remove = 'BOS')
    trainingStats$V <- length(featnames(df))
    trainingStats$N <- sum(ntoken(df))
    trainingStats <- rbindlist(list(trainingStats))
    
    # Create N-Grams for Corpora
    nGramSummary <- parallelizeTask(nGramCorpora, korpora, directories)
    
    # Initialize MKN language model
    mknInit(mkn, korpora$training$nGrams$text, regex)
    
    # Create absolute counts of each nGram
    features <- parallelizeTask(mknAbsCount, mkn, korpora$training$nGrams$text)
    
    # Create continuation counts of each nGram
    parallelizeTask(mknCKN,mkn)
    
    # Count nGram histories
    parallelizeTask(mknHistories,mkn)
    
    # Calculate discounts
    discounts <- mknDiscount(mkn)
    
    # Update models with normalizing factors
    parallelizeTask(mknNorm,mkn)
    
    # Calculate pseudo probability alpha
    parallelizeTask(mknAlpha, mkn)
    
    # Compute weighting factor lambda
    parallelizeTask(mknLambda, mkn)
    
    # Evaluate Model on Validation Set
    evaluation <- parallelizeTask(mknEstimate, mkn, 
                                  korpora$training$processed$text$quadgram, 
                                  korpora$validation$processed$text$quadgram,
                                  sents = 1000, directories)
    
    # Format evaluation package
    evalPackage <- list()
    evalPackage$pct = pctSamples[s]
    evalPackage$training = trainingStats
    evalPackage$oov = oov
    evalPackage$nGramSummary = nGramSummary
    evalPackage$eval = evaluation
    
    # Save Analysis
    output <- list()
    output$directory <- directories$analysisDir
    output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                               paste0('RQ01-analysis-package-',pctSamples[s],'-pct'),
                               format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    output$objName   <- 'evalPackage'
    output$data  <- evalPackage
    saveObject(output)

    # Create evaluation summary    
    evalSummary <- list()
    evalSummary$corpusPct <- evalPackage$pct
    evalSummary$modelSize <- evalPackage$eval$summary$size
    evalSummary$duration  <- evalPackage$eval$summary$duration
    evalSummary$unigrams <- features[1,2]$Count
    evalSummary$bigrams <- features[2,2]$Count
    evalSummary$trigrams <- features[3,2]$Count
    evalSummary$quadgrams <- features[4,2]$Count
    evalSummary$trainingSents <- trainingStats$sents
    evalSummary$trainingWords <- trainingStats$N
    evalSummary$testSet <- korpora$validation$corpusName
    evalSummary$testSetSents <- evalPackage$eval$summary$sentences
    evalSummary$testSetWords <- evalPackage$eval$summary$words
    evalSummary$perplexity <- evalPackage$eval$summary$perplexity
    
    evalSummary
  }))

  # Save Analysis Summary
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'RQ01-analysis-summary',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'evalSummary'
  output$data  <- evaluations
  saveObject(output)
  
  # Log Results
  logR('rq01Pipeline', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('RQ01 Pipeline Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(evaluations)
}
## ---- end
