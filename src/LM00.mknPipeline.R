## ---- mkn_pipeline

#------------------------------------------------------------------------------#
#                               mknPipeline                                    #
#------------------------------------------------------------------------------#
#'  mknPipeline  
#' 
#' This function executes the pipeline for the Modified Kneser-Ney Trigram
#' and Quadgram Models.  It processes training corpora of various sizes and 
#' reports the perplexity of the designated test set. 
#' 
#' @param training - the meta data for the training set being modeled
#' @param test - the meta data for the test set 
#' @param model - the meta data for the model
#' @param analysis - the training set coverage analyses
#' @param regex - the regex patterns
#' @param directories - the project directory structure
#' @author John James
#' @export
mknPipeline <- function(training, test, model, analysis, regex, directories) {
  
  startTime <- Sys.time()
  message(paste('\nExecuting', model$args$mDesc, 'on',
                training$corpusName, 'at', startTime))
  
  gc()
  
  # Preorocess OOV     
  oov <- parallelizeTask(preprocessCorpora, training, test, directories)  
  
  # Process text, creating sentence boundary tokens for nGram order 1-4
  parallelizeTask(processCorpus, training)
  parallelizeTask(processCorpus, test)
  
  # Create nGrams
  nGrams <- parallelizeTask(createNGrams, training, directories)
  
  # Initialize MKN language model
  mknInit(model, training$nGrams$words, regex)
  
  # Create absolute counts of each nGram
  features <- parallelizeTask(mknAbsCount, model, training$nGrams$words)
  
  # Create continuation counts of each nGram
  parallelizeTask(mknCKN, model, model$args$mOrder)
  
  # Count nGram histories
  parallelizeTask(mknHistories, model, model$args$mOrder)
  
  # Calculate discounts
  discounts <- mknDiscount(model)
  
  # Update models with normalizing factors
  parallelizeTask(mknNorm, model, model$args$mOrder)
  
  # Calculate pseudo probability alpha
  parallelizeTask(mknAlpha, model)
  
  # Compute weighting factor lambda
  parallelizeTask(mknLambda, model, model$args$mOrder)
  
  # Evaluate Model on Validation Set
  evaluation <- parallelizeTask(mknEstimate, model, 
                                training$processed$words[[4]], 
                                test$processed$words[[model$args$mOrder]],
                                sents = 1000, directories)
  
  # Format evaluation package
  evalPackage <- list()
  evalPackage$pct = training$pct
  evalPackage$training = analysis
  evalPackage$eval = evaluation
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             paste0('MKN-analysis-package-',training$pct,'-pct'),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'evalPackage'
  output$data  <- evalPackage
  saveObject(output)

  # Log Results
  logR('mknPipeline', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('MKN Pipeline Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(evalPackage)
}
## ---- end
