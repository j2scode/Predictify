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
#' @param regex - the regex patterns
#' @param directories - the project directory structure
#' @author John James
#' @export
mknPipeline <- function(training, test, model, regex, directories) {
  
  startTime <- Sys.time()
  message(paste('\nExecuting', model$mDesc, 'on',
                training$corpusName, 'at', startTime))
  
  gc()
  
  # Initialize MKN language model
  mknInit(model, training$nGrams, regex)
  
  # Create absolute counts of each nGram
  features <- parallelizeTask(mknAbsCount, model, training$nGrams)
  
  # Create continuation counts of each nGram
  parallelizeTask(mknCKN, model, model$mOrder)
  
  # Count nGram histories
  parallelizeTask(mknHistories, model, model$mOrder)
  
  # Calculate discounts
  discounts <- mknDiscount(model)
  
  # Update models with normalizing factors
  parallelizeTask(mknNorm, model, model$mOrder)
  
  # Calculate pseudo probability alpha
  parallelizeTask(mknAlpha, model)
  
  # Compute weighting factor lambda
  parallelizeTask(mknLambda, model, model$mOrder)
  
  # Evaluate Model on Validation Set
  evaluation <- mknEstimate(model,training$processed[[model$mOrder]],
                                test$processed[[model$mOrder]],
                                sents = 1000, directories)
  
  # Format evaluation package
  evalPackage <- list()
  evalPackage$model = model$mDesc
  evalPackage$corpus = training$corpusName
  evalPackage$pct = training$pct
  evalPackage$eval = evaluation

  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('MKN-analysis-package-')),
                             training$fileName,
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
