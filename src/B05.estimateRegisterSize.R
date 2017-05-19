## ---- estimate_register_size
#------------------------------------------------------------------------------#
#                            estimateRegisterSize                              #
#------------------------------------------------------------------------------#
#'  estimateRegisterSize 
#' 
#' This function provides an estimate of  register size for each register 
#' based upon the distribution of lexical  features per 2000-word 
#' samples of the text.
#' 
#' @param korpus - the meta data for the corpus
#' @param corpusSize - the corpus sizes estimate
#' @param samplingUnit - sampling unit analysis
#' @param posTags - selected POS tags
#' @param directories - the project directory structure
#' @return corpusSize - the corpus size estimate with summary tables
#' @author John James
#' @export
estimateRegisterSize <- function(korpus, corpusSize, 
                                 samplingUnit, posTags, directories) {
  
  startTime <- Sys.time()
  
  message(paste('\nEstimating register size at', startTime))
  
  # Initialize key variables
  sampleSize <- 2000
  numSamples <- 100
  posTags <- subset(posTags, Study == TRUE)
  
  # Conduct POS analysis on each register 
  posAnalysis <- lapply(seq_along(korpus$documents), function(x) {
    message(paste('...loading', korpus$documents[[x]]$fileDesc))
    document <- readFile(korpus$documents[[x]])
    
    message('...converting document to word tokens')
    document <- unlist(quanteda::tokenize(document, what = 'word'))
    
    message(paste0('...sampling ', numSamples, ', ', sampleSize, '-word samples'))
    chunks <- sampleData(document, numChunks = numSamples, 
                         chunkSize = sampleSize, format = 'lv')
    
    message('...conducting POS analysis')
    posAnalysis <- analyzeLexicalFeatures(chunks, posTags)
    posAnalysis
  })
  
  # Format parameters based upon corpus size estimate
  total <- corpusSize$n
  base  <- round(total * .1, 0)
  pool  <- total - (base * length(registers))
  sumAvgVc <- 
    sum(unlist(lapply(seq_along(posAnalysis), function(x) {
      as.numeric(posAnalysis[[x]]$avgVc)
    })))
  lambda <- round(pool / sumAvgVc, digits = 0)

  registerSize <- rbindlist(lapply(seq_along(posAnalysis), function(x) {
    register <- registers[[x]]$fileDesc
    avgVc <- posAnalysis[[x]]$avgVc
    proportion <- round(avgVc * lambda, digits = 0)
    numSamples <- base + proportion
    sampleLength <- samplingUnit[[length(samplingUnit)]]$size
    numTokens <- numSamples * sampleLength
    size <- data.frame(register = register, base = base, avgVc = avgVc, 
                       lambda = lambda, proportion = proportion, 
                       numSamples = numSamples, sampleLength = sampleLength, sampleSize = numTokens)
    size
  }))

  # Add summary row to register table
  
  register = 'Corpus'
  base =  sum(registerSize$base)
  avgVc = sum(registerSize$avgVc)
  lambda = mean(as.numeric(registerSize$lambda))
  proportion = sum(registerSize$proportion)
  numSamples = sum(registerSize$numSamples)
  sampleLength = mean(as.numeric(registerSize$sampleLength))
  sampleSize = sum(registerSize$sampleSize)
  lastRow <- data.frame(register = register, base = base, avgVc = avgVc,
                        lambda = lambda, proportion = proportion, 
                        numSamples = numSamples, sampleLength = sampleLength,
                        sampleSize = sampleSize)
  registerSize <- rbind(registerSize, lastRow)  
  
  names(registerSize) <- c('Register', 'Base', 'Avg VC', 'Lambda', 'Proportion', 
                   'Num Samples', 'Sample Length', 'Sample Size')
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'register-sample-size-estimate',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'registerSampleSize'
  output$data  <- registerSize
  saveObject(output)
  
  # Log Results
  logR('registerSampleSize', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Register sample size estimated at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(registerSize)
}
## ---- end
#css <- estimateRegisterSize(corpora$clean, posTags, directories)