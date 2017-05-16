## ---- create_sample_size

#==============================================================================#
#                            calcSampleSizes                                   #
#==============================================================================#
#'  calcSampleSizes 
#' 
#' This function calculates sample size for a register based upon the 
#' zipf data from the vocabulary analysis
#' 
#' @param zipf - the zipf data from the vocabulary analysis
#' @return sampleSize - the sample size in types
#' @author John James
#' @export
calcSampleSizes <- function(zipf) {
  
  message(paste("......", zipf$category))
  
  # Estimate the spectrum for the corpus register
  n <-  zipf$nTokens
  oov <- n * .05
  mMax <- 200
  if (n < 77330) { mMax <- 100 } # Gross Kluge
  docLnreSpc <- lnre.spc(zipf$docLnre, N(zipf$docLnre), m.max = mMax)
  
  # Determine the number of types required to meet OOV threshold
  freqClasses <- zipf$docTfl$k
  frequencies <- zipf$docTfl$f
  freqSpec    <- data.frame(freqClasses = freqClasses, frequencies = frequencies)
  freqSpec    <- freqSpec[order(frequencies, freqClasses),]
  i <- 1
  while(sum(Vm(docLnreSpc, 1:i) * (1:i)) < oov) {
    i <- i + 1
  }
  sampleSize <- list()
  sampleSize$docLnreSpc  <- docLnreSpc
  sampleSize$category   <- zipf$category
  sampleSize$N          <- N(zipf$docSpc)
  sampleSize$V          <- V(zipf$docSpc)
  sampleSize$NSeen      <- N(zipf$docSpc) * .95
  sampleSize$OOV        <- N(zipf$docSpc) * .05
  sampleSize$freqClasses <- i
  sampleSize$Voov       <- sum(Vm(docLnreSpc, 1:i))
  sampleSize$Noov       <- sum(Vm(docLnreSpc, 1:i) * (1:i))
  sampleSize$EV <- EV(zipf$docLnre, N(zipf$docLnre)) - sum(Vm(docLnreSpc, 1:i))
  sampleSize$EN <- head(subset(zipf$docExtVgc, V > sampleSize$EV, select = N), 1)
  
  return(sampleSize)
}  

#==============================================================================#
#                           createSampleSize                                  #
#==============================================================================#
#'  createSampleSize
#' 
#' This function calculates the sample size based upon Zipf vocabulary growth
#' data
#' 
#' @param diversityAnalysis - the diversity analysis
#' @param directories - the directory structure
#' @return sampleSizeData - list containing sample size estimates, as well as 
#'          document feature matrices, frequency spectrums, and vocabulary
#'          growth data.
#' @author John James
#' @export
createSampleSize <- function(zipf, directories) {
  
  startTime <- Sys.time()
  message(paste('\nEstimating sample size at', startTime))

  sampleSizeData <- lapply(seq_along(zipf), function(x) {
    calcSampleSizes(zipf[[x]])
  })
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'sample-size-estimate', 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- paste0('sampleSizeEstimate')
  output$data  <- sampleSizeData
  saveObject(output)
  
  # Log Results
  logR('sampleSizeEstimate', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Sample Size Estimate Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(sampleSizeData)
}
## ---- end