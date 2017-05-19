## ---- estimate_sample_size

#------------------------------------------------------------------------------#
#                                   trainLNRE                                  #
#------------------------------------------------------------------------------#
#'  trainLNRE
#' 
#' This function trains an LNRE model based upon the model number and
#' the frequency spectrum object parameters. 
#' 
#' @param modelNum - the model number 
#' @param sampleDocSpc - the document frequency spectrum
#' @return docLnre - the LNRE MODEL
#' @author John James
#' @export
trainLNRE <- function(modelNum, sampleDocSpc) {
  
  switch(modelNum,
         '1'	 = lnre('zm', spc = sampleDocSpc, cost = 'chisq', method = 'Custom', exact  = FALSE),
         '2'	 = lnre('zm', spc = sampleDocSpc, cost = 'chisq', method = 'Custom', exact  = FALSE),
         '3'	 = lnre('zm', spc = sampleDocSpc, cost = 'chisq', method = 'Custom', exact  = FALSE),
         '4'	 = lnre('zm', spc = sampleDocSpc, cost = 'chisq', method = 'Custom'),
         '5'	 = lnre('zm', spc = sampleDocSpc, cost = 'chisq', method = 'Nelder-Mead', exact  = FALSE),
         '6'	 = lnre('zm', spc = sampleDocSpc, cost = 'chisq', method = 'Nelder-Mead'),
         '7'	 = lnre('zm', spc = sampleDocSpc, cost = 'chisq', method = 'SANN', exact  = FALSE),
         '8'	 = lnre('zm', spc = sampleDocSpc, cost = 'chisq', method = 'SANN'),
         '9'	 = lnre('zm', spc = sampleDocSpc, cost = 'linear', method = 'Custom', exact  = FALSE),
         '10'	 = lnre('zm', spc = sampleDocSpc, cost = 'linear', method = 'Custom'),
         '11'	 = lnre('zm', spc = sampleDocSpc, cost = 'linear', method = 'Nelder-Mead', exact  = FALSE),
         '12'	 = lnre('zm', spc = sampleDocSpc, cost = 'linear', method = 'Nelder-Mead'),
         '13'	 = lnre('zm', spc = sampleDocSpc, cost = 'linear', method = 'SANN', exact  = FALSE),
         '14'	 = lnre('zm', spc = sampleDocSpc, cost = 'linear', method = 'SANN'),
         '15'	 = lnre('zm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'Custom', exact  = FALSE),
         '16'	 = lnre('zm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'Custom'),
         '17'	 = lnre('zm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'NLM', exact  = FALSE),
         '18'	 = lnre('zm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'NLM'),
         '19'	 = lnre('zm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'Nelder-Mead', exact  = FALSE),
         '20'	 = lnre('zm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'Nelder-Mead'),
         '21'	 = lnre('zm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'SANN', exact  = FALSE),
         '22'	 = lnre('zm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'SANN'),
         '23'	 = lnre('zm', spc = sampleDocSpc, cost = 'mse', method = 'Custom', exact  = FALSE),
         '24'	 = lnre('zm', spc = sampleDocSpc, cost = 'mse', method = 'Custom'),
         '25'	 = lnre('zm', spc = sampleDocSpc, cost = 'mse', method = 'NLM', exact  = FALSE),
         '26'	 = lnre('zm', spc = sampleDocSpc, cost = 'mse', method = 'NLM'),
         '27'	 = lnre('zm', spc = sampleDocSpc, cost = 'mse', method = 'Nelder-Mead', exact  = FALSE),
         '28'	 = lnre('zm', spc = sampleDocSpc, cost = 'mse', method = 'Nelder-Mead'),
         '29'	 = lnre('zm', spc = sampleDocSpc, cost = 'mse', method = 'SANN', exact  = FALSE),
         '30'	 = lnre('zm', spc = sampleDocSpc, cost = 'mse', method = 'SANN'),
         '31'	 = lnre('zm', spc = sampleDocSpc, cost = 'exact', method = 'Custom', exact  = FALSE),
         '32'	 = lnre('zm', spc = sampleDocSpc, cost = 'exact', method = 'Custom'),
         '33'	 = lnre('zm', spc = sampleDocSpc, cost = 'exact', method = 'NLM', exact  = FALSE),
         '34'	 = lnre('zm', spc = sampleDocSpc, cost = 'exact', method = 'NLM'),
         '35'	 = lnre('zm', spc = sampleDocSpc, cost = 'exact', method = 'Nelder-Mead', exact  = FALSE),
         '36'	 = lnre('zm', spc = sampleDocSpc, cost = 'exact', method = 'Nelder-Mead'),
         '37'	 = lnre('zm', spc = sampleDocSpc, cost = 'exact', method = 'SANN', exact  = FALSE),
         '38'	 = lnre('zm', spc = sampleDocSpc, cost = 'exact', method = 'SANN'),
         '39'	 = lnre('fzm', spc = sampleDocSpc, cost = 'chisq', method = 'Custom', exact  = FALSE),
         '40'	 = lnre('fzm', spc = sampleDocSpc, cost = 'chisq', method = 'Nelder-Mead'),
         '41'	 = lnre('fzm', spc = sampleDocSpc, cost = 'chisq', method = 'SANN', exact  = FALSE),
         '42'	 = lnre('fzm', spc = sampleDocSpc, cost = 'chisq', method = 'SANN'),
         '43'	 = lnre('fzm', spc = sampleDocSpc, cost = 'linear', method = 'Custom', exact  = FALSE),
         '44'	 = lnre('fzm', spc = sampleDocSpc, cost = 'linear', method = 'Custom'),
         '45'	 = lnre('fzm', spc = sampleDocSpc, cost = 'linear', method = 'Nelder-Mead', exact  = FALSE),
         '46'	 = lnre('fzm', spc = sampleDocSpc, cost = 'linear', method = 'Nelder-Mead'),
         '47'	 = lnre('fzm', spc = sampleDocSpc, cost = 'linear', method = 'SANN', exact  = FALSE),
         '48'	 = lnre('fzm', spc = sampleDocSpc, cost = 'linear', method = 'SANN'),
         '49'	 = lnre('fzm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'Custom', exact  = FALSE),
         '50'	 = lnre('fzm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'Nelder-Mead', exact  = FALSE),
         '51'	 = lnre('fzm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'Nelder-Mead'),
         '52'	 = lnre('fzm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'SANN', exact  = FALSE),
         '53'	 = lnre('fzm', spc = sampleDocSpc, cost = 'smooth.linear', method = 'SANN'),
         '54'	 = lnre('fzm', spc = sampleDocSpc, cost = 'mse', method = 'Nelder-Mead', exact  = FALSE),
         '55'	 = lnre('fzm', spc = sampleDocSpc, cost = 'mse', method = 'Nelder-Mead'),
         '56'	 = lnre('fzm', spc = sampleDocSpc, cost = 'mse', method = 'SANN', exact  = FALSE),
         '57'	 = lnre('fzm', spc = sampleDocSpc, cost = 'mse', method = 'SANN'),
         '58'	 = lnre('fzm', spc = sampleDocSpc, cost = 'exact', method = 'Nelder-Mead', exact  = FALSE),
         '59'	 = lnre('fzm', spc = sampleDocSpc, cost = 'exact', method = 'Nelder-Mead'),
         '60'	 = lnre('fzm', spc = sampleDocSpc, cost = 'exact', method = 'SANN', exact  = FALSE),
         '61'	 = lnre('fzm', spc = sampleDocSpc, cost = 'exact', method = 'SANN')
         
         
  )
  
}
  

#------------------------------------------------------------------------------#
#                                 estimateSampleSize                           #
#------------------------------------------------------------------------------#
#'  estimateSampleSize 
#' 
#' This function uses LNRE models from the zipfR package to calculate and
#' evaluate various samples sizes and associated OOV Rates.
#' 
#' @param korpus - the meta data for the HC Corpus
#' @param directories - the project directory structure
#' @return estimate - list of data frames containing coverage estimates 
#' @author John James
#' @export
estimateSampleSize <- function(korpus, directories) {
  
  startTime <- Sys.time()
  message(paste('\nEstimating vocabulary-based Sample size', startTime))
  
  # Designate sample sizes in terms of percent HC Corpora registers
  sampleSizes <- c(.1,.5,1,3,5,10,15)

  # Iterate through registers, samples sizes, and coverages 
  coverage <- rbindlist(lapply(seq_along(korpus$documents), function(d) {
    message(paste('...loading', korpus$documents[[d]]$fileDesc))
    document <- readFile(korpus$documents[[d]])
    
    message(paste('...tokenizing', korpus$documents[[d]]$fileDesc))
    tokens <- unlist(tokenize(document, what = 'word'))
    
    message(paste('...creating spectrum object for', korpus$documents[[d]]$fileDesc))
    hcDocSpc  <- text2spc.fnc(tokens)
    hcDocSpc$m  <- as.integer(hcDocSpc$m)
    hcDocSpc$Vm <- as.integer(hcDocSpc$Vm)
    hcDocV <- V(hcDocSpc)
    hcDocN <- N(hcDocSpc)
    hcDocSpc$Voov <- cumsum(hcDocSpc$Vm)
    hcDocSpc$Noov <- cumsum(hcDocSpc$Vm * hcDocSpc$m)
    
    
    # Iterate through sample sizes
    registerAnalysis <- rbindlist(lapply(seq_along(sampleSizes), function(s) {
      message(paste('......processing', sampleSizes[s], 'percent of the document'))
      
      # Sample document
      numSamples <- floor(hcDocN * sampleSizes[s] / 100)
      sampleSize <- 1
      sampleDoc <- sampleData(tokens, numChunks = numSamples, chunkSize = sampleSize, format = 'v')

      # Create sample document observed spectrum object
      sampleDocSpc    <- text2spc.fnc(sampleDoc)
      sampleDocSpc$m  <- as.integer(sampleDocSpc$m)
      sampleDocSpc$Vm <- as.integer(sampleDocSpc$Vm)

      # Obtain vocabulary size for sample data
      vSample <- V(sampleDocSpc)
      
      # Train ZM model on sample data.
      docLnre <- lnre('zm', spc = sampleDocSpc, cost = 'chisq', method = 'Custom', exact  = FALSE)
      
      # Extrapolate spectrum for zm model out to sizes of original document
      extSpc <- lnre.spc(docLnre, hcDocN, m.max = nrow(hcDocSpc))
      extSpc$m  <- as.integer(extSpc$m)
      extSpc$Vm <- as.integer(extSpc$Vm)
      extSpc$Voov <- cumsum(extSpc$Vm)
      extSpc$Noov <- cumsum(extSpc$Vm * extSpc$m)
      
      # Extrapolate out to size of original document and return vocabulary size
      vExt <- EV(docLnre, hcDocN)
      
      # Calculate estimated Voov vis-a-vis vocabulary calculated from observed spectrum.
      eVoov <- vExt - vSample
      
      # Calculate estimated Noov
      eNoov <- head(subset(extSpc, extSpc$Voov > eVoov, select = Noov)$Noov, 1)
      
      # Calculate OOV Rate in percent
      oovRate <- eNoov / hcDocN * 100
      
      # Calculate Coverage
      coverage <- 100 - oovRate
      
      # Format output
      result <- list()
      result$register <- korpus$documents[[d]]$fileDesc
      result$percent  <- sampleSizes[s]
      result$size     <- numSamples
      result$vSample <- vSample
      result$vExt <- vExt
      result$eVoov <- eVoov
      result$eNoov <- eNoov
      result$oovRate <- oovRate
      result$coverage <- coverage
      result
    }))
    registerAnalysis
  }))
  
  
  
  # Summarize estimates at approximately 95% coverage
  ss <- subset(coverage, coverage > 96 & percent > 1)
  ss <- data.table(ss)[, .SD[which.min(coverage)], by = register]
  ss <- data.frame(ss)
  
  # Format Summary Row
  register <- 'Corpus'
  percent <- mean(ss$percent)
  size <- sum(ss$size)
  vSample <- sum(ss$vSample)
  vExt <- sum(ss$vExt)
  eVoov <- sum(ss$eVoov)
  eNoov <- sum(ss$eNoov)
  oovRate <- mean(ss$oovRate)
  cover <- mean(ss$coverage)
  ttl <- data.frame(register = register, percent = percent, size = size, 
                    vSample = vSample, vExt = vExt, eVoov = eVoov,
                    eNoov = eNoov, oovRate = oovRate, coverage = cover,
                    stringsAsFactors = FALSE)
  ss <- rbind(ss, ttl)
  
  
  # Format Results
  estimates <- list()
  estimates$coverage <- coverage
  estimates$sampleSize <- ss
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'sample-size-estimate',
                            format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'sampleSizeEstimate'
  output$data  <- estimates
  saveObject(output)
  
  # Log Results
  logR('estimateSampleSize', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Vocabulary-based sample size estimate complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(estimates)
}
## ---- end
#estimates <- estimateSampleSize(corpora$clean, directories)