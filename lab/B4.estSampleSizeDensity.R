## ---- est_sample_size_density

#==============================================================================#
#                             estSampleSizeDensity                             #
#==============================================================================#
#'  estSampleSizeDensity
#' 
#' This function takes as its parameters, the project meta data and
#' returns an analysis that includes the descriptive statistics
#' for the total corpus and for each register, as well as the 
#' size of the total corpus as well as that of each register.
#' 
#' @param densityAnalysis - the density analysis
#' @param directories - the directory structure
#' @param posTags - the POS tags
#' @return sampleSize - the estimated sample size for the corpus
#' @author John James
#' @export
estSampleSizeDensity <- function(densityAnalysis, directories, posTags) {
  
  startTime <- Sys.time()
  # Message User
  message(paste('\nConducting a density based estimate of sample size at', Sys.time()))
  
  # Compute Sample Size Summary Data
  
  # Determine POS tags accounting for 99% of tags
  wordsTagged <- sum(densityAnalysis$korpusAnalysis$featureStats$total) 
  tags <- subset(densityAnalysis$korpusAnalysis$featureStats, 
                 total >= 0.01 * wordsTagged,
                 select = tag)$tag
  
  # Format sample size parameters
  sampleSize <- list()
  sampleSize$parameters$tags  <- tags
  sampleSize$parameters$total <- 
    round(max(subset(densityAnalysis$korpusAnalysis$featureStats, 
                     tag %in% tags,
                     select = n)$n))
  sampleSize$parameters$base  <- round(sampleSize$parameters$total * .1, 0)
  sampleSize$parameters$pool  <- sampleSize$parameters$total - 
    (sampleSize$parameters$base * length(densityAnalysis$documentAnalyses))
  sampleSize$parameters$sumAvgVc <- 
    sum(unlist(lapply(seq_along(densityAnalysis$documentAnalyses), function(x) {
      as.numeric(densityAnalysis$documentAnalyses[[x]]$avgVc)
    })))
  sampleSize$parameters$x <- sampleSize$parameters$pool / sampleSize$parameters$sumAvgVc
  
  
  # Format sample size summary data
  sampleSize$summary <- 
    rbindlist(lapply(seq_along(densityAnalysis$documentAnalyses), function(x) {
      register <- densityAnalysis$documentAnalyses[[x]]$title
      base <- sampleSize$parameters$base
      avgVc <- densityAnalysis$documentAnalyses[[x]]$avgVc
      x <- round(sampleSize$parameters$x, digits = 0)
      proportional <- round(avgVc * x, digits = 0)
      numSamples <- base + proportional
      sampleLength <- densityAnalysis$korpusAnalysis$sampleLength
      numTokens <- numSamples * sampleLength
      s <- data.frame(register = register, base = base, avgVc = avgVc, x = x, 
                      proportional = proportional, sampleSize = numSamples, 
                      sampleLength = sampleLength, total = numTokens)
      names(s) <- c('Register', 'Base', 'Avg CV', 'x', 'Proportional', 
                    'Sample Size', 'Sample Length', 'Tokens')
      s
    }))
  # Format feature distributions for the corpus
  sampleSize$features$corpus <- 
    subset(densityAnalysis$korpusAnalysis$featureStats, tag %in% tags)
  
  # Format feature distributions for each register
  sampleSize$features$registers <- 
    lapply(seq_along(densityAnalysis$documentAnalyses), function(x) {
      subset(densityAnalysis$documentAnalyses[[x]]$featureStats,
             tag %in% tags)
    })
  
  message(paste('Completing a density based estimate of sample size at', Sys.time()))
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '',''),
                             'density-sample-size-estimate-', 
                             densityAnalysis$metaData$fileName, 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- paste0('densitySampleSizeEst', 
                             densityAnalysis$metaData$fileName)
  output$data  <- sampleSize
  saveObject(output)
  
  # Log Results
  logR('densitySampleSizeEst', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Density Sample Size Estimate Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  return(sampleSize)
}
## ---- end