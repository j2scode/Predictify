## ---- verify_corpus
#==============================================================================#
#                              verifyCorpora                                    #
#==============================================================================#
#'  verifyCorpora
#' 
#' This function compares the lexical feature distribution of the test corpus
#' with that of the model training corpus.
#' 
#' @param trainDensityAnalysis - the density analysis for the training corpus
#' @param testDensityAnalysis - the density analysis for the test corpus
#' @return comparison - results from chi-squared test
#' @author John James
#' @export
verifyCorpora <- function(trainDensityAnalysis, testDensityAnalysis) {
  
  startTime <- Sys.time()
  
  message(paste("\nComparing master and model corpora at", startTime))
  
  t <- as.data.frame(testDensityAnalysis$korpusAnalysis$featureStats$tag)
  d <- as.data.frame(testDensityAnalysis$korpusAnalysis$featureStats$Description)
  e <- testDensityAnalysis$korpusAnalysis$featureStats$mean
  o <- trainDensityAnalysis$korpusAnalysis$featureStats$mean
  c <- cbind(e, o)
  x2 <- chisq.test(c)
  c <- cbind(t, d, e, o)
  names(c) <- c('Tag', 'POS', 'Null', 'Observed')
  result <- list()
  result$data <- c
  result$test <- x2
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Corpora comparison Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(result)  
  
}
## ---- end