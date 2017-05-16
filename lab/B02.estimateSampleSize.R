## ---- create_coverage
#------------------------------------------------------------------------------#
#                                 estimateSampleSize                             #
#------------------------------------------------------------------------------#
#'  estimateSampleSize 
#' 
#' This function takes as its parameter, the zipf objectd, extended SPC object 
#' and the project directory, and creates a data frame that relates coverage to 
#' sample size.
#' 
#' @param zipf = the zipf analysis
#' @param spc - the extended SPC 
#' @param directories - the project directory structure
#' @return coverageTable - coverage table 
#' @author John James
#' @export
estimateSampleSize <- function(zipf, spc, directories) {
  
  startTime <- Sys.time()
  
  message(paste('\nCreating Coverage Table at', startTime))
  

  # Subset Data by Register
  Register <- as.character(unique(spc$Register))
  blogs <- subset(spc, Register == 'Blogs')
  news <- subset(spc, Register == 'News') 
  twit <- subset(spc, Register == 'Twitter') 
  
  # Create table rows
  N <- sum(unlist(lapply(seq_along(zipf), function(x) {
    zipfR::N(zipf[[x]]$docSpc)
  })))
  coverage <- with(blogs, approx(Coverage, Tokens, xout = c(90, 95, 99), rule = 2))
  blogsCoverage <- data.frame(Register = Register[1], t(coverage$y))
  
  coverage <- with(news, approx(Coverage, Tokens, xout = c(90, 95, 99), rule = 2))
  newsCoverage <- data.frame(Register = Register[2], t(coverage$y))
  
  coverage <- with(twit, approx(Coverage, Tokens, xout = c(90, 95, 99), rule = 2))
  twitCoverage <- data.frame(Register = Register[3], t(coverage$y))
  
  coverageTable <- rbind(blogsCoverage, newsCoverage, twitCoverage)
  
  coverageTotal <-data.frame(Register = 'Corpus', X1 = sum(coverageTable[,2]),
                             X2 = sum(coverageTable[,3]), 
                             X3 = sum(coverageTable[,4]))

  coverageTable <- rbind(coverageTable, coverageTotal)
  
  coveragePct <- data.frame(Register = '% HC Corpora', 
                            X1 = coverageTable[4,2] / N * 100,
                            X2 = coverageTable[4,3] / N * 100,
                            X3 = coverageTable[4,4] / N * 100)
  
  coverageTable <- rbind(coverageTable, coveragePct)
  
  names(coverageTable) <- c('Register', '90%', '95%', '99%')
  
  coverageTable[-1,-1] <- round(coverageTable[-1,-1], 1)

  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'lexical-diversity-based-sample-size-estimate',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'coverageTable'
  output$data  <- coverageTable
  saveObject(output)
  
  # Log Results
  logR('estimateSampleSize', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Coverage Table Created at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(coverageTable)
}
## ---- end
