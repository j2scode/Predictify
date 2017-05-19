## ---- design_pilot
#------------------------------------------------------------------------------#
#                              designPilot                                     #
#------------------------------------------------------------------------------#
#'  designPilot 
#' 
#' This function takes the vocabulary and feature based estimates, the 
#' sampling unit analysis, and the clean data analysis and produces a 
#' list containing a data frame, comparing the two estimates and a data frame 
#' summaring the chosen estimate as the pilot corpus.
#' 
#' @param sampleSizeEstimate - the vocabulary-based estimate
#' @param registerSizeEstimate - the lexical feature based estimate 
#' @param samplingUnit - the sampling unit 
#' @param analysis - the clean data analysis
#' @return alpha - the corpus design for alpha training set.
#' @author John James
#' @export
designPilot <- function(sampleSizeEstimate, registerSizeEstimate, 
                        samplingUnit, analysis) {
  
  startTime <- Sys.time()
  
  message(paste('\nDesigning alpha corpus at', startTime))
  
  #---------------------------------------------------------------------------#
  #                  Compute Estimated Pilot Corpus Size                      #
  #---------------------------------------------------------------------------#
  register <- c(as.character(registerSizeEstimate$Register[1:3]), 'Corpus')
  hcCorpus <- analysis$featureMatrix$tokens
  diversity <- as.vector(t(sampleSizeEstimate[1:4,3]))
  syntactic <- round(registerSizeEstimate$'Sample Size'[1:4], 0)
  tokens <- pmax(diversity, syntactic, na.rm = TRUE)
  sentences <- floor(tokens[1:3] / analysis$featureMatrix$wordsPerSent[1:3])
  sentences <- c(sentences, sum(sentences))              
  pctTotal <- round(tokens / analysis$featureMatrix$tokens[1:4] * 100, 0)
  proportion <- round(tokens[1:3] / sum(tokens[1:3]) * 100, 0)
  proportion <- c(proportion, 100)
  

  comparison <- data.frame(register = register, hcCorpus = hcCorpus,
                       diversity = diversity,
                       syntactic = syntactic, tokens = tokens,
                       sentences = sentences, pctTotal = pctTotal,
                       proportion = proportion)
  
  names(comparison) <- c('Register', 'HC Corpus', 'Diversity-Based Estimate',
                     'Lexical Feature-Based Estimate', 
                     'Tokens', 'Sentences', '% Total', 
                     'Proportion')
  #---------------------------------------------------------------------------#
  #                  Compute Extrapolated Pilot Corpus Size                   #
  #---------------------------------------------------------------------------#
  register <- c(as.character(registerSizeEstimate$Register[1:3]), 'Corpus')
  hcTokens <- analysis$featureMatrix$tokens
  extrapolated <- pctTotal / pctTotal[4] * 5
  extrapolatedTokens <- hcTokens * extrapolated / 100
  extrapolatedSents <- floor(extrapolatedTokens / analysis$featureMatrix$wordsPerSent)
  chunkSize <- rep(samplingUnit[[length(samplingUnit)]]$size, 4)
  sentsPerChunk <- floor(samplingUnit[[length(samplingUnit)]]$size / 
                    analysis$featureMatrix$wordsPerSent)
  chunks <- floor(extrapolatedSents / sentsPerChunk)
  sampleSize <- chunks * sentsPerChunk
  
  pilot <- data.frame(register = register, hcTokens = hcTokens, 
                       extrapolated = extrapolated,
                       extrapolatedTokens = extrapolatedTokens,
                       extrapolatedSents = extrapolatedSents,
                       chunkSize = chunkSize,
                       sentsPerChunk = sentsPerChunk,
                       chunks = chunks,
                       sampleSize = sampleSize)
  names(pilot) <- c('Register', 'HC Corpus (Tokens)',
                     '% Total', 'Tokens', 'Sentences', 
                     'Chunk Size (Tokens)',
                     'Sentences per Chunk', '# Chunks', 
                     'Sample Size (Sentences)')
  
  #---------------------------------------------------------------------------#
  #           Compute Model Corpora (Train, Validation, Test) Sizes           #
  #---------------------------------------------------------------------------#
  # Compute training, validation and test set sizes
  Registers <- c('Blogs', 'News', 'Twitter', 'Corpus')
  Validation <- pilot$`Sample Size (Sentences)`[1:3]
  Test <- pilot$`Sample Size (Sentences)`[1:3]
  Alpha <-  pilot$`Sample Size (Sentences)`[1:3] * 2
  Beta <-  pilot$`Sample Size (Sentences)`[1:3] * c(5,2,5)
  Gamma <-  pilot$`Sample Size (Sentences)`[1:3] * c(10,2,10)
  Delta <-  pilot$`Sample Size (Sentences)`[1:3] * c(14,2,14)
  corpusDesign <- data.frame(Alpha, Beta, Gamma, Delta, Validation, Test)
  ttl <- colSums(corpusDesign)
  corpusDesign <- rbind(corpusDesign, ttl)
  corpusDesign <- as.data.frame(cbind(Registers, corpusDesign), stringsAsFactors=FALSE)
  
  # Format results
  design <- list(
    comparison = comparison,
    pilot = pilot,
    corpusDesign = corpusDesign
  )
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'pilot-corpus-design',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'pilotDesign'
  output$data  <- design
  saveObject(output)
  
  # Log Results
  logR('pilotDesign', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Pilot corpus designed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(design)
}
## ---- end
#css <- estimateregisterSizeEstimate(corpora$clean, posTags, directories)