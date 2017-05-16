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
#' @param vocabularyEstimate - the vocabulary-based estimate
#' @param featureEstimate - the lexical feature based estimate 
#' @param samplingUnit - the sampling unit 
#' @param analysis - the clean data analysis
#' @return alpha - the corpus design for alpha training set.
#' @author John James
#' @export
designPilot <- function(vocabularyEstimate, featureEstimate, 
                        samplingUnit, analysis) {
  
  startTime <- Sys.time()
  
  message(paste('\nDesigning alpha corpus at', startTime))
  
  #---------------------------------------------------------------------------#
  #                  Compute Estimated Pilot Corpus Size                      #
  #---------------------------------------------------------------------------#
  register <- c(as.character(featureEstimate$Register[1:3]), 'Corpus')
  hcCorpus <- analysis$featureMatrix$tokens
  diversity <- as.vector(t(vocabularyEstimate[1:4,3]))
  syntactic <- round(featureEstimate$'Sample Size'[1:4], 0)
  tokens <- pmax(diversity, syntactic, na.rm = TRUE)
  sentences <-round(tokens[1:3] / analysis$featureMatrix$wordsPerSent[1:3], 0)
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
  register <- c(as.character(featureEstimate$Register[1:3]), 'Corpus')
  hcTokens <- analysis$featureMatrix$tokens
  extrapolated <- pctTotal / pctTotal[4] * 5
  extrapolatedTokens <- hcTokens * extrapolated / 100
  extrapolatedSents <- ceiling(extrapolatedTokens / analysis$featureMatrix$wordsPerSent)
  chunkSize <- rep(samplingUnit[[length(samplingUnit)]]$size, 4)
  sentsPerChunk <- ceiling(samplingUnit[[length(samplingUnit)]]$size / 
                    analysis$featureMatrix$wordsPerSent)
  chunks <- ceiling(extrapolatedSents / sentsPerChunk)
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
  validationSet <- pilot$`Sample Size (Sentences)`[1:3]
  testSet <- pilot$`Sample Size (Sentences)`[1:3]
  multipliers <- c(2,4,7,10)
  trainingSets <- tcrossprod(pilot$`Sample Size (Sentences)`[1:3], multipliers)
  
  # Format Corpus Design
  corporaDesign <- as.data.frame(cbind(trainingSets, validationSet, testSet))
  corporaDesign <- rbind(corporaDesign, colSums(corporaDesign))
  names(corporaDesign) <- c('Alpha', 'Beta', 'Gamma', 'Delta', 'Validation', 'Test')
  
  # Correct for over allocation
  hcCorpus <- comparison$`HC Corpus`
  for (i in 1:(nrow(corporaDesign)-1)) {
    for (j in 1:ncol(corporaDesign)) {
      tokens <- corporaDesign[i,j] * round(analysis$featureMatrix$wordsPerSent[i],0)
      if (tokens >= hcCorpus[i]) {
        corporaDesign[i,j] = floor(hcCorpus[i] / chunkSize[i] * sentsPerChunk[i])
      }
    }
  }
  
  # Reallocate shortfalls
  blogTwitterTotal <- comparison$Proportion[1]  + comparison$Proportion[3]
  blogProportion <- comparison$Proportion[1] / blogTwitterTotal
  totals <- colSums(corporaDesign[1:3,])
  shortfall <- corporaDesign[4,] - totals
  shortfallBlogs <- shortfall * blogProportion
  shortfallTwitter <- shortfall - shortfallBlogs
  blogAdjustment <- floor(shortfallBlogs /  sentsPerChunk[1] * sentsPerChunk[1])
  twitAdjustment <- floor(shortfallTwitter / sentsPerChunk[3] * sentsPerChunk[3])
  corporaDesign[1,3] <- corporaDesign[1,3] + blogAdjustment$Gamma
  corporaDesign[1,3] <- corporaDesign[1,3] + blogAdjustment$Delta
  corporaDesign[3,3] <- corporaDesign[3,3] + twitAdjustment$Gamma
  corporaDesign[3,3] <- corporaDesign[3,3] + blogAdjustment$Delta
  Registers <- c('Blogs Register', 'News Register', 'Twitter Register', 'Corpus')
  corporaDesign <- cbind(Registers, corporaDesign)
  
  
  # Format results
  design <- list(
    comparison = comparison,
    pilot = pilot,
    corporaDesign = corporaDesign
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
#css <- estimatefeatureEstimate(corpora$clean, posTags, directories)