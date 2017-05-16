## ---- analyze_sampling_unit
#------------------------------------------------------------------------------#
#                            estimateSamplingUnit                              #
#------------------------------------------------------------------------------#
#'  estimateSamplingUnit 
#' 
#' This function takes the meta data for the corpus to be sampled, 
#' the meta data for the registers, the posTags,
#' and the directory structure and compares the distributions of lexical features 
#' across pairs of samples of varying sizes. The results of chi-squared tests 
#' for selected features are averaged over the samples.  The function returns 
#' a data frame indicating average chi-squared p-values for each feature and 
#' sampling unit size.
#' 
#' @param korpus - the meta data for the corpus
#' @param registers - the meta data for the registers
#' @param posTags - selected POS tags
#' @param directories - the project directory structure
#' @return analysis - the sampling unit size analysis.
#' @author John James
#' @export
estimateSamplingUnit <- function(korpus, registers, posTags, directories) {
  
  startTime <- Sys.time()
  
  message(paste('\nAnalyzing Sampling Unit at', startTime))
  
  # Designate sample sizes
  sampleSizes <- c(2000)
  numSamples <- 10
  
  # Designate POS Tags to include in study
  posTags <- subset(posTags, Study == TRUE)
  
  filePath <- list()
  filePath$directory <- korpus$directory
  analysis <- lapply(seq_along(sampleSizes), function(s) {
    message(paste('...processing', sampleSizes[s], 'token samples'))
    scores <- rbindlist(lapply(seq_along(registers), function(x) {
      message(paste('......processing', registers[[x]]$fileDesc))
      filePath$fileName <- registers[[x]]$fileName
      document <- tolower(readFile(filePath))
      tokens <- unlist(quanteda::tokenize(document, what = "word"))
      midway <- floor(length(tokens)/2)
      
      # Process Sample A
      sampleA <- sampleData(tokens[1:midway], numChunks = numSamples, 
                            chunkSize = sampleSizes[s], format = 'lv')
      distA <- data.table(analyzeLexicalFeatures(sampleA, posTags)$featureMatrix)
      distA <- as.data.table(t(distA), keep.rownames = TRUE)
      setnames(distA, 'rn', 'Tag')
      
      # Process Sample B
      sampleB <- sampleData(tokens[(midway+1):length(tokens)], numChunks = numSamples, 
                            chunkSize = sampleSizes[s], format = 'lv')
      distB <- data.table(analyzeLexicalFeatures(sampleB, posTags)$featureMatrix)
      distB <- as.data.table(t(distB), keep.rownames = TRUE)
      setnames(distB, 'rn', 'Tag')
      
      # Create POS Table
      set.seed(230)
      posDt <- data.table(Tag = posTags$Tag)
      
      # Set Keys
      setkey(posDt, Tag)
      setkey(distA, Tag)
      setkey(distB, Tag)
      distA <- merge(posDt, distA, by='Tag', all.x = TRUE)
      distB <- merge(posDt, distB, by='Tag', all.x = TRUE)
      distA[is.na(distA)] <- 0
      distB[is.na(distB)] <- 0
      
      # Iterate through rows and run Chi-Sq Tests on distributions
      x2 <- rbindlist(lapply(seq_along(1:nrow(distA)), function(d) {
        a <- as.vector(t(distA[d,-1]))
        b <- as.vector(t(distB[d,-1]))
        dist <- data.frame(a = a, b = b)
        
        x2 <- list()
        x2$Size <- sampleSizes[s]
        x2$Register <- registers[[x]]$fileDesc
        x2$Pos      <- as.character(distA[d][,Tag])
        x2$Score <- chisq.test(dist)$p.value
        x2
      }))
      x2
    }))
    analysis <- list()
    analysis$size <- sampleSizes[s]
    analysis$long <- scores
    analysis$wide <- dcast(scores, Register + Size~Pos, value.var = "Score")
    
    # Create useful summary information
    # Total row with column means
    cm1 <- as.data.frame(t(colMeans(analysis$wide[,3:ncol(analysis$wide)], na.rm = TRUE)))
    cm2 <- data.frame(Register = 'Corpus Mean', Size = sampleSizes[s])
    cm1 <- cbind(cm2, cm1)
    analysis$wide <- rbind(analysis$wide, cm1)
  
    # Total column with row means
    rm <- data.frame(Mean = rowMeans(analysis$wide[,3:ncol(analysis$wide)], na.rm = TRUE))
    analysis$wide <- cbind(analysis$wide, rm)

    # Tags varying above alpha = .05 showing too much variation (tmv) for sample size 
    analysis$tmv <- merge(as.data.table(subset(analysis$long, Score < 0.05 & Size == sampleSizes[s])),
                          as.data.table(subset(posTags, select = c(Tag, Description))),
                          by.x = 'Pos', by.y = 'Tag')[,.(Description, Score, Register)]
    
    # Tags that vary the most
    analysis$mvt <- merge(as.data.table(subset(analysis$long, Size == sampleSizes[s] & 
                                        Score == min(subset(analysis$long, 
                                                            select = Score)))),
                 as.data.table(subset(posTags, select = c(Tag, Description))),
                 by.x = 'Pos', by.y = 'Tag')[,.(Description, Score, Register)]
    # Mean of all tags
    analysis$mean <- mean(as.numeric(analysis$wide[nrow(analysis$wide),
                                                   3:ncol(analysis$wide)]), 
                          na.rm = TRUE)
    analysis
  })
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'estimate-sampling-unit',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'samplingUnit'
  output$data  <- analysis
  saveObject(output)
  
  # Log Results
  logR('estimateSamplingUnit', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Sampling units analyzed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(analysis)
}
## ---- end
#sua <- estimateSamplingUnit(corpora$reshaped, posTags, directories)