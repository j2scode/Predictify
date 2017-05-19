## ---- analyze_diversity

#==============================================================================#
#                                repackData                                    #
#==============================================================================#
#'  repackData
#' 
#' The function repacks the data by object type (not register)
#' 
#' @param analysis - the diversity analysis formatted by register
#' @return diversity - the diversity analysis formatted by object type
#' @author John James
#' @export
repackData <- function(korpus, analysis) {
  
  metaData <- list()
  metaData$corporaName <- korpus$corporaName
  metaData$objName     <- korpus$objName
  metaData$fileName    <- korpus$fileName
  
  # Combine growth objects into a list
  growth <- lapply(seq_along(analysis), function(x) {
    analysis[[x]]$growth
  })
  
  # Combine diversity measures by register into single data frame
  measures <- list()
  for (i in 1:8) {
    measures[[i]] <- rbindlist(lapply(seq_along(analysis), function(x) {
      analysis[[x]]$measures[[i]]
    }))
  }
  
  # Combine growth curves into a list
  zipf <- lapply(seq_along(analysis), function(x) {
    analysis[[x]]$zipf
  })
  
  diversity <- list(
    metaData = metaData,
    growth = growth,
    measures = measures,
    zipf = zipf)
  
  return(diversity)
  
}


#==============================================================================#
#                           measureDiversity                                   #
#==============================================================================#
#'  measureDiversity
#' 
#' The function returns of diversity measures for a document
#' 
#' @param register - the name of the document register
#' @param growth - the growth object for the document
#' @return measures - data frames containing diversity data
#' @author John James
#' @export
measureDiversity <- function(register, growth) {
  
  # Preparing type token measures
  typeToken <- data.frame("Register" = register, 
                          "Tokens" = growth@data$data$Tokens,
                          "Types" = growth@data$data$Types)
  
  # Preparing growth rate measures
  growthRate <- data.frame("Register" = register, 
                           "Tokens" = growth@data$data$Tokens,
                           "Growth Rate" = growth@data$data$HapaxLegomena/
                             growth@data$data$Tokens)
  
  
  # Preparing type token ratio measures
  typeTokenRatio <- data.frame("Register" = register, 
                               "Tokens" = growth@data$data$Tokens,
                               "Type-Token Ratio" = growth@data$data$TypeTokenRatio)
  
  
  # Preparing mean log frequency measures
  meanLog <- data.frame("Register" = register, 
                        "Tokens" = growth@data$data$Tokens,
                        "Mean Log Frequency" = growth@data$data$Lognormal)
  
  
  # Preparing Herdan's C measures
  herdan <- data.frame("Register" = register, 
                       "Tokens" = growth@data$data$Tokens,
                       "Herdan's C" = growth@data$data$Herdan)
  
  
  # Preparing Guiraud's R measures
  guiraud <- data.frame("Register" = register, 
                        "Tokens" = growth@data$data$Tokens,
                        "Guiraud's R" = growth@data$data$Guiraud)
  
  
  # Preparing Yule's K measures
  yule <- data.frame("Register" = register, 
                     "Tokens" = growth@data$data$Tokens,
                     "Yule's K" = growth@data$data$Yule)
  
  
  # Preparing Zipf's Slope measures
  zipf <- data.frame("Register" = register, 
                     "Tokens" = growth@data$data$Tokens,
                     "Zipf's Slope" = growth@data$data$Zipf)
  
  measures <- list(typeToken = typeToken,
                   growthRate = growthRate,
                   typeTokenRatio = typeTokenRatio,
                   meanLog = meanLog,
                   herdan = herdan,
                   guiraud = guiraud,
                   yule = yule,
                   zipf = zipf)
  
  return(measures)
}


#==============================================================================#
#                           analyzeDiversity                                   #
#==============================================================================#
#'  analyzeDiversity
#' 
#' This function analyzes lexical diversity according to eight measures of 
#' lexical diversity.  It returns a nested list containing eight data frames, 
#' one per  lexical measure, for each of the three registers. Each data 
#' frame contains the data necessary to plot the measure using ggplot2
#' 
#' @param korpus - the meta data for the clean corpus
#' @param analysis - the corpus analysis
#' @param directories - the directory structure
#' @param regex - the regex patterns
#' @param numSamples - the number of samples to be included in the VGCs
#' @return diversityAnalysis - a list with the growth object, measures, and zipf 
#'                            data
#' @author John James
#' @export
analyzeDiversity <- function(korpus, analysis, directories, regex, 
                             numSamples = 100) {
  
  startTime <- Sys.time()
  message(paste('\nConducting Lexical Diversity Analysis at', startTime))
  
  size <- rep(floor(min(analysis$featureMatrix$tokens[1:3]) / numSamples) * numSamples, 3)
  chunkSize <- floor(size / numSamples) 
  
  # Conduct diversity analysis of each register  
  analysis <- lapply(seq_along(korpus$documents), function(d) {
    
    message(paste('\n...loading', korpus$documents[[d]]$fileDesc))
    textData <- unlist(readFile(korpus$documents[[d]]))
    
    message('......tokenizing data')
    korpus$documents[[d]]$data <- unlist(quanteda::tokenize(textData, what = 'word'))[1:size[d]]
    
    message('......creating growth object')
    diversity <- list()
    diversity$growth <- growth.fnc(korpus$documents[[d]]$data, size = chunkSize[d], nchunks = numSamples)
    diversity$measures <- 
      measureDiversity(korpus$documents[[d]]$fileDesc, 
                       diversity$growth)
    message('......creating zipf object')
    diversity$zipf <- 
      createZipfObject(korpus$documents[[d]], diversity$growth, numSamples)
    diversity
  })
  
  # Repack the data by object type (not register)
  diversityAnalysis <- repackData(korpus, analysis)
  
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'diversity-analysis-', 
                             diversityAnalysis$metaData$fileName, 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- paste0('diversityAnalysis', diversityAnalysis$metaData$objName)
  output$data  <- diversityAnalysis
  saveObject(output)
  
  # Log Results
  logR('diversityAnalysis', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Diversity Analysis Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(diversityAnalysis)
}
## ---- end