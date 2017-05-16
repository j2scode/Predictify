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
repackData <- function(korpora, analysis) {
  
  metaData <- list()
  metaData$corpusName <- korpora$corpusName
  metaData$objName     <- korpora$objName
  metaData$fileName    <- korpora$fileName
  
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
#' This function takes as its parameters, the meta data for the corpus to
#' be analyzied and returns a nested list containing eight data frames, one per 
#' lexical measure, for each of the three registers. Each data frame contains 
#' the data necessary to plot the measure using ggplot2
#' 
#' @param korpora - the meta data for the clean corpus
#' @param growth - the growth object
#' @param size - a vector of sizes for each register
#' @param directories - the directory structure
#' @param environ - indicator of environment 1=development; 2=production
#' @param regex - the regex patterns
#' @param numSamples - the number of samples to be included in the VGCs
#' @param extrapolate - logical indicating whether the growth curve is
#'                      to be extrapolated to size.
#' @return diversityAnalysis - If running the full version, return a list
#'          with the growth object, measures, and zipf data.  Otherwise,
#'          return zipf data only.
#' @author John James
#' @export
analyzeDiversity <- function(korpora, growth, size, directories, environ, regex,
                             numSamples = 100, extrapolate = FALSE) {
  
  startTime <- Sys.time()
  message(paste('\nConducting Lexical Diversity Analysis at', startTime))
  
  # Conduct diversity analysis of each register  
  analysis <- lapply(seq_along(korpora$words$documents), function(x) {
    message(paste('...analyzing', korpora$words$documents[[x]]$fileDesc))
    tokens <- readFile(korpora$words$documents[[x]])[1:size[x]]
    words  <- grep(regex$words, tokens, perl = TRUE, value = TRUE)
    if (extrapolate == TRUE) {
      sampleSize <- floor(size[x] / numSamples)
    } else {
      sampleSize <- floor(length(words) / numSamples)
    }
    diversity <- list()
    diversity$growth <- growth[[x]]$growth
    diversity$measures <- 
      measureDiversity(korpora$words$documents[[x]]$category, 
                       growth[[x]]$growth)
    diversity$zipf <- 
      createZipfObject(korpora$words$documents[[x]]$category, words, 
                       growth[[x]]$growth, numSamples, 
                                       sampleSize)
    diversity
  })
  
  # Repack the data by object type (not register)
  diversityAnalysis <- repackData(korpora, analysis)
  
  
  # Save Analysis
  env <- 'development'
  if (environ == 2) { env <- 'production' }
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0(env,'-environment-')),
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