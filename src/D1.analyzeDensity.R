## ---- analyze_density


#==============================================================================#
#                             analyzeDensity                                   #
#==============================================================================#
#'  analyzeDensity
#' 
#' This function takes as its parameters, the meta data for the POS data,
#' the POS tags and the directory structure and returns a data frame with 
#' lexical density information for each register.
#' 
#' @param posData - the meta data for the clean POS tags for each register
#' @param posTags - the posTags 
#' @param directories - the directory structure
#' @return densityAnalysis - a data frame with lexical density calculations
#'          for each register
#' @author John James
#' @export
analyzeDensity <- function(posData, posTags, directories) {
  
  startTime <- Sys.time()
  message(paste('\nConducting Lexical Density Analysis at', startTime))
  
  contentTags <- as.character(subset(posTags, Category == 'Content', select = Tag)$Tag)
  
  densityAnalysis <- rbindlist(lapply(seq_along(posData), function(p) {
    message(paste('...loading and tokenizing', posData[[p]]$fileDesc))
    tags <- unlist(readFile(posData[[p]]))
    tokens <- quanteda::tokenize(tags, what = 'word')
    
    message('...extracting content words')
    contentWords <- tokens_select(tokens, contentTags, selection = 'keep', valuetype = 'fixed')
    
    message('...calculating density')
    d <- list()
    d$register <- posData[[p]]$fileDesc
    d$tokens <- sum(ntoken(tokens))
    d$content <- sum(ntoken(contentWords))
    d$density <- d$content / d$tokens * 100
    d
  }))

  register <- 'Corpus'
  tokens <- sum(densityAnalysis$tokens)
  content <- sum(densityAnalysis$content)
  density <- content / tokens * 100
  summary <- data.frame(register = register, tokens = tokens, content = content, density = density)
  densityAnalysis <- rbind(densityAnalysis, summary)
  names(densityAnalysis) <- c('Register', 'Tokens', 'Content Words', 'Density')
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'density-analysis-', 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'densityAnalysis'
  output$data  <- densityAnalysis
  saveObject(output)
  
  # Log Results
  logR('densityAnalysis', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Density Analysis Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(densityAnalysis)
}
## ---- end