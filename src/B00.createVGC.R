## ---- create_growth_objects


#==============================================================================#
#                           createGrowthObject                                 #
#==============================================================================#
#'  createGrowthObject
#' 
#' This function takes as its parameter, the meta data for the document to be 
#' analyzed and returns vocabulary growth curve data for the document
#' 
#' @param document - the meta data for the document being analyzed
#' @param type - c('fast', 'full')
#' @param samples - the number of rows in the growth document
#' @return vgc - vocabulary growth curve data 
#' @author John James
#' @export
createGrowthObject <- function(document, type = 'fast', samples = 1000) {
  
  message(paste('...preparing growth object for', document$fileDesc))
  
  startTime <- Sys.time()
  
  # Read data and create document frequency matrix
  textData <- unlist(readFile(document))
  tokens <- unlist(quanteda::tokenize(textData, what = "word"))
  sampleSize <- floor(length(tokens) / samples)
  
  if (type == 'fast') {

    # Initialize VGC vectors an data
    N <- numeric(length = samples)
    V <- numeric(length = samples)
    vocabulary <- ''

    # Build vocabulary growth curve data
    for (i in 1:samples) {
      from <- 1 + ((i-1)*sampleSize)
      to <- i * sampleSize
      vocabulary <- unique(c(vocabulary, unique(tokens[from:to])))
      N[i] <- i * sampleSize
      V[i] <- length(vocabulary)
    }
  
    vgc <- data.frame(N = N, V = V)
  } else {
    vgc <- growth.fnc(text = textData, size = sampleSize, nchunks = samples)
  }  
  return(vgc)
}

#==============================================================================#
#                                  createVGC                                   #
#==============================================================================#
#'  createVGC
#' 
#' This function takes as its parameter, the meta data for the corpus to be 
#' analyzed and returns a list of vocabulary growth curve data, one for each 
#' register
#' 
#' @param korpus - the meta data for the korpus being analyzed
#' @param registers - the meta data for the registers
#' @param directories - the project directory structure
#' @param type = c('fast', 'full)
#' @return growth - list of growth objects
#' @author John James
#' @export
createVGC <- function(korpus, registers, directories, type = 'fast') {
  
  startTime <- Sys.time()
  message(paste('\nPreparing vocabulary growth curve data for', korpus$corpusName,
                'at', startTime))  

  document <- list()
  document$directory <- korpus$directory
  vgc <- lapply(seq_along(registers), function(x) {
    document$fileName <- registers[[x]]$fileName
    document$fileDesc <- registers[[x]]$fileDesc
    createGrowthObject(document, type = type)
  })
  
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'vocabulary-growth-data-', korpus$fileName, 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- paste0('growthAnalysis', korpus$objName)
  output$data  <- vgc
  saveObject(output)
  
  # Log Results
  logR('createVGC', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Vocabulary Growth Data Complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(vgc)
}
## ---- end
