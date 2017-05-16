## ---- analyze_nGram_frequencies

#==============================================================================#
#                           analyzeNGramFrequencies                            #
#==============================================================================#
#'  analyzeNGramFrequencies
#' 
#' This function takes as its parameters, the meta data for the nGrams to be 
#' analyzed and the directory structure, then creates a frequency spectrum
#' data frame.
#' 
#' 
#' @param nGrams - meta data for the nGrams being analyzed
#' @param directories - the project directory structure
#' @return freqAnalysis - a list containing data frames of frequency spectrum
#'                         data
#' @author John James
#' @export
analyzeNGramFrequencies <- function(nGrams, directories) {
  
  startTime <- Sys.time()
  message(paste('\nAnalyzing nGram Frequencies at', startTime))  
   
  freqAnalysis <- lapply(seq_along(nGrams), function(n) {
    dfm <- loadObject(nGrams[[n]])
    tf <- topfeatures(dfm, n = dfm@Dim[2])
    spc <- as.data.table(table(Frequency = unname(tf)))
    spc$Frequency <- as.integer(spc$Frequency)
    setnames(spc, c("Frequency", "N"), c("m", "Vm"))
    spc <- spc[,Nm := (m * Vm)][, N := cumsum(Nm)]
    V <- sum(spc$Vm)
    N <- sum(spc$Nm)
    fa <- list()
    fa$spc <- zipfR::spc(spc$Vm, m.max = 0,N = N, V = V)
    fa$spc$m <- as.integer(fa$spc$m)
    fa$tfl <- zipfR::spc2tfl(fa$spc)
    fa
  })
  
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'nGram-Frequencies',
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'nGramFrequencies'
  output$data  <- freqAnalysis
  saveObject(output)
  
  # Log Results
  logR('analyzeNGramFrequencies', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('nGram Frequencies Analyzed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  
  return(freqAnalysis)
}
## ---- end

