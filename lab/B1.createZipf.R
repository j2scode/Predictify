## ---- create_zipf_objects

#==============================================================================#
#                            createZipfObject                                  #
#==============================================================================#
#'  createZipfObject 
#' 
#' This function creates a zipf object given the growth object created in 
#' createGrowthObjects
#' 
#' @param register - the register for the document
#' @param document - the document for which the growth curves are calculated
#' @param vgc - the vocabulary growth curve data 
#' @param numSamples - the number of samples, default = 1000
#' @param zipfObject - the zipf object for the register
#' @author John James
#' @export
createZipfObject <- function(register, document, vgc, numSamples = 1000) {
  
  message(paste('...preparing zipf object for', register))
  
  # Read document and extract words
  nTokens <- length(document)
  sampleSize <- floor(nTokens / numSamples)
  
  # Create frequency spectrum object and term frequency list
  docSpc    <- text2spc.fnc(document)
  docSpc$m  <- as.integer(docSpc$m)
  docSpc$Vm <- as.integer(docSpc$Vm)
  docTfl    <- spc2tfl(docSpc)
  
  # Prepare Empirical Vocabulary Growth Curve
  docEmpVgc <- vgc
  
  # Prepare an interpolated vocabulary growth curve
  docIntVgc <- zipfR::vgc.interp(docSpc, subset(docEmpVgc,
                                                select = N)$N, m.max=1)

  # Prepare extrapolated vocabulary growth curve
  docLnre <- lnre('zm', spc=docSpc, cost='chisq', method='Custom', exact = FALSE)
  docExtVgc <- lnre.vgc(docLnre, (1:numSamples)*sampleSize)
  
  # Calculate goodness of fit
  g <- lnre.goodness.of.fit(docLnre, docSpc, n.estimated=0, m.max=15)
  goodness <- data.frame(Register = register, X2 = g$X2, P = g$p)

  zipfObject <- list(
    category = register,
    nTokens = nTokens,
    docTfl = docTfl,
    docSpc = docSpc,
    docLnre = docLnre,
    docEmpVgc = docEmpVgc,
    docIntVgc = docIntVgc,
    docExtVgc = docExtVgc,
    goodness = goodness)
  
  return(zipfObject)
}

#==============================================================================#
#                               createZipf                                     #
#==============================================================================#
#'  createZipf
#' 
#' This function takes as its parameters, the meta data for the corpus and the
#' growth objects created in createGrowthObjects, and returns a zipf object
#' that will be used for diversity and sample size analysis
#' 
#' @param korpus - meta data for the corpus being analyzed
#' @param vgc - the vocabulary growth curve data
#' @param directories - the project directory structure
#' @return zipfObjects - the zipf object used for analysis and plotting
#' @author John James
#' @export
createZipf <- function(korpus, vgc, directories) {
  
  startTime <- Sys.time()
  message(paste('\nCreating Zipf objects for ', korpus$corpusName,
                'at', startTime))  
  
  zipfObjects <- lapply(seq_along(korpus$documents), function (x) {
    message(paste('...preparing', korpus$documents[[x]]$fileDesc))
    document <- readFile(korpus$documents[[x]])
    tokens <- unlist(quanteda::tokenize(document, what = "word", 
                                        removeNumbers = TRUE, 
                                        remove_punct = TRUE, 
                                        removeSymbols = TRUE, 
                                        removeTwitter = TRUE, 
                                        removeHyphens = FALSE,
                                        removeURL = TRUE))
    createZipfObject(korpus$documents[[x]]$category, tokens, vgc[[x]])
  })
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'zipf-analysis-', korpus$fileName, 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'zipf'
  output$data  <- zipfObjects
  saveObject(output)
  
  # Log Results
  logR('createZipf', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Zipf Objects Created at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  
  return(zipfObjects)
}
## ---- end

