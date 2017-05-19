## ---- create_zipf_objects

#==============================================================================#
#                            createZipfObject                                  #
#==============================================================================#
#'  createZipfObject 
#' 
#' This function creates a zipf object given the growth object created in 
#' createGrowthObjects
#' 
#' @param document - the document and meta data to be analyzed
#' @param vgc - the vocabulary growth curve data 
#' @param numSamples - the number of samples, default = 1000
#' @param zipfObject - the zipf object for the register
#' @author John James
#' @export
createZipfObject <- function(document, vgc, numSamples = 1000) {
  
  message(paste('...preparing zipf object for', document$fileDesc))
  
  # Read document and extract words
  nTokens <- length(document$data)
  sampleSize <- floor(nTokens / numSamples)
  
  # Create frequency spectrum object and term frequency list
  docSpc    <- text2spc.fnc(document$data)
  docSpc$m  <- as.integer(docSpc$m)
  docSpc$Vm <- as.integer(docSpc$Vm)
  docTfl    <- spc2tfl(docSpc)
  
  # Prepare Empirical Vocabulary Growth Curve
  docEmpVgc <- vgc
  
  # Calculate N
  if ('data.frame' %in% class(vgc)) {
    N = subset(docEmpVgc, select = N)$N
  } else {
    N = tail(docEmpVgc@data$data$Tokens, 1)
  }
  
  # Prepare an interpolated vocabulary growth curve
  docIntVgc <- zipfR::vgc.interp(docSpc, N, m.max=1, allow.extrapolation=TRUE)

  # Prepare extrapolated vocabulary growth curve
  docLnre <- lnre('zm', spc=docSpc, cost='chisq', method='Custom', exact = FALSE)
  docExtVgc <- lnre.vgc(docLnre, (1:numSamples)*sampleSize)
  
  # Calculate goodness of fit
  g <- lnre.goodness.of.fit(docLnre, docSpc, n.estimated=0, m.max=15)
  goodness <- data.frame(Register = document$fileDesc, X2 = g$X2, P = g$p)

  zipfObject <- list(
    category = document$fileDesc,
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
#' This function returns a zipf object that will be used for diversity 
#' and sample size analysis
#' 
#' @param korpus - meta data for the corpus being analyzed
#' @param vgc - the vocabulary growth curve data
#' @param directories - the project directory structure
#' @return zipfObjects - the zipf object used for analysis and plotting
#' @author John James
#' @export
createZipf <- function(korpus, vgc, directories) {
  
  startTime <- Sys.time()
  message(paste('\nCreating Zipf objects for', korpus$corpusName,
                'at', startTime))  
  
  zipfObjects <- lapply(seq_along(korpus$documents), function (x) {
    message(paste('...preparing', korpus$documents[[x]]$fileDesc))
    document <- readFile(korpus$documents[[x]])
    korpus$documents[[x]]$data <- unlist(quanteda::tokenize(document, what = "word"))
    createZipfObject(korpus$documents[[x]], vgc[[x]])
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

