## ---- create_spc
#------------------------------------------------------------------------------#
#                                 createSPC                                    #
#------------------------------------------------------------------------------#
#'  createSPC 
#' 
#' This function takes as its parameter, zipf word frequency distribution
#' data and renders an extended SPC including cumulative vocabulary and token
#' counts, the corresponding OOV rates and associated vocabulary and token
#' counts at various levels of coverage.
#' 
#' @param zipf - the zipf word frequency distribution object
#' @param directories - the project directory structure
#' @return spc - extended SPC object
#' @author John James
#' @export
createSPC <- function(zipf, directories) {
  
  startTime <- Sys.time()
  
  message(paste('\nCreating SPC Table at', startTime))
  
  # Get Data
  spc <- rbindlist(lapply(seq_along(zipf), function(x) {
    N <- zipfR::N(zipf[[x]]$docSpc)
    V <- zipfR::EV(zipf[[x]]$docLnre, N)
    register <- rep(zipf[[x]]$category, nrow(zipf[[x]]$docLnre$spc))
    m  <- zipf[[x]]$docLnre$spc$m
    Vm <- zipf[[x]]$docLnre$spc$Vm
    Voov <- cumsum(Vm)
    Noov <- cumsum(m * Vm)
    Vseen <- V - Voov
    Nseen <- N - Noov
    VoovPct <- Voov / V * 100
    NoovPct <- Noov / N * 100
    Vc <- 100 - VoovPct
    Nc <- 100 - NoovPct
    spectra <- data.frame(Register = register, m = m, Vm = Vm, Voov = Voov,
                          Noov = Noov, Vseen = Vseen, Nseen = Nseen, 
                          VoovPct = VoovPct, NoovPct = NoovPct,
                          Vc = Vc, Nc = Nc)
    spectra <- subset(spectra, Coverage > 80)
    spectra
  }))
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             'spc-analysis', 
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'spc'
  output$data  <- spc
  saveObject(output)
  
  # Log Results
  logR('createSPC', startTime, output$directory, output$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Extended SPC Object Created at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(spc)
  
}
## ---- end