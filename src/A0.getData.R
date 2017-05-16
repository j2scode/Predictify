## ---- get_data
#==============================================================================#
#                                  getData                                     #
#==============================================================================#
#'  getData
#' 
#' This function obtains the raw corpus data from the HC Corpora site and the
#' reference data from the project github site.
#' 
#' @param rawCorpus - the meta data for the raw corpus
#' @param reference - the meta data for the reference files which contain
#'                    lists of profane words, abbreviations, emoticons
#'                    and contractions.
#' @param directories - the directory structure
#' @param local - logical indicating whether to obtain the data locally
#' @author John James
#' @export
getData <- function(rawCorpus, reference, directories, local=TRUE) {
  
  message('\nObtaining Corpus Data')
  
  startTime <- Sys.time()
  
  # If local is FALSE, download the data from the remote source and unzip it
  # into the raw data directory.  If local is TRUE, check local directory,
  # and alert user if already exists.  If not in the local directory, check
  # for the zipped file.  If that exists, unzip it.  All else false
  # go remote.
  
  # Format key variables
  zipFile <- list()
  zipFile$filePath  <- rawCorpus$source$zipPath
  zipFile$files     <- rawCorpus$source$unZipfiles
  rawDataDir        <- rawCorpus$source$unZipDir
  
  if (local == FALSE) {
    message('...downloading corpus files from remote site, stand by this will take a while')
    downloadData(rawCorpus$source$url, 
                 rawCorpus$source$downloadPath)
    message('...unzipping corpus files, almost there!')
    unZipFile(zipFile, rawDataDir)
    message("Corpus files obtained, you're welcome!")
  } else {
   if (file.exists(zipFile$filePath)) {
     message("...unzipping corpus files, this should be quick")
     unZipFile(zipFile, rawDataDir)
     message("Corpus files obtained, you're welcome!") 
   } else {
     message('...downloading corpus files from remote site, stand by this will take a while')
     downloadData(rawCorpus$source$url, 
                  rawCorpus$source$downloadPath)
     message('...unzipping corpus files, almost there!')
     unZipFile(zipFile, rawDataDir)
     message("Corpus files obtained, you're welcome!")
   }
  }
  

  message('\nObtaining Reference Data')
  if (checkDir(directories$referenceDataDir) == 0 |
      local == FALSE) {
    message('...obtaining the reference data from remote source')
    refData <- list()
    invisible(lapply(seq_along(reference), function(x) {
      refData$directory <- reference[[x]]$directory
      refData$fileName  <- reference[[x]]$fileName
      refData$data      <- read.csv(text = getURL(reference[[x]]$url),
                                    header = TRUE)
      saveFile(refData)
    }))
    message("Reference data obtained")
  } else {
    message('Reference data already exists locally, delete for fresh copy')
  }
  
  # Update log
  logR('getData', startTime, rawDataDir, '*.txt')
  message("\nData acquisition complete...awesome!")
  
}
## ---- end
#getData(schema$corpora$raw, schema$referenceFiles, schema$directories, FALSE)