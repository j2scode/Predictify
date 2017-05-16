## ---- annotate_corpora

#==============================================================================#
#                             annotateFile                                     #
#==============================================================================#
#'  annotateFile
#' 
#' This function takes as its parameter, the document meta data and creates
#' creates multigram versions of document with appropriate sentence boundaries.#' 
#' 
#' @param source - the meta data for the source document to be annotated
#' @param target - the meta data for processed multigram documents
#' @author John James
#' @export
annotateFile <- function(source, target) {
  
  message(paste('...annotating', source$fileDesc))
  document <- readFile(source)
  
  lapply(seq_along(target), function(x) {
    message(paste('......creating', target[[x]]$fileDesc, 'sentence boundaries'))
    annotatedDoc <- unlist(lapply(seq_along(document), function(s) {
      paste(paste0(rep("BOS", times = x-1), collapse = " "), document[s], "EOS", collapse = " ")
    }))
    target[[x]]$data <- annotatedDoc
    saveFile(target[[x]])
  })
}
  

#==============================================================================#
#                           annotateCorpus                                     #
#==============================================================================#
#'  annotateCorpus
#' 
#' This function takes as its parameter, the corpus meta data and creates
#' creates multigram versions of the text and pos tags documents
#' 
#' 
#' @param korpus - the corpus meta data
#' @param what - c('text', 'pos', 'both)
#' @author John James
#' @export
annotateCorpus <- function(korpus, what) {
  
  if (what %in% c('text', 'both')) {
    annotateFile(korpus$preprocessedData$text, korpus$processedData$text)
  }
  if (what %in% c('pos', 'both')) {
    annotateFile(korpus$preprocessedData$pos, korpus$processedData$pos)
  }
}



#==============================================================================#
#                           annotateCorpora                                    #
#==============================================================================#
#'  annotateCorpora
#' 
#' This function takes the corpora meta data and produces multigram versions
#' of the training, validation, and test sets' text and POS tags. 
#' 
#' @param korpora - the meta data for corpora
#' @param what = c('text', 'pos', 'both)
#' @author John James
#' @export
annotateCorpora <- function(korpora, what) {
  
  startTime <- Sys.time()
  
  message(paste("\nAnnotating Corpora at", startTime))
  
  cv <- list(
    trainingSet = korpora$train,
    validationSet = korpora$validation,
    testSet = korpora$test
  )
  
  lapply(seq_along(cv), function(x) {
    annotateCorpus(cv[[x]], what)
  })
  
  # Log Results
  logR('annotateCorpora', startTime, ' ', 'various')

  # Alert User
  endTime <- Sys.time()
  message(paste('Processing of training text completed at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
