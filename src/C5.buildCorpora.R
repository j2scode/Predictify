## ---- build_corpora
#------------------------------------------------------------------------------#
#                              buildCorpora                                    #
#------------------------------------------------------------------------------#
#'  buildCorpora
#' 
#' This function creates the training, validation, and test sets. Four training
#' sets would be comprised of two, four, seven,and ten pilot corpora, 
#' containing approximately 10%, 20$, 35%, and 50% of the HC Corpus.  The
#' validation and test sets will be comprised of a single pilot corpus each,
#' 
#' 
#' @param design - the pilot and corpora design
#' @param hcCorpus - the meta data for the HC Corpus
#' @param training - the meta data for the training corpus
#' @param validation - the meta data for the validation corpus
#' @param test - the meta data for the test corpus
#' @author John James
#' @export
buildCorpora <- function(design, hcCorpus, training, 
                         validation, test) {
  
  startTime <- Sys.time()
  message(paste('\nBuilding Model Corpora at', startTime))
  
  #---------------------------------------------------------------------------#
  #                         Reading HC Corpus                                 #
  #---------------------------------------------------------------------------#
  
  message('...reading HC Corpus and break into chunks')
  korpus <- lapply(seq_along(hcCorpus$documents), function(d) {
    document <- readFile(hcCorpus$documents[[d]])
    chunkDocument(document, design$pilot$`Sentences per Chunk`[d])
  })
  
  #---------------------------------------------------------------------------#
  #                 Build Training, Validation and Test Sets                  #
  #---------------------------------------------------------------------------#
  
  # Iterate through corpus registers
  lapply(seq_along(korpus), function(r) {
    message(paste('...processing', hcCorpus$documents[[r]]$fileDesc))
    
    message('...creating validation set')
    pool <- 1:length(korpus[[r]])
    chunks <- design$corpusDesign$Validation[r] / 
      design$pilot$`Sentences per Chunk`[r]
    set.seed(0505)
    valIndices <- sample(pool, chunks)
    validation$documents[[r]]$data <- unlist(korpus[[r]][valIndices])
    saveFile(validation$documents[[r]])
    
    message('...creating test set')
    pool <- pool[!pool %in% valIndices]
    chunks <- design$corpusDesign$Test[r] / 
              design$pilot$`Sentences per Chunk`[r]
    set.seed(0505)
    testIndices <- sample(pool, chunks)
    test$documents[[r]]$data <- unlist(korpus[[r]][testIndices])
    saveFile(test$documents[[r]])
    
    message('...creating training sets')
    pool <- pool[!pool %in% testIndices]
    lapply(seq_along(training), function(t) {
      set.seed(0505)
      chunks <- design$corpusDesign[r, t+1] / 
                design$pilot$`Sentences per Chunk`[r]
      trainingIndices <- sample(pool, chunks)
      training[[t]]$documents[[r]]$data <- unlist(korpus[[r]][trainingIndices])
      saveFile(training[[t]]$documents[[r]])
    })
  }) 

  # Log Results
  logR('buildCorpora', startTime, '', '')
  
  # Alert User
  endTime <- Sys.time()
  message(paste('Model corpora build complete at', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
}
## ---- end
#splitData(analysis, featureDistributionAnalysis[[length(featureDistributionAnalysis)]]$size, corpora)