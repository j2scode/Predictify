## ---- build_corpus
#==============================================================================#
#                              buildCorpora                                     #
#==============================================================================#
#'  buildCorpora
#' 
#' This function builds the training, validation, and test sets based upon the
#' lexical diversity and feature analysis.
#' 
#' @param korpus - the meta data for the corpus to split 
#' @param design - the corpus design
#' @param training - meta data for the training set
#' @param validation - the meta data for the validation set
#' @param test - the meta data for the test set
#' @return cv - the cross validation set summary
#' @author John James
#' @export
buildCorpora <- function(korpus, design, training, validation, test) {
  
  startTime <- Sys.time()
  
  message("\nBuilding language model training, validation and test sets")

  # Unpack corpus design splits
  splits <- list(
    blogs = design[1,c(8,10,12)],
    news = design[2,c(8,10,12)],
    twitter = design[3,c(8,10,12)]
  )
  
  crossValidation <- list(
    training = training,
    validation = validation,
    test = test
  )
  set.seed(1212)
  lapply(seq_along(splits), function(r) {
    message(paste('...splitting', korpus$documents[[r]]$fileDesc))
    keys <- list()
    sents <- readFile(korpus$documents[[r]])
    pool <- seq(1:length(sents))
    keys$train <- sample(pool, splits[[r]][1,1])
    pool <- pool[-keys$train]
    keys$val   <- sample(pool, splits[[r]][1,2])
    pool <- pool[-keys$val]
    keys$test <- sample(pool, splits[[r]][1,3])
    lapply(seq_along(crossValidation), function(c) {
      samples <- sents[keys[[c]]]
      crossValidation[[c]]$documents[[r]]$data <- samples
      saveFile(crossValidation[[c]]$documents[[r]])
    })
  })

  endTime <- Sys.time()
  message(paste('Corpus Build Complete', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  # Log and Return results
  logR('buildCorpora', startTime, ' ', ' ')
  
}
## ---- end
#cv <- buildCorpora(corpora$clean, corpusDesign, corpora$train, corpora$validation, corpora$test)