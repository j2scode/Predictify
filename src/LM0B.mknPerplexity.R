## ---- mkn_perplexity
#==============================================================================#
#                                 mknPerplexity                                #
#==============================================================================#
#'  mknPerplexity
#' 
#' This function returns 
#' a data frame containing model size data, processing time, and perplexity
#' scores. 
#' 
#' @param mkn - the meta data for the language model
#' @param training the meta data for the training data 
#' @param test - the meta data for the validation or test corpus
#' @param sents - the number of sentences from the test data to evaluate, 
#'                NULL = all sentences
#' @param directories - the project directory structure
#' @return estimates - the probability estimates for each word in the test data
#' @author John James
#' @export
mknPerplexity <- function(mkn, training, test, sents = NULL, directories) {
  
  memory.limit(20000)
  startTime <- Sys.time()
  message(paste("\nEvaluating performance on ", mkn$mDesc, 'at', startTime))
  
  
  # Function that performs the quadgram probability estimate 
  score <- function(ngram, tokens, n) {
    
    Pmkn <- model[[n]][nGram == ngram][, Pmkn]
    
    if (length(Pmkn) == 0) {
      tokens <- tokens[2:length(tokens)]
      ngram <- paste0(tokens, collapse = ' ')
      return(score(ngram, tokens, n-1))
    } else {
      return(log2(Pmkn))
    }
  }
  
  scoreSentence <- function(sentence) {
    tokens <- unlist(quanteda::tokenize(sentence, what = 'word'))
    rbindlist(lapply(seq_along(tokens[1:(length(tokens)-3)]), function(x) {
      ngram <- paste0(tokens[x:(x+3)], collapse = ' ')
      sentScore <- list()
      sentScore$quadgram <- paste0(tokens[x:(x+3)], collapse = ' ')
      sentScore$logProb <- score(ngram, tokens[x:(x+3)], mkn$mOrder)
      sentScore
    }))
  }
  
  message('...loading training data')
  train <- readFile(training$processed[[mkn$mOrder]])
  df <- quanteda::dfm(train, tolower = FALSE, remove = 'BOS')
  V <- length(featnames(df))
  trainTokens <- sum(ntoken(df))

  message(paste('...loading language model'))
  model <- lapply(seq_along(mkn$model), function(x) {
    loadObject(mkn$model[[x]])
  })

  message(paste('...loading test data'))
  document <- readFile(test$processed[[mkn$mOrder]])
  if (!(is.null(sents))) {
    document <- sampleData(document, numChunks = sents, chunkSize = 1, format = 'v')
  }
  # Compute number of sentences and tokens w/o 'BOS'
  M <- length(document) # M = number of sentences

  message('...evaluating sentence probabilities')
  scores <- rbindlist(lapply(seq_along(document), function(x) {
    s <- scoreSentence(document[x])
    if (x %in% c(100, 200, 500, 1000, 5000, 10000, 20000)) { 
      elapsed <- round(difftime(Sys.time(), startTime,  units = 'mins'))
      elapsed <- as.numeric(elapsed) + 1
      rate <- x / elapsed 
      remaining <- length(document) - x
      timeMin <- round(remaining / rate, digits = 1)
      timeHrs <- round(timeMin / 60, digits = 1)
      message(paste('......',x,'out of',length(document), 'sentences processed in', 
                    elapsed, 'minutes.', timeMin,'minutes remaining (', timeHrs, 'hours)'))
    }
    s
  }))
  

  # Compute perplexity
  N <- length(scores$logProb)
  pp <- 2^-(sum(scores$logProb) / N)
  
  # Note the time
  endTime <- Sys.time()
  duration <- round(difftime(endTime, startTime,  units = 'auto'), 2)
  
  # Summarize results
  evaluation <- list(
    summary = list(
      date = startTime,
      end = endTime,
      duration = duration,
      model = mkn$mDesc,
      trainingSet = training$corpusName,
      trainingSents = length(train),
      trainingTokens = trainTokens,
      trainingSize = object.size(train),
      modelSize = object.size(model),
      testSents = M,
      testWords = N,
      perplexity = pp
    ),
    detail = scores
  )
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('MKN-perplexity-')),
                             mkn$mName,
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'evaluation'
  output$data  <- evaluation
  saveObject(output)
  
  # Log and Return results
  logR('mknPerplexity', startTime, '', ' ')

  # Alert User
  message(paste('MKN model evaluated at', endTime))
  message(paste('Elapsed time is', duration))

  return(evaluation)
}
## ---- end
#ppd <- mknPerplexity(lm$mkn$delta, corpora$training$delta,  corpora$validation$delta, sents = NULL, directories)