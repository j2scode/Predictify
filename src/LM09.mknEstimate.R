## ---- mkn_evaluate
#==============================================================================#
#                                 mknEstimate                                  #
#==============================================================================#
#'  mknEstimate
#' 
#' This function takes as it parameter, the language model to evaluate and
#' the meta data for the training and test sets, the number of sentences from
#' the test set to evaluate and the project directory structure and returns 
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
mknEstimate <- function(mkn, training, test, sents = NULL, directories) {
  
  startTime <- Sys.time()
  message(paste("\nEvaluating performance on ", test$fileDesc, 'at', startTime))
  
  
  # Function that performs the quadgram probability estimate 
  score <- function(quintgram) {
      
    # Calculate unigram probability, p1
    s <- quintgram[5]
    n1p <- model[[1]][ nGram == s][,c(cKN, norm)]
    p1 <- as.numeric(n1p[1] / n1p[2]) + 1 / V
    
    # Calculate bigram probability, p2
    sfx <- paste0(quintgram[4:5], collapse = ' ')
    cntx <- quintgram[4]
    alpha <- max(0,model[[2]][ nGram == sfx][, alpha])
    lambda <- model[[1]][ nGram == cntx][, lambda]
    if (length(lambda) == 0) {
      lambda <- 0.8 * as.numeric(discounts[2,4] / model[[1]][, norm])
    }
    p2 <- alpha + lambda * p1
    
    # Calculate trigram probability, p3
    sfx <- paste0(quintgram[3:5], collapse = ' ')
    cntx <- paste0(quintgram[3:4], collapse = ' ')
    alpha <- max(0,model[[3]][ nGram == sfx][, alpha])
    lambda <- model[[2]][ nGram == cntx][, lambda]
    if (length(lambda) == 0) {
      lambda <- 0.8 * as.numeric(discounts[3,4] / model[[2]][, norm])
    }
    p3 <- alpha + lambda * p2
    
    # Calculate quadgram probability, p4
    sfx <- paste0(quintgram[2:5], collapse = ' ')
    cntx <- paste0(quintgram[2:4], collapse = ' ')
    alpha <- max(0,model[[4]][ nGram == sfx][, alpha])
    lambda <- model[[3]][ nGram == cntx][, lambda]
    if (length(lambda) == 0) {
      lambda <- 0.8 * as.numeric(discounts[4,4])
    }
    p4 <- alpha + lambda * p3
    
    # Calculate quintgram probability, p5
    sfx <- paste0(quintgram[1:5], collapse = ' ')
    cntx <- paste0(quintgram[1:4], collapse = ' ')
    alpha <- max(0,model[[5]][ nGram == sfx][, alpha])
    lambda <- model[[4]][ nGram == cntx][, lambda]
    if (length(lambda) == 0) {
      lambda <- 0.8 * as.numeric(discounts[5,4])
    }
    p5 <- alpha + lambda * p4
    return(log(p5))
  }
  
  scoreSentence <- function(sentence) {
    tokens <- unlist(quanteda::tokenize(sentence, what = 'word'))
    rbindlist(lapply(seq_along(tokens[1:(length(tokens)-4)]), function(x) {
      nGram <- list()
      nGram$quintgram <- paste0(tokens[x:(x+4)], collapse = ' ')
      nGram$logProb <- score(tokens[x:(x+4)])
      nGram
    }))
  }
  
  message('...loading training data')
  train <- readFile(training)
  df <- quanteda::dfm(train, tolower = FALSE, remove = 'BOS')
  V <- length(featnames(df))

  message(paste('...loading language model'))
  model <- lapply(seq_along(mkn$counts), function(x) {
    loadObject(mkn$counts[[x]])
  })

  message(paste('...loading test data'))
  document <- readFile(test)
  if (!(is.null(sents))) {
    document <- sampleData(document, numChunks = sents, chunkSize = 1, format = 'v')
  }
  # Compute number of sentences and tokens w/o 'BOS'
  M <- length(document) # M = number of sentences

  message('...loading discounts')
  discounts <- loadObject(mkn$discounts)
  
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
      trainingSet = training$corpusName,
      size = object.size(model),
      duration = duration,
      testSents = M,
      words = N,
      perplexity = pp
    ),
    detail = scores
  )
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('')),
                             paste0(training$fileName,'-estimates'),
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'evaluation'
  output$data  <- evaluation
  saveObject(output)
  
  # Log and Return results
  logR('mknEstimate', startTime, test$directory, test$fileName)

  # Alert User
  message(paste('MKN model evaluated at', endTime))
  message(paste('Elapsed time is', duration))

  return(evaluation)
}
## ---- end
#est <- mknEstimate(lm$mkn, corpora$training$processed$text$quadgram,  corpora$validation$processed$text$quadgram, directories)