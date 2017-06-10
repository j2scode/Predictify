## ---- katz_evaluate
#==============================================================================#
#                                 katzEvaluate                                 #
#==============================================================================#
#'  katzEvaluate
#' 
#' This function evaluates a test set on the katz-backoff model, 
#' returning a data frame containing model size data, processing time, and 
#' perplexity scores. 
#' 
#' @param katz - the meta data for the language model
#' @param training - the meta data for the training corpus
#' @param test - the meta data for the validation or test corpus
#' @param sents - the number of sentences from the test data to evaluate, 
#'                NULL = all sentences
#' @param directories - the project directory structure
#' @return estimates - the probability estimates for each word in the test data
#' @author John James
#' @export
katzEvaluate <- function(katz, training, test, sents = NULL, directories) {
  
  memory.limit(20000)
  startTime <- Sys.time()
  message(paste("\nEvaluating performance on ", katz$mDesc, 'at', startTime))
  
  
  # Function that performs the quadgram probability estimate 
  backoff <- function(ngram, n) {
    
    # Format context (prefix) and suffix (tail) for scoring
    prefix <- gsub(regex$context[[n]], "\\1", ngram, perl = TRUE)
    tail <- gsub(regex$suffix[[n]], "\\1", ngram, perl = TRUE)
    
    # Get katz probabiity for tail of n+1 gram, if it exists
    pKatz <- model[[n+1]][suffix == tail, pKatzSuffix, by = suffix]$pKatzSuffix[1]
    
    # Get Alpha based upon context of n+1 gram
    alpha <- model[[n+1]][context == prefix, alpha, by = context]$alpha[1]
    
    # If data found, return alpha * pkatz
    if (!is.na(alpha) & !is.na(pKatz)) {
      return(alpha * pKatz)
    } else {
      
      # If data doesn't exist and ngram order > 1, backoff
      if (n > 1 ) {
        return(backoff(tail, n-1))
      } else {
        
        # If the context isn'f found, get alpha for the unknown word 
        if (is.na(alpha)) {
          alpha <- model[[n+1]][context == 'UNK', alpha, by = context]$alpha[1]
        }
        
        # If the suffix isn'f found, get probability of the unknown word
        if (is.na(pKatz)) {
          pKatz <- model[[n+1]][suffix == 'UNK', pKatzSuffix, by = suffix]$pKatzSuffix[1]
        }
        return(alpha * pKatz)
      }
    }
  }
    
  # Score nGram
  score <- function(ngram, n) {
    
    # Check if nGram exists and return probability
    s <- model[[n]][ nGram == ngram, pKatzNGram]
    
    if (length(s) > 0 ) {
      return(s)
    } else {
      return(backoff(ngram, n-1))
    }
  }

  
  scoreSentence <- function(sentence) {
    tokens <- unlist(quanteda::tokenize(sentence, what = 'word'))
    rbindlist(lapply(seq_along(tokens[1:(length(tokens)-3)]), function(x) {
      ngram <- paste0(tokens[x:(x+3)], collapse = ' ')
      sentScore <- list()
      sentScore$quadgram <- ngram
      sentScore$prob <- score(ngram, katz$mOrder)
      sentScore$logProb <- log2(sentScore$prob)
      sentScore
    }))
  }
  
  message(paste('...loading language model'))
  model <- lapply(seq_along(katz$model), function(x) {
    loadObject(katz$model[[x]])
  })

  message(paste('...loading test data'))
  document <- readFile(test)
  if (!(is.null(sents))) {
    document <- sampleData(document, numChunks = sents, chunkSize = 1, format = 'v')
  }
  # Compute number of sentences and tokens w/o 'BOS'
  M <- length(document) # M = number of sentences

  message('...evaluating sentence probabilities')
  scores <- rbindlist(lapply(seq_along(document), function(x) {
    s <- scoreSentence(document[x])
    message(paste('\n...',s$quadgram, 'probability is', s$prob))
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
      model = katz$mDesc,
      #trainingSet = training$corpusName,
      #trainingSents = length(train),
      #trainingTokens = trainTokens,
      #trainingSize = object.size(train),
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
  output$fileName  <- paste0(sub('\\..*', '', paste0('Katz-evaluation-')),
                             katz$mName,
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'evaluation'
  output$data  <- evaluation
  saveObject(output)
  
  # Log and Return results
  logR('katzEvaluate', startTime, '', ' ')

  # Alert User
  message(paste('Katz model evaluated at', endTime))
  message(paste('Elapsed time is', duration))

  return(evaluation)
}
## ---- end
#ppd <- katzEvaluate(lm$katz$delta, corpora$training$delta,  corpora$validation$delta, sents = NULL, directories)