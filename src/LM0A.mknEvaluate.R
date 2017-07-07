## ---- mkn_evaluate
#==============================================================================#
#                                 mknEvaluate                                  #
#==============================================================================#
#'  mknEvaluate
#' 
#' This function evaluates a test set on the modified kneser-ney model, 
#' returning a data frame containing model size data, processing time, and 
#' perplexity scores. 
#' 
#' @param mkn - the meta data for the language model
#' @param test - the meta data for the validation or test corpus
#' @param sents - the number of sentences from the test data to evaluate, 
#'                NULL = all sentences
#' @param bos <- start of sentence symbol, defaults = <s>
#' @param eos <- end of sentence symbol, default = </s>
#' @param unk <- unknown word symbol, default 'UNK'
#' @param directories - the project directory structure
#' @return estimates - the probability estimates for each word in the test data
#' @author John James
#' @export
mknEvaluate <- function(mkn, test, sents = NULL, bos = '<s>',
                        eos = '</s>', unk = 'UNK', directories) {
  
  memory.limit(20000)
 
  startTime <- Sys.time()
  message(paste("\nEvaluating performance on ", mkn$mDesc, 'at', startTime))

  # Reports progress 
  rptProgress <- function(document, startTime, x) {
    elapsed <- round(difftime(Sys.time(), startTime,  units = 'mins'))
    elapsed <- as.numeric(elapsed) + 1
    rate <- x / elapsed 
    remaining <- length(document) - x
    timeMin <- round(remaining / rate, digits = 1)
    timeHrs <- round(timeMin / 60, digits = 1)
    message(paste('......',x,'out of',length(document), 'sentences processed in', 
                  elapsed, 'minutes.', timeMin,'minutes remaining (', timeHrs, 'hours)'))
    
  }
  
  # Loads language model 
  loadModel <- function(mkn) {  
    message(paste('...loading language model'))
    model <- lapply(seq_along(mkn$model), function(x) {
      loadObject(mkn$model[[x]])
    })
    return(model)
  }
  
  # Load summary counts
  getSummaryCounts <- function(mkn) {
    return(loadObject(mkn$summary))
  }
  
  # Loads test data
  loadTestData <- function(test, sents) {
    message(paste('...loading test data'))
    filePath <- test$processed[[4]]
    document <- readFile(filePath)
    if (!(is.null(sents))) {
      document <- sampleData(document, numChunks = sents, chunkSize = 1, format = 'v')
    }
    dfm <- quanteda::dfm(document, what = 'fasterword', remove = '<s>', ngrams = 1)
    
    testData <- list(
      document = document,
      sents = length(document),
      tokens = sum(ntoken(dfm)),
      types = length(featnames(dfm)),
      size = object.size(document)
    )
    return(testData)
  }
  
  # Computes normalizing factor for alpha and lambda calculations
  getNormalizer <- function(summary, ngram, order, n, model) {
    if (n == order) {
      normalizer <- model[[n]][ nGram == ngram][, contextCount]
    } else {
      normalizer <- summary[n+1,2]
    }
    return(normalizer)
  }
  
  # Recursively computes probability of an ngram
  getProb <- function(summary, ngram, order, n, model) {
    
    if (n == 1) {
      return(model[[n]][ nGram == ngram][, Pmkn])
    } else {
      norm <- getNormalizer(summary, ngram, order, n, model)
      alpha <- model[[n]][ nGram == ngram][, alphaCount] / norm
      lambda <- model[[n]][ nGram == ngram][, DnNn] / norm
      suffix <- paste0(unlist(strsplit(ngram, ' '))[-1], collapse = ' ')
      return(alpha + lambda * getProb(summary, suffix, order, n-1, model))
    }
  }
  
  # Estimates ngram probability
  scoreNgram <- function(summary, ngram, lmOrder, order, model, nGramTypes) {

    score <- list()
    score$ngram <- ngram
    score$type <- nGramTypes[[order]]
    
    # If highest order equals model order, return exact match probability
    if (order == lmOrder) {
      prob <- model[[order]][ nGram == ngram][, Pmkn]
      score$prob <- prob
      score$logProb <- log2(prob)

    # Else compute probabilities recursively using getProb
    } else {
      suffix <- paste0(unlist(tail(strsplit(ngram, ' ')[[1]], order)), collapse = ' ')
      prob <- as.numeric(getProb(summary, suffix, order, order, model))
      score$prob <- prob
      score$type <- nGramTypes[[order]]
      score$logProb <- log2(prob)
    }
    return(score)
  }
  
  # Determines ngram order based upon presence of ngram in training data
  getNGramOrder <- function(ngram, mkn, n, model) {
    prob <- model[[n]][ nGram == ngram][, Pmkn]
    if (length(prob) != 0) {
      return(n)
    } else {
      ngram <- paste0(unlist(strsplit(ngram, ' '))[-1], collapse = ' ')
      return(getNGramOrder(ngram, mkn, n-1, model))
    }
  }
  
  # Estimates sentence probability
  scoreSentence <- function(summary, sentence, mkn, model, nGramTypes) {
    tokens <- unlist(quanteda::tokenize(sentence, what = 'fasterword'))
    sentScore <- rbindlist(lapply(seq_along(tokens[1:(length(tokens)-3)]), function(x) {
      ngram <- paste0(tokens[x:(x+3)], collapse = ' ')
      order <- getNGramOrder(ngram, mkn, mkn$mOrder, model)
      nGramScore <- scoreNgram(summary, ngram, mkn$mOrder, order, model, nGramTypes)
      score <- list()
      score$quadgram <- ngram
      score$ngramType <- nGramScore$type
      score$prob <- nGramScore$prob
      score$logProb <- nGramScore$logProb
      score
    }))
    return(sentScore)
  }

  # Estimates probabilities for corpus
  scoreCorpus <- function(summary, document, mkn, model, nGramTypes) {
    message('...evaluating sentence probabilities')
    startTime <- Sys.time()
    scores <- rbindlist(lapply(seq_along(document), function(x) {
      s <- scoreSentence(summary, document[x], mkn, model, nGramTypes)
      if (x %in% c(100, 200, 500, 1000, 5000, 10000, 20000)) { 
        rptProgress(document, startTime, x)
      }
      s
    }))
    return(scores)
  }
  
  # Summarize scoring statistics
  getStats <- function(mkn, scores) {
    counts <- loadObject(mkn$summary)
    counts$Exact[1] <- sum(scores[,ngramType == 'Unigram'])
    counts$Exact[2] <- sum(scores[,ngramType == 'Bigram'])
    counts$Exact[3] <- sum(scores[,ngramType == 'Trigram'])
    counts$Exact[4] <- sum(scores[,ngramType == 'Quadgram'])
    counts <- counts[, Pct := Exact / sum(Exact) * 100]
    return(counts)
  }
  
  # Compute perplexity
  calcPerplexity <- function(scores) {
    N <- length(scores$logProb)
    H <- -1/N * sum(scores$logProb)
    pp <- 2^H
    return(pp)
  }
  
  # Main processing
  nGramTypes <- list('Unigram', 'Bigram', 'Trigram', 'Quadgram')
  model <- loadModel(mkn)
  testData <- loadTestData(test, sents)
  summary <- getSummaryCounts(mkn)
  scores <- scoreCorpus(summary, testData$document, mkn, model, nGramTypes)
  stats <- getStats(mkn, scores)
  pp <- calcPerplexity(scores)
  
  # Note the time
  endTime <- Sys.time()
  duration <- round(difftime(endTime, startTime,  units = 'auto'), 2)
  
  # Summarize results
  evaluation <- list(
    summary = list(
      date = startTime,
      end = endTime,
      duration = duration,
      perplexity = pp
    ),
    model = list(
      name = mkn$mName,
      desc = mkn$mDesc,
      stats = loadObject(mkn$summary),
      size = object.size(model)
    ),
    training = list(
      corpusName = mkn$training$meta$corpusName,
      sents = mkn$training$sents,
      tokens = mkn$training$tokens,
      types = mkn$training$types,
      size = mkn$training$size
    ),
    test = list(
      corpusName = test$corpusName,
      sents = testData$sents,
      tokens = testData$tokens,
      types = testData$types,
      size = testData$size
    ),
    stats = stats,
    detail = scores
  )
  
  # Save Analysis
  output <- list()
  output$directory <- directories$analysisDir
  output$fileName  <- paste0(sub('\\..*', '', paste0('MKN-evaluation-')),
                             mkn$mName,
                             format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
  output$objName   <- 'evaluation'
  output$data  <- evaluation
  saveObject(output)
  
  # Log and Return results
  logR('mknEvaluate', startTime, '', ' ')

  # Alert User
  message(paste('MKN model evaluated at', endTime))
  message(paste('Elapsed time is', duration))

  return(evaluation)
}
## ---- end
#ppd <- mknEvaluate(lm$mkn$epsilon, corpora$training$epsilon,  corpora$validation$epsilon, sents = NULL, directories)