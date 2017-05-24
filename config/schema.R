## ---- schema
#' Predictify Variable Schema
#' This script contains the constant variables used throughout the project
#' 
#' 

# Regex Patterns
regexPatterns = list(
  words           = "\\b[a-zA-Z]+\\b",
  longWords       = "\\b[a-zA-Z0-9]{41,}\\b",
  alphabetic      = "[[:alpha:]]",
  nonAlphabetic   = "[^[:alpha:]]",
  digits          = '[[:digit:]]',
  emails          = "[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*@[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*\\.[a-zA-Z]{2,}",
  urls            = "(?:(?:https?:\\/\\/)|(?:www\\.))[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b(?:[-a-zA-Z0-9@:%_\\+.~#?&/=]*)",
  twitter         = '\\B#\\w*[a-zA-Z]+\\w*',
  punct           = "[[:punct:]]",
  punctSansMain   = "(?![.?!'])[[:punct:]]",
  punctSansApos   = "(?!['])[[:punct:]]",
  symbols         = "[-^*()_|~={}\\[\\]:;<>\\/]",
  control         = '[[:cntrl:]]',
  nonAscii        = '[^[:ascii:]]',
  nonPrintable    = '[^[:print:]]',
  contractions    = "^([^']*?)(')([^']*?)$",
  hyphens         = '[-]',
  hyphenatedWords = "(?=\\S*[-])(^[a-zA-Z-]+$)",
  repeatedChars   = '(.)\\1{2,}',
  repeatedPattern = '(.+?)\\1+',
  apostrophe      = '[\']',
  strayApostrophe = "\\s*'\\B|\\B'\\s*",
  singles         = '\\b[b-hj-z]{1}\\b',
  whiteSpace      = "\\s+",
  abbreviations   = "(?:[a-zA-Z]\\.){2,}",
  context = list(
    bigrams   = "^((\\S+\\s+){0}\\S+).*$",
    trigrams  = "^((\\S+\\s+){1}\\S+).*$",
    quadgrams = "^((\\S+\\s+){2}\\S+).*$"
  ),
  suffix = list(
    bigrams = "^.*\\s+((?:\\S+\\s+){0}\\S+)$",
    trigrams = "^.*\\s+((?:\\S+\\s+){1}\\S+)$",
    quadgrams = "^.*\\s+((?:\\S+\\s+){2}\\S+)$"
  )
) # end of regexPatterns

pos <- list(
  tags = c("IN",	"JJ",	"JJR",	"JJS",	"NN", "NNS",	"NP",	"NPS",	"RB",	"RBR",	
           "VB",	"VBD",	"VBG",	"VBN",	"VBP",	"VBZ",	"VD",	"VDD",	"VDG",	
           "VDN",	"VDP",	"VDZ",	"VH",	"VHD",	"VHG",	"VHN",	"VHP",	"VHZ",	
           "VV",	"VVD",	"VVG",	"VVN",	"VVP",	"VVZ",	"CC",	"DT",	"MD",	"PRP",	
           "PP$",	"RBS",	"RP",	"SENT",	"WDT",	"WP",	"WP$",	"WRB",	"FW",
           "NNP", "NNPS", "PRP$", "POS"),
  desc = c("Preposition/Subord. Conj.",	"Adjective",	"Adjective, Comparative",	
           "Adjective, Superlative",	"Noun, Singular Or Mass",	"Noun Plural",	
           "Proper Noun, Singular",	"Proper Noun, Plural",	"Adverb",	
           "Adverb, Comparative",	"Verb Be, Base Form",	"Verb Be, Past",	
           "Verb Be, Gerund/Participle",	"Verb Be, Past Participle",	
           "Verb Be, Pres Non-3Rd P.",	"Verb Be, Pres, 3Rd P. Sing",	
           "Verb Do, Base Form",	"Verb Do, Past",	"Verb Do Gerund/Participle",	
           "Verb Do, Past Participle",	"Verb Do, Pres, Non-3Rd Per.",	
           "Verb Do, Pres, 3Rd Per.Sing",	"Verb Have, Base Form",	
           "Verb Have, Past",	"Verb Have, Gerund/Participle",	
           "Verb Have, Past Participle",	"Verb Have, Pres Non-3Rd Per.",	
           "Verb Have, Pres 3Rd Per.Sing",	"Verb, Base Form",	
           "Verb, Past Tense",	"Verb, Gerund/Participle",	
           "Verb, Past Participle",	"Verb, Present, Non-3Rd P.",	
           "Verb, Present 3D P. Sing.",	"Coordinating Conjunction",	
           "Determiner",	"Modal",	"Personal Pronoun",	"Possessive Pronoun",	
           "Adverb, Superlative",	"Particle",	"End Punctuation",	"Wh-Determiner",	
           "Wh-Pronoun",	"Possessive Wh-Pronoun",	"Wh-Abverb",	"Foreign Word",
           "Proper Noun Singular", "Proper Noun Plural", "Possessive Pronoun",
           "Possessive Ending"),
  category = c("Function",	"Content",	"Content",	"Content",	"Content",	"Content",
               "Content",	"Content",	"Content",	"Content",	"Function",	"Function",	
               "Function",	"Function",	"Function",	"Function",	"Function",	"Function",	
               "Function",	"Function",	"Function",	"Function",	"Function",	"Function",
               "Function",	"Function",	"Function",	"Function",	"Content",	"Content",	
               "Content",	"Content",	"Content",	"Content",	"Function",	"Function",	
               "Function",	"Function",	"Function",	"Function",	"Function",	"Function",	
               "Function",	"Function",	"Function",	"Function",	"X", "Content", "Content",
               "Content", "Content"),
  study = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
            TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
            FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE)
)

posTags <- data.frame(Category = pos$category, Tag = pos$tags, 
                      Description = pos$desc, Study = pos$study)

selectPOSTAgs <- c("IN",	"NN",	"NNS",	"NNP",	"NNPS", "POS",	"PRP",	"PRP$", 
                   "VB",	"VBD",	"VBG",	"VBN",	"VBZ",	"WP",	"WP$",	"WRB")

#' ## Project Directory Structure
directories <- list(
  analysisDir        = './analysis',
  libDir             =	'./lib',
  sourceDir          =	'./src',
  configDir          = './config',
  logsDir            = './logs',
  labsDir            = './lab',
  referenceDataDir   = './data/referenceData',
  rawCorpus          = './data/raw',
  devData            = './data/development',
  reshapedCorpus     = './data/reshaped',
  cleanCorpus        = './data/clean',
  pilotCorpus        = './data/pilot',
  trainingCorpora    = './data/training',
  validationCorpus   = './data/validation',
  testCorpus         = './data/test',
  lm                 = './lm',
  testingDir         = './test'
)
# End of directories

referenceFiles  = list(
  abbreviations  = list(
    url          = "https://raw.githubusercontent.com/j2scode/PredictifyR/master/data/referenceData/abbreviations.csv",
    directory    = directories$referenceDataDir,
    fileName     = 'abbreviations.csv', 
    objName      = 'abbreviations',
    fileType     = 'csv',
    columns      = c("key", "value")
  ),
  emoticons  = list(
    url          = "https://raw.githubusercontent.com/j2scode/PredictifyR/master/data/referenceData/emoticons.csv",
    directory    = directories$referenceDataDir,
    fileName     = 'emoticons.csv',
    objName      = 'emoticons',
    fileType     = 'csv',
    columns      = c("key")
  ),
  badWordsFile   = list(
    url          = "https://raw.githubusercontent.com/j2scode/PredictifyR/master/data/referenceData/bad-words.csv",
    directory    = directories$referenceDataDir,
    fileName     = 'bad-words.csv', 
    objName      = 'badWordsFile',
    fileType     = 'csv',
    columns      = c("key")
  ),
  contractions   = list(
    url          = "https://raw.githubusercontent.com/j2scode/PredictifyR/master/data/referenceData/contractions.csv",
    directory    = directories$referenceDataDir,
    fileName     = 'contractions.csv',
    objName      = 'contractions',
    fileType     = 'csv',
    columns      = c("key", "value")
  ),
  corrections   = list(
    url          = "https://raw.githubusercontent.com/j2scode/PredictifyR/master/data/referenceData/corrections.csv",
    directory    = directories$referenceDataDir,
    fileName     = 'corrections.csv',
    objName      = 'corrections',
    fileType     = 'csv',
    columns      = c("key", "value")
  )
) # end of reference files

registers <- list(
  blogs = list(
    fileDesc = 'Blogs Register',
    fileName  = 'en_US.blogs.txt',
    objName = 'blogsRegister'
  ),
  news = list(
    fileDesc = 'News Register',
    fileName  = 'en_US.news.txt',
    objName = 'newsRegister'
  ),
  twitter = list(
    fileDesc = 'Twitter Register',
    fileName  = 'en_US.twitter.txt',
    objName = 'twitterRegister'
  )
)

triGrams <- list(
  unigrams = list(
    fileDesc = 'Unigrams',
    fileName = 'unigrams.Rdata',
    objName = 'unigrams'
  ),
  bigrams = list(
    fileDesc = 'Bigrams',
    fileName = 'bigrams.Rdata',
    objName = 'bigrams'
  ),
  trigrams = list(
    fileDesc = 'Trigrams',
    fileName = 'trigrams.Rdata',
    objName = 'trigrams'
  )
)

triGramText <- list(
  unigrams = list(
    fileDesc = 'Unigrams',
    fileName = 'unigrams.txt',
    objName = 'unigrams'
  ),
  bigrams = list(
    fileDesc = 'Bigrams',
    fileName = 'bigrams.txt',
    objName = 'bigrams'
  ),
  trigrams = list(
    fileDesc = 'Trigrams',
    fileName = 'trigrams.txt',
    objName = 'trigrams'
  )
)

quadGrams <- list(
  unigrams = list(
    fileDesc = 'Unigrams',
    fileName = 'unigrams.Rdata',
    objName = 'unigrams'
  ),
  bigrams = list(
    fileDesc = 'Bigrams',
    fileName = 'bigrams.Rdata',
    objName = 'bigrams'
  ),
  trigrams = list(
    fileDesc = 'Trigrams',
    fileName = 'trigrams.Rdata',
    objName = 'trigrams'
  ),
  quadgrams = list(
    fileDesc = 'Quadgrams',
    fileName = 'quadgrams.Rdata',
    objName = 'quadgrams'
  )
)

quadGramText <- list(
  unigrams = list(
    fileDesc = 'Unigrams',
    fileName = 'unigrams.txt',
    objName = 'unigrams'
  ),
  bigrams = list(
    fileDesc = 'Bigrams',
    fileName = 'bigrams.txt',
    objName = 'bigrams'
  ),
  trigrams = list(
    fileDesc = 'Trigrams',
    fileName = 'trigrams.txt',
    objName = 'trigrams'
  ),
  quadgrams = list(
    fileDesc = 'Quadgrams',
    fileName = 'quadgrams.txt',
    objName = 'quadgrams'
  )
)

# Corpora meta data 
corpora = list(
  raw = list(
    corpusName  = 'Raw Corpus',
    directory   = directories$rawCorpus,
    source      = list(
      url           = 'http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',
      downloadPath  = 'data/download/HC-Corpus.zip',
      zipPath       = "C:\\Users\\John\\Documents\\Data Science\\Data Science Projects\\PredictifyR\\data\\download\\HC-Corpus.zip",
      unZipDir      = directories$rawCorpus,
      unZipfiles    = c(file.path('final/en_US/en_US.blogs.txt'),
                        file.path('final/en_US/en_US.news.txt'),
                        file.path('final/en_US/en_US.twitter.txt'))),
    documents = lapply(seq_along(registers), function(r) {
      d <- list()
      d$directory <- directories$rawCorpus
      d$fileName <- registers[[r]]$fileName
      d$fileDesc <- registers[[r]]$fileDesc
      d$objName  <- registers[[r]]$objName
      d
    })
  ), # end of raw
  reshaped = list(
    corpusName = 'Reshaped Data',
    directory  = directories$reshapedCorpus,
    fileName = 'reshaped-data',
    objName = 'reshapedData',
    documents = lapply(seq_along(registers), function(r) {
      d <- list()
      d$directory <- directories$reshapedCorpus
      d$fileName <- registers[[r]]$fileName
      d$fileDesc <- registers[[r]]$fileDesc
      d$objName  <- registers[[r]]$objName
      d
    })
  ), # end of reshaped
  clean = list(
    corpusName = 'Clean Corpus',
    directory  = directories$cleanCorpus,
    fileName = 'clean-corpus',
    objName = 'cleanCorpus',
    documents = lapply(seq_along(registers), function(r) {
      d <- list()
      d$directory <- directories$cleanCorpus
      d$fileName <- registers[[r]]$fileName
      d$fileDesc <- registers[[r]]$fileDesc
      d$objName  <- registers[[r]]$objName
      d
    })
  ), # end of clean
  pilot = list(
    corpusName = 'Pilot Corpus',
    documents = lapply(seq_along(registers), function(r) {
      d <- list()
      d$directory <- file.path(directories$pilotCorpus, 'documents')
      d$fileName <- registers[[r]]$fileName
      d$fileDesc <- registers[[r]]$fileDesc
      d$objName  <- registers[[r]]$objName
      d
    }),
    pos = lapply(seq_along(registers), function(r) {
      p <- list()
      p$directory <- file.path(directories$pilotCorpus, 'pos')
      p$fileName <- registers[[r]]$fileName
      p$fileDesc <- registers[[r]]$fileDesc
      p$objName  <- registers[[r]]$objName
      p
    }),
    nGrams = lapply(seq_along(quadGrams), function(n) {
      nGram <- list()
      nGram$directory <- file.path(directories$pilotCorpus, 'nGrams')
      nGram$objName <- quadGrams[[n]]$objName
      nGram$fileName <- quadGrams[[n]]$fileName
      nGram$fileDesc <- quadGrams[[n]]$fileDesc
      nGram
    })
  ),
  training = list(
    alpha = list(
      corpusName = 'Training Set Alpha',
      directory = file.path(directories$trainingCorpora, 'alpha'),
      fileName = 'training-set-alpha',
      objName = 'trainingSetAlpha',
      pct = 10,
      documents = lapply(seq_along(registers), function(r) {
        d <- list()
        d$directory <- file.path(directories$trainingCorpora, 'alpha', 'documents')
        d$fileName <- registers[[r]]$fileName
        d$fileDesc <- registers[[r]]$fileDesc
        d$objName  <- registers[[r]]$objName
        d
      }),
      pos  = list(
        tags = lapply(seq_along(registers), function(r) {
          t <- list()
          t$directory <- file.path(directories$trainingCorpora, 'alpha', 'posTags')
          t$fileName <- registers[[r]]$fileName
          t$fileDesc <- registers[[r]]$fileDesc
          t$objName  <- registers[[r]]$objName
          t
        }),
        pairs = lapply(seq_along(registers), function(r) {
          p <- list()
          p$directory <- file.path(directories$trainingCorpora, 'alpha', 'posPairs')
          p$fileName <- registers[[r]]$fileName
          p$fileDesc <- registers[[r]]$fileDesc
          p$objName  <- registers[[r]]$objName
          p
        })
      ),
      processed = list(
        words = lapply(seq_along(quadGramText), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'alpha', 'processed', 'words')
          nGram$objName <- quadGramText[[n]]$objName
          nGram$fileName <- quadGramText[[n]]$fileName
          nGram$fileDesc <- quadGramText[[n]]$fileDesc
          nGram
        }),
        pos = lapply(seq_along(triGramText), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'alpha', 'processed', 'pos')
          nGram$objName <- triGramText[[n]]$objName
          nGram$fileName <- triGramText[[n]]$fileName
          nGram$fileDesc <- triGramText[[n]]$fileDesc
          nGram
        }),
        pairs = list(
          directory = file.path(directories$trainingCorpora, 'alpha', 'processed', 'pairs'),
          fileDesc = 'POS Word Pairs',
          fileName = 'pos-word-pairs.txt',
          objName = 'posWordPairs'
        )
      ),
      nGrams = list(
        words = lapply(seq_along(quadGrams), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'alpha', 'nGrams', 'word')
          nGram$objName <- quadGrams[[n]]$objName
          nGram$fileName <- quadGrams[[n]]$fileName
          nGram$fileDesc <- quadGrams[[n]]$fileDesc
          nGram
        }),
        pos = lapply(seq_along(triGrams), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'alpha', 'nGrams', 'pos')
          nGram$objName <- triGrams[[n]]$objName
          nGram$fileName <- triGrams[[n]]$fileName
          nGram$fileDesc <- triGrams[[n]]$fileDesc
          nGram
        })
      )
    ),
    beta = list(
      corpusName = 'Training Set Beta',
      directory = file.path(directories$trainingCorpora, 'beta'),
      fileName = 'training-set-beta',
      objName = 'trainingSetBeta',
      pct = 20,
      documents = lapply(seq_along(registers), function(r) {
        d <- list()
        d$directory <- file.path(directories$trainingCorpora, 'beta', 'documents')
        d$fileName <- registers[[r]]$fileName
        d$fileDesc <- registers[[r]]$fileDesc
        d$objName  <- registers[[r]]$objName
        d
      }),
      pos  = list(
        tags = lapply(seq_along(registers), function(r) {
          t <- list()
          t$directory <- file.path(directories$trainingCorpora, 'beta', 'posTags')
          t$fileName <- registers[[r]]$fileName
          t$fileDesc <- registers[[r]]$fileDesc
          t$objName  <- registers[[r]]$objName
          t
        }),
        pairs = lapply(seq_along(registers), function(r) {
          p <- list()
          p$directory <- file.path(directories$trainingCorpora, 'beta', 'posPairs')
          p$fileName <- registers[[r]]$fileName
          p$fileDesc <- registers[[r]]$fileDesc
          p$objName  <- registers[[r]]$objName
          p
        })
      ),
      processed = list(
        words = lapply(seq_along(quadGramText), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'beta', 'processed', 'words')
          nGram$objName <- quadGramText[[n]]$objName
          nGram$fileName <- quadGramText[[n]]$fileName
          nGram$fileDesc <- quadGramText[[n]]$fileDesc
          nGram
        }),
        pos = lapply(seq_along(triGramText), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'beta', 'processed', 'pos')
          nGram$objName <- triGramText[[n]]$objName
          nGram$fileName <- triGramText[[n]]$fileName
          nGram$fileDesc <- triGramText[[n]]$fileDesc
          nGram
        }),
        pairs = list(
          directory = file.path(directories$trainingCorpora, 'beta', 'processed', 'pairs'),
          fileDesc = 'POS Word Pairs',
          fileName = 'pos-word-pairs.txt',
          objName = 'posWordPairs'
        )
      ),
      nGrams = list(
        words = lapply(seq_along(quadGrams), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'beta', 'nGrams', 'word')
          nGram$objName <- quadGrams[[n]]$objName
          nGram$fileName <- quadGrams[[n]]$fileName
          nGram$fileDesc <- quadGrams[[n]]$fileDesc
          nGram
        }),
        pos = lapply(seq_along(triGrams), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'beta', 'nGrams', 'pos')
          nGram$objName <- triGrams[[n]]$objName
          nGram$fileName <- triGrams[[n]]$fileName
          nGram$fileDesc <- triGrams[[n]]$fileDesc
          nGram
        })
      )
    ),
    gamma = list(
      corpusName = 'Training Set Gamma',
      directory = file.path(directories$trainingCorpora, 'gamma'),
      fileName = 'training-set-gamma',
      objName = 'trainingSetGamma',
      pct = 35,
      documents = lapply(seq_along(registers), function(r) {
        d <- list()
        d$directory <- file.path(directories$trainingCorpora, 'gamma', 'documents')
        d$fileName <- registers[[r]]$fileName
        d$fileDesc <- registers[[r]]$fileDesc
        d$objName  <- registers[[r]]$objName
        d
      }),
      pos  = list(
        tags = lapply(seq_along(registers), function(r) {
          t <- list()
          t$directory <- file.path(directories$trainingCorpora, 'gamma', 'posTags')
          t$fileName <- registers[[r]]$fileName
          t$fileDesc <- registers[[r]]$fileDesc
          t$objName  <- registers[[r]]$objName
          t
        }),
        pairs = lapply(seq_along(registers), function(r) {
          p <- list()
          p$directory <- file.path(directories$trainingCorpora, 'gamma', 'posPairs')
          p$fileName <- registers[[r]]$fileName
          p$fileDesc <- registers[[r]]$fileDesc
          p$objName  <- registers[[r]]$objName
          p
        })
      ),
      processed = list(
        words = lapply(seq_along(quadGramText), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'gamma', 'processed', 'words')
          nGram$objName <- quadGramText[[n]]$objName
          nGram$fileName <- quadGramText[[n]]$fileName
          nGram$fileDesc <- quadGramText[[n]]$fileDesc
          nGram
        }),
        pos = lapply(seq_along(triGramText), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'gamma', 'processed', 'pos')
          nGram$objName <- triGramText[[n]]$objName
          nGram$fileName <- triGramText[[n]]$fileName
          nGram$fileDesc <- triGramText[[n]]$fileDesc
          nGram
        }),
        pairs = list(
          directory = file.path(directories$trainingCorpora, 'gamma', 'processed', 'pairs'),
          fileDesc = 'POS Word Pairs',
          fileName = 'pos-word-pairs.txt',
          objName = 'posWordPairs'
        )
      ),
      nGrams = list(
        words = lapply(seq_along(quadGrams), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'gamma', 'nGrams', 'word')
          nGram$objName <- quadGrams[[n]]$objName
          nGram$fileName <- quadGrams[[n]]$fileName
          nGram$fileDesc <- quadGrams[[n]]$fileDesc
          nGram
        }),
        pos = lapply(seq_along(triGrams), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'gamma', 'nGrams', 'pos')
          nGram$objName <- triGrams[[n]]$objName
          nGram$fileName <- triGrams[[n]]$fileName
          nGram$fileDesc <- triGrams[[n]]$fileDesc
          nGram
        })
      )
    ),
    delta = list(
      corpusName = 'Training Set Delta',
      directory = file.path(directories$trainingCorpora, 'delta'),
      fileName = 'training-set-delta',
      objName = 'trainingSetDelta',
      pct = 50,
      documents = lapply(seq_along(registers), function(r) {
        d <- list()
        d$directory <- file.path(directories$trainingCorpora, 'delta', 'documents')
        d$fileName <- registers[[r]]$fileName
        d$fileDesc <- registers[[r]]$fileDesc
        d$objName  <- registers[[r]]$objName
        d
      }),
      pos  = list(
        tags = lapply(seq_along(registers), function(r) {
          t <- list()
          t$directory <- file.path(directories$trainingCorpora, 'delta', 'posTags')
          t$fileName <- registers[[r]]$fileName
          t$fileDesc <- registers[[r]]$fileDesc
          t$objName  <- registers[[r]]$objName
          t
        }),
        pairs = lapply(seq_along(registers), function(r) {
          p <- list()
          p$directory <- file.path(directories$trainingCorpora, 'delta', 'posPairs')
          p$fileName <- registers[[r]]$fileName
          p$fileDesc <- registers[[r]]$fileDesc
          p$objName  <- registers[[r]]$objName
          p
        })
      ),
      processed = list(
        words = lapply(seq_along(quadGramText), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'delta', 'processed', 'words')
          nGram$objName <- quadGramText[[n]]$objName
          nGram$fileName <- quadGramText[[n]]$fileName
          nGram$fileDesc <- quadGramText[[n]]$fileDesc
          nGram
        }),
        pos = lapply(seq_along(triGramText), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'delta', 'processed', 'pos')
          nGram$objName <- triGramText[[n]]$objName
          nGram$fileName <- triGramText[[n]]$fileName
          nGram$fileDesc <- triGramText[[n]]$fileDesc
          nGram
        }),
        pairs = list(
          directory = file.path(directories$trainingCorpora, 'delta', 'processed', 'pairs'),
          fileDesc = 'POS Word Pairs',
          fileName = 'pos-word-pairs.txt',
          objName = 'posWordPairs'
        )
      ),
      nGrams = list(
        words = lapply(seq_along(quadGrams), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'delta', 'nGrams', 'word')
          nGram$objName <- quadGrams[[n]]$objName
          nGram$fileName <- quadGrams[[n]]$fileName
          nGram$fileDesc <- quadGrams[[n]]$fileDesc
          nGram
        }),
        pos = lapply(seq_along(triGrams), function(n) {
          nGram <- list()
          nGram$directory <- file.path(directories$trainingCorpora, 'delta', 'nGrams', 'pos')
          nGram$objName <- triGrams[[n]]$objName
          nGram$fileName <- triGrams[[n]]$fileName
          nGram$fileDesc <- triGrams[[n]]$fileDesc
          nGram
        })
      )
    )
  ), # end of training
  validation = list(
    corpusName = 'Validation Corpus',
    directory  = directories$validationCorpus,
    fileName = 'validation-set',
    objName = 'validationSet',
    documents = lapply(seq_along(registers), function(r) {
      d <- list()
      d$directory <- file.path(directories$validationCorpus, 'documents')
      d$fileName <- registers[[r]]$fileName
      d$fileDesc <- registers[[r]]$fileDesc
      d$objName  <- registers[[r]]$objName
      d
    }),
    processed = list(
      words = lapply(seq_along(quadGramText), function(n) {
        nGram <- list()
        nGram$directory <- file.path(directories$validationCorpus, 'processed', 'words')
        nGram$objName <- quadGramText[[n]]$objName
        nGram$fileName <- quadGramText[[n]]$fileName
        nGram$fileDesc <- quadGramText[[n]]$fileDesc
        nGram
      })
    )
  ),
  test = list(
    corpusName = 'Test Corpus',
    directory  = directories$testCorpus,
    fileName = 'test-set',
    objName = 'testSet',
    documents = lapply(seq_along(registers), function(r) {
      d <- list()
      d$directory <- directories$testCorpus
      d$fileName <- registers[[r]]$fileName
      d$fileDesc <- registers[[r]]$fileDesc
      d$objName  <- registers[[r]]$objName
      d
    }),
    processed = list(
      words = lapply(seq_along(quadGramText), function(n) {
        nGram <- list()
        nGram$directory <- file.path(directories$validationCorpus, 'processed', 'words')
        nGram$objName <- quadGramText[[n]]$objName
        nGram$fileName <- quadGramText[[n]]$fileName
        nGram$fileDesc <- quadGramText[[n]]$fileDesc
        nGram
      })
    )
  )
)# end of corpora


lm <- list(
  mkn4 = list(
    func = 'trainMKN4',
    args = list(
      mName = 'quadgramMKN4',
      mDesc = 'Quadgram MKN4 Model',
      mOrder = 4,
      directory = file.path(directories$lm, 'mkn4'),
      regex = regexPatterns,
      summary = list(
        directory = file.path(directories$lm, 'mkn4'),
        fileDesc = 'MKN4 N-Gram Count Summary',
        fileName = 'mkn4-ngram-count-summary.Rdata',
        objName  = 'mkn4NGramCountSummary'
      ),
      discounts = list(
        directory = file.path(directories$lm, 'mkn4'),
        fileDesc = 'MKN4 Discounts',
        fileName = 'mkn4-discounts.Rdata',
        objName  = 'mkn4Discounts'
      ),
      counts = lapply(seq_along(quadGrams), function(n) {
        nGram = list()
        nGram$directory <- file.path(directories$lm, 'mkn4', 'counts')
        nGram$objName  <- paste0('mkn4', quadGrams[[n]]$fileDesc)
        nGram$fileName <- paste0('mkn4-', quadGrams[[n]]$fileName)
        nGram$fileDesc <- paste0('MKN4 ', quadGrams[[n]]$fileDesc)
        nGram
      }),
      model = lapply(seq_along(quadGrams), function(n) {
        nGram = list()
        nGram$directory <- file.path(directories$lm, 'mkn4', 'model')
        nGram$objName  <- paste0('mkn4', quadGrams[[n]]$fileDesc)
        nGram$fileName <- paste0('mkn4-', quadGrams[[n]]$fileName)
        nGram$fileDesc <- paste0('MKN4 ', quadGrams[[n]]$fileDesc)
        nGram
      })
    )# end of args
  )# end of mkn
)

dependencies <- list(
  rPackages = list('data.table', 'doParallel', 'fastmatch', 'kfigr', 'knitr',
                   'ggplot2', 'ggthemes', 'gridExtra', 'lsa', 'languageR', 'jpeg',
                   'qdapDictionaries', 'qdapRegex', 'qdapTools', 'qdap',
                   'NLP', 'openNLP', 'pbapply', 'plotly', 'quanteda', 'RCurl', 
                   'reshape2', 'stringr', 'tools', 'tm', 'wordcloud', 'zipfR')
                       
)
## ---- end