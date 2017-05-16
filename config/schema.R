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
                        file.path('final/en_US/en_US.twitter.txt')))
  ), # end of raw
  reshaped = list(
    corpusName = 'Reshaped Data',
    directory  = directories$reshapedCorpus,
    fileName = 'reshaped-data',
    objName = 'reshapedData'
  ), # end of reshaped
  clean = list(
    corpusName = 'Clean Corpus',
    directory  = directories$cleanCorpus,
    fileName = 'clean-corpus',
    objName = 'cleanCorpus'
  ), # end of clean
  pilot = list(
    corpusName = 'Pilot Corpus',
    directory  = directories$pilotCorpus,
    fileName = 'pilot-corpus',
    objName = 'pilotCorpus'
  ),
  training = list(
    corpusName = 'Training Corpora',
    directory  = directories$trainingCorpora,
    fileName = 'training-corpora',
    objName = 'trainingCorpora'
  ),
  validation = list(
    corpusName = 'Validation Corpus',
    directory  = directories$validationCorpus,
    fileName = 'validation-set',
    objName = 'validationSet'
  ),
  test = list(
    corpusName = 'Test Corpus',
    directory  = directories$testCorpus,
    fileName = 'test-set',
    objName = 'testSet'
  )
)# end of corpora

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

docTypes <- list(
  text = 'text',
  pos = list(
    tags = 'tags',
    pairs = 'pairs'
  )
)

textPipeline <- list(
  clean = 'clean',
  preprocessed = 'preprocessed',
  processed = 'processed',
  nGrams = 'nGrams'
)

posPipeline <- list(
  clean = 'clean',
  processed = 'processed',
  nGrams = 'nGrams'
)

lm <- list(
  mkn = list(
    func = 'trainMKN',
    args = list(
      mName = 'quadgramMKN',
      mDesc = 'Quadgram MKN Model',
      directory = file.path(directories$lm, 'mkn'),
      regex = regexPatterns,
      summary = list(
        directory = file.path(directories$lm, 'mkn'),
        fileDesc = 'MKN N-Gram Count Summary',
        fileName = 'mkn-ngram-count-summary.Rdata',
        objName  = 'mknNGramCountSummary'
      ),
      discounts = list(
        directory = file.path(directories$lm, 'mkn'),
        fileDesc = 'MKN Discounts',
        fileName = 'mkn-discounts.Rdata',
        objName  = 'mknDiscounts'
      ),
      counts = list(
        unigrams = list(
          directory = file.path(directories$lm, 'mkn', 'counts'),
          fileDesc = 'MKN Unigram Counts',
          fileName = 'mkn-unigram-counts.Rdata',
          objName  = 'mknUnigramCounts'
        ),
        bigrams = list(
          directory = file.path(directories$lm, 'mkn', 'counts'),
          fileDesc = 'MKN Bigram Counts',
          fileName = 'mkn-bigram-counts.Rdata',
          objName  = 'mknBigramCounts'
        ),
        trigrams = list(
          directory = file.path(directories$lm, 'mkn', 'counts'),
          fileDesc = 'MKN Trigram Counts',
          fileName = 'mkn-trigram-counts.Rdata',
          objName  = 'mknTrigramCounts'
        ),
        quadgrams = list(
          directory = file.path(directories$lm, 'mkn', 'counts'),
          fileDesc = 'MKN Quadgram Counts',
          fileName = 'mkn-quadgram-counts.Rdata',
          objName  = 'mknQuadgramCounts'
        )
      ),
      model = list(
        unigrams = list(
          directory = file.path(directories$lm, 'mkn', 'model'),
          fileDesc = 'MKN Unigram Model',
          fileName = 'mkn-unigram-model.Rdata',
          objName  = 'mknUnigramModel'
        ),
        bigrams = list(
          directory = file.path(directories$lm, 'mkn', 'model'),
          fileDesc = 'MKN Bigram Model',
          fileName = 'mkn-bigram-model.Rdata',
          objName  = 'mknBigramModel'
        ),
        trigrams = list(
          directory = file.path(directories$lm, 'mkn', 'model'),
          fileDesc = 'MKN Trigram Model',
          fileName = 'mkn-trigram-model.Rdata',
          objName  = 'mknTrigramModel'
        ),
        quadgrams = list(
          directory = file.path(directories$lm, 'mkn', 'model'),
          fileDesc = 'MKN Quadgram Model',
          fileName = 'mkn-quadgram-model.Rdata',
          objName  = 'mknQuadgramModel'
        )
      )
    )
  )
)

dependencies <- list(
  rPackages = list('data.table', 'doParallel', 'fastmatch', 'kfigr', 'knitr',
                   'ggplot2', 'ggthemes', 'gridExtra', 'lsa', 'languageR', 'jpeg',
                   'qdapDictionaries', 'qdapRegex', 'qdapTools', 'qdap',
                   'NLP', 'openNLP', 'pbapply', 'plotly', 'quanteda', 'RCurl', 
                   'reshape2', 'stringr', 'tools', 'tm', 'wordcloud', 'zipfR')
                       
)
## ---- end