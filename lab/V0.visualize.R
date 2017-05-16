#------------------------------------------------------------------------------#
#                              90.visualize                                    #
#------------------------------------------------------------------------------#
#                                                                              #
# This file contains the scripts to present data visualizations                #
#                                                                              #
#------------------------------------------------------------------------------#

## ---- plot_corpus_summary
#------------------------------------------------------------------------------#
#                         plotCorpusSummary                                    #
#------------------------------------------------------------------------------#
#'  plotCorpusSummary
#' 
#' This function takes as a parameter, the data analysis and returns 
#' descriptive statistics for the corpus.
#' 
#' @param analysis - data quality analysis
#' @param summaryAnalysis - data frame with summary features 
#' @author John James
#' @export
plotCorpusSummary <- function(analysis) {
  
  summaryAnalysis <- data.frame(analysis$featureMatrix$category,
                                analysis$featureMatrix$objectSize,
                                analysis$featureMatrix$sentences,
                                analysis$featureMatrix$tokens,
                                analysis$featureMatrix$words,
                                analysis$featureMatrix$wordTypes)
  names(summaryAnalysis) <- c('Genre', 'Size (MB)', 'Sentences', 
                              'Tokens', 'Words', 'Types')
  return(summaryAnalysis)
}
## ---- end

## ---- plot_spc
#------------------------------------------------------------------------------#
#                              plotSPC                                         #
#------------------------------------------------------------------------------#
#'  plotSPC - plot frequency spectrum
#' 
#' This function takes as a parameter, the observed frequency spectrum and 
#' renders the spectrum plot and table side-by-side  
#' 
#' @param spc - the frequency spectrum 
#' @param spcPlot - frequency spectrum plot data including 
#'                  m - the frequency class at 5% OOV
#'                  bar - the bar plot
#'                  tbl - the frequency spectrum table 
#' @author John James
#' @export
plotSPC <- function(spc) {
  
  # Determine frequency class for 5% OOV.
  spcData <- spc
  N <- zipfR::N(spcData)
  Noov <- round(N * .05, 0)
  spcData$V <- cumsum(spcData$Vm)
  spcData$N <- cumsum(spcData$Vm * spcData$m)
  oov <- head(subset(spcData, N > Noov), 1)
  
  # Prepare bar chart
  bar <- ggplot(spc[1:15,], aes(x = m, y = Vm)) + geom_col(fill = 'orangered3') + 
    theme_minimal()
  bar <- ggplotGrob(bar)
    
  # Prepare table
  tt <- ttheme_minimal(
    core = list(fg_params=list(cex=0.75)),
    colhead=list(fg_params = list(parse=TRUE, cex=0.75)))
  tbl <- tableGrob(spcData[c(1:15,oov$m),], rows = NULL, theme = tt)
  
  # Return objects
  spcPlot = list(
    oov = oov,
    bar = bar,
    tbl = tbl
  )

  return(spcPlot)
}
## ---- end

## ---- plot_corpus_audit
#------------------------------------------------------------------------------#
#                                   plotCorpusAudit                            #
#------------------------------------------------------------------------------#
#'  plotCorpusAudit
#' 
#' This function takes as a parameter, the data analysis and returns
#' a data frame containing frequencies of various linguistic features
#' 
#' @param analysis - data quality analysis
#' @param features - data frame containing frequencies of various
#'         linguistic features
#' @author John James
#' @export
plotCorpusAudit <- function(analysis) {
  
  specialChars <- vector('integer')
  for (i in 1:dim(analysis$featureMatrix)[1]) {
    specialChars[i] <- sum(analysis$featureMatrix$control[i],
                           analysis$featureMatrix$nonAscii[i],
                           analysis$featureMatrix$nonPrint[i],
                           analysis$featureMatrix$nonUTF8[i])
  }
  
  
  # Format features
  features <- data.frame(specialChars / analysis$featureMatrix$tokens * 100,
                         analysis$featureMatrix$contractions / 
                           analysis$featureMatrix$tokens * 100,
                         analysis$featureMatrix$abbreviations / 
                           analysis$featureMatrix$tokens * 100,
                         analysis$featureMatrix$badWords / 
                           analysis$featureMatrix$tokens * 100, 
                         stringsAsFactors = FALSE)
  audit <- analysis$featureMatrix[,c(1:6, 11, 10)]
  audit <- cbind(audit, features)
  names(audit) <- c('Register', 'Size (Mb)', 'Sentences', 'Tokens', 'Words',
                    'Types', 'Mean Word Length', 'Max Word Length', 
                    '% Special Chars', '% Contractions', '% Abbreviations', 
                    '% Profanity')
  
  return(audit)
}
## ---- end
#==============================================================================#
#                            Model Corpus Development                          #
#==============================================================================#


## ---- plot_zipf
#------------------------------------------------------------------------------#
#                                plotZipf                                      #
#------------------------------------------------------------------------------#
#'  plotZipf 
#' 
#' This function takes as its parameter, zipf word frequency distribution
#' data and prepares the zipf plots and residuals plots for the register
#' 
#' @param tfl - the term frequency matrix
#' @param register - the register being plotted
#' @param color - the color of the zipf line
#' @return zipfPlots - the zipf and residuals plots for the register
#' @author John James
#' @export
plotZipf <- function(tfl, register, color) {
  
  # Prepare Zipfplot
  zipf <- ggplot(data = tfl, aes(x = log(k), y = log(f))) +
    geom_line(colour=color) + theme_minimal(base_size = 8) + 
    ylab("Log Frequency") + 
    xlab("Log Rank") + 
    ggtitle(paste(register, "Zipf Curve")) +
    stat_smooth(method = 'lm', aes(weight = 1/k), linetype=2)
  
  # Prepare residuals plot
  zipfLm <- lm(log(f) ~ log(k), data = tfl)
  residuals <- ggplot(zipfLm, aes(.fitted, .resid)) +
    geom_point() +
    theme_minimal() +
    labs(title = paste(register,'Residual Plot')) +
    xlab('Fit') + ylab('Residuals') + 
    geom_hline(yintercept = -2) +
    geom_smooth(se = FALSE)
  
  residuals
  zipfPlots <- list(
    zipf = zipf,
    residuals = residuals
  )
  return(zipfPlots)
}
## ---- end 

## ---- plot_zm_parameters
#------------------------------------------------------------------------------#
#                                plotZMParameters                              #
#------------------------------------------------------------------------------#
#'  plotZMParameters 
#' 
#' This function takes as its parameter, zipf word frequency distribution
#' data and renders a data frame with model parameters for each function.
#' 
#' @param zipf - the zipf word frequency distribution object
#' @return zmParams - the ZM model parameters
#' @author John James
#' @export
plotZMParameters <- function(zipf) {
  
  zmParams <- rbindlist(lapply(seq_along(zipf), function(x) {
    params <- list()
    params$Register <- zipf[[x]]$category
    params$Alpha    <- zipf[[x]]$docLnre$param$alpha
    params$Beta     <- zipf[[x]]$docLnre$param$B
    params
  }))
 
  return(zmParams) 
}
## ---- end  


## ---- plot_coverage
#------------------------------------------------------------------------------#
#                                plotCoverage                                  #
#------------------------------------------------------------------------------#
#'  plotCoverage 
#' 
#' This function takes as its parameter, zipf word frequency distribution
#' data and renders a coverage plot that relates coverage to vocabulary size
#' for each register
#' 
#' @param zipf - the zipf word frequency distribution object
#' @param analysis - the raw (reshaped) data analysis
#' @return coverage - coverage plot 
#' @author John James
#' @export
plotCoverage <- function(zipf, analysis) {
  
  # Get Data
  spc <- rbindlist(lapply(seq_along(zipf), function(x) {
    N <- zipfR::N(zipf[[x]]$docLnre)
    V <- zipfR::EV(zipf[[x]]$docLnre, N)
    register <- rep(zipf[[x]]$category, nrow(zipf[[x]]$docLnre$spc))
    m  <- zipf[[x]]$docLnre$spc$m
    Vm <- zipf[[x]]$docLnre$spc$Vm
    Vc <- cumsum(Vm)
    Nc <- cumsum(m * Vm)
    Noov  <- Nc / N * 100
    C <- 100 - Noov
    Vt <- V - Vc
    Nt <- numeric(nrow(zipf[[x]]$docLnre$spc))
    for (i in 1:nrow(zipf[[x]]$docLnre$spc)) {
      Nt[i] <- head(subset(zipf[[x]]$docExtVgc, V > Vt[i], select = N)$N, 1)
    }
    spectra <- data.frame(Register = register, Coverage = C, Tokens = Nt)
    spectra <- subset(spectra, Coverage > 80)
  }))
  
  # Render plot
  coveragePlot <- ggplot(spc, aes(x=Tokens, y=Coverage, group=Register)) +
    scale_fill_discrete(name = "Registers") +
    geom_line(aes(color=Register)) +
    theme_minimal() + 
    labs(title = 'Coverage by Sample Size', x = 'Tokens', y = 'Coverage(%)') +
    scale_color_brewer(palette = 'Dark2') 
  
  # Subset Data by Register
  Register <- as.character(unique(spc$Register))
  blogs <- subset(spc, Register == 'Blogs')
  news <- subset(spc, Register == 'News') 
  twit <- subset(spc, Register == 'Twitter') 
  
  # Create table rows
  N <- sum(unlist(lapply(seq_along(zipf), function(x) {
    zipfR::N(zipf[[x]]$docSpc)
  })))
  coverage <- with(blogs, approx(Coverage, Tokens, xout = c(90, 95, 99), rule = 2))
  blogsCoverage <- data.frame(Register = Register[1], t(coverage$y))
  
  coverage <- with(news, approx(Coverage, Tokens, xout = c(90, 95, 99), rule = 2))
  newsCoverage <- data.frame(Register = Register[2], t(coverage$y))
  
  coverage <- with(twit, approx(Coverage, Tokens, xout = c(90, 95, 99), rule = 2))
  twitCoverage <- data.frame(Register = Register[3], t(coverage$y))
  
  coverageTable <- rbind(blogsCoverage, newsCoverage, twitCoverage)
  
  coverageTotal <-data.frame(Register = 'Corpus', X1 = sum(coverageTable[,2]),
                             X2 = sum(coverageTable[,3]), 
                             X3 = sum(coverageTable[,4]))

  coverageTable <- rbind(coverageTable, coverageTotal)
  
  coveragePct <- data.frame(Register = '% HC Corpora', 
                            X1 = coverageTable[4,2] / N * 100,
                            X2 = coverageTable[4,3] / N * 100,
                            X3 = coverageTable[4,4] / N * 100)
  
  coverageTable <- rbind(coverageTable, coveragePct)
  
  names(coverageTable) <- c('Register', '90%', '95%', '99%')
  
  coverageTable[-1,-1] <- round(coverageTable[-1,-1], 1)

  # Prepare plot grob
  coveragePlot <- ggplotGrob(coveragePlot)
  
  # Prepare table grob
  tt <- ttheme_minimal(
    core = list(fg_params=list(cex=.75)),
    colhead=list(fg_params = list(parse=TRUE, cex=.75)))
  coverageTableGrob <- tableGrob(format(coverageTable, big.mark=","), rows = NULL, theme = tt)
  
  coverage <- list(
    N = N,
    plot = coveragePlot,
    tbl = coverageTable,
    grob = coverageTableGrob
  )
  
  return(coverage)
}
## ---- end


## ---- plot_corpus_design
#------------------------------------------------------------------------------#
#                                plotCorpusDesign                              #
#------------------------------------------------------------------------------#
#'  plotCorpusDesign
#' 
#' This function takes as its parameter, the raw corpus summary and the zipf
#' object and returns a dat frame summarizing training, validation and test
#' set sizes
#' 
#' @param analysis - raw corpus analysis (w/ average sentence length)
#' @param zipf - the zipf word frequency distribution object
#' @param coverage - the coverage table
#' @return design - the corpus design
#' @author John James
#' @export
plotCorpusDesign <- function(analysis, zipf, coverage) {
  
  sentLength <- analysis$featureMatrix$wordsPerSent[1:3]
  design <- coverage$tbl[c(1:3), c(1, 3)]
  design$sentLength <- sentLength
  design$sents <- design$'95%' / design$sentLength 
  design$trainSent <- coverage$tbl[c(1:3), c(3)] * .8 / design$sentLength
  design$valSent <- coverage$tbl[c(1:3), c(3)] * .1 / design$sentLength
  design$testSent <- coverage$tbl[c(1:3), c(3)] * .1 / design$sentLength
  total <- data.frame(Register = 'Corpus',
                      `95%` = sum(design$`95%`),
                      sentLength = mean(design$sentLength),
                      sents = sum(design$sents),
                      trainSent = sum(design$trainSent),
                      valSent = sum(design$valSent),
                      testSent = sum(design$testSent))
  names(total) <- names(design)
  design <- rbind(design, total)
  names(design) <- c('Register', 'Tokens', 'Mean Sentence Length',  'Sentences',
                     'Training Set', 'Validation Set', 'Test Set')
  
  return(design)
}

## ---- end


#==============================================================================#
#                            Model Corpus EDA                                  #
#==============================================================================#

## ---- plot_nGrams
#------------------------------------------------------------------------------#
#                                   plotNgrams                                 #
#------------------------------------------------------------------------------#
#'  plotNgrams
#' 
#' This function plots top features for the document feature matrix provided.  
#' 
#' @param nGram - the document feature matrix meta datea
#' @param type - indicator of type of nGram to plot
#' @param color - the color of the boxplot
#' @param features - the number of features to plot
#' @return nGramPlotData - nGram plot data including top features and boxplot
#' @author John James
#' @export
plotNgrams <- function(nGram, type, color, features = 50) {
  
  # Top Feature Counts
  topFeatures <- topfeatures(nGram, n = features)
  
  # Format data frame for plotting
  dfNGram <- data.frame(Feature = as.factor(names(topFeatures)), 
                        Frequency = topFeatures)
  dfNGram$Feature <- factor(dfNGram$Feature, 
                            levels = unique(as.character(dfNGram$Feature)))
  
  # Render Bar Plots
  boxPlot <- ggplot(dfNGram, aes(x=Feature,y=Frequency)) + 
    geom_bar(stat="Identity", fill=color) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45,hjust=1)) + 
    xlab(type) + ylab("Frequency") + ggtitle(paste("Top", features, type))
  
  # Format output
  nGramPlotData <- list(
    topFeatures = dfNGram,
    boxPlot = boxPlot,
    dfm = nGram
  )
  return(nGramPlotData)
  
}
## ---- end

#==============================================================================#
#                            Lexical Features Section                          #
#==============================================================================#
## ---- plot_lexical_features
#------------------------------------------------------------------------------#
#                             plotLexicalFeatures                              #
#------------------------------------------------------------------------------#
#'  plotLexicalFeatures
#' 
#' This function plots the distribution of the top 10 lexical features of a 
#' corpus analysis.  This function returns the data in a boxplot or table of 
#' expected values, based upon the output format selected
#' 
#' @param densityAnalysis - the density analysis
#' @param posTags - the posTags being evaluated
#' @param output - form of output (bp = boxplut or x = expected values)
#' @return bp - a box plot of features
#' @author John James
#' @export
plotLexicalFeatures <- function(densityAnalysis, posTags, output = 'bp') {
  
  keyTags <- c("NN",	"IN",	"JJ",	"PRP",	"RB",	"NNS",	"VB",	"VBD",	
               "VBP",	"VBZ",	"WRB",	"WP",	"WP$")
  
  if (output == 'bp') {
    # Extract Features
    features <- densityAnalysis$korpusAnalysis$chunkMatrix
    features <- subset(features, tags %in% keyTags)
    names(features) <- c('Chunk', 'Tag', 'Freq')
    
    # Add descriptions
    pos <- subset(posTags, Tag %in% keyTags, select = c('Tag', 'Description'))
    names(pos) <- c('Tag', 'Feature')
    features <- merge(features, pos, by = 'Tag')
    
    # Render plot
    features <- ggplot(features, aes(x=Feature, y = Freq)) + 
      geom_boxplot(aes(fill = Feature)) + labs(x = "Feature", y = "Frequency") + 
      scale_fill_brewer(palette="Paired") + theme_minimal() +
      theme(axis.text.x = element_text(angle=45,hjust=1))
  } else {
    features <- densityAnalysis$korpusAnalysis$featureStats$mean
    features <- subset(features, tags %in% keyTags)
    names(features) <- c('Tag', 'Description', 'Mean')
  }
  
  return(features)
}

## ---- end  