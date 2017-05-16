#------------------------------------------------------------------------------#
#                              90.visualize                                    #
#------------------------------------------------------------------------------#
#                                                                              #
# This file contains the scripts to present data visualizations                #
#                                                                              #
#------------------------------------------------------------------------------#

## ---- plot_gantt
#------------------------------------------------------------------------------#
#                                plotGantt                                     #
#------------------------------------------------------------------------------#
#'  plotGantt 
#' 
#' This renders a gantt chart for the project.
#' 
#' @return gantt - the gantt chart
#' @author John James
#' @export
plotGantt <- function() {
  
  gantt <- "C:\\Users\\John\\Documents\\Data Science\\Data Science Projects\\PredictifyR\\documents\\gantt.csv"

  # Read in data
  df <- read.csv(gantt, stringsAsFactors = F, header = T)
  
  # Convert to dates
  df$Start <- as.Date(df$Start, format = "%m/%d/%Y")
  
  # Sample client name
  client = "PredictifyR"
  
  # Choose colors based on number of resources
  cols <- RColorBrewer::brewer.pal(length(unique(df$Resource)), name = "Set3")
  df$color <- factor(df$Resource, labels = cols)
  
  # Initialize empty plot
  p <- plot_ly()
  
  # Each task is a separate trace
  # Each trace is essentially a thick line plot
  # x-axis ticks are dates and handled automatically
  
  for(i in 1:(nrow(df) - 1)){
    p <- add_trace(p,
                   x = c(df$Start[i], df$Start[i] + df$Duration[i]),  # x0, x1
                   y = c(i, i),  # y0, y1
                   mode = "lines",
                   line = list(color = df$color[i], width = 20),
                   showlegend = F,
                   hoverinfo = "text",
                   
                   # Create custom hover text
                   
                   text = paste("Task: ", df$Task[i], "<br>",
                                "Duration: ", df$Duration[i], "days<br>",
                                "Resource: ", df$Resource[i]),
                   
                   evaluate = T  # needed to avoid lazy loading
    )
  }
  
  
  # Add information to plot and make the chart more presentable
  m <- list(l = 150, r = 50, pad = 4)
  p <- layout(p,
              
              # Axis options:
              # 1. Remove gridlines
              # 2. Customize y-axis tick labels and show task names instead of numbers
              
              xaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6")),
              
              yaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6"),
                           tickmode = "array", tickvals = 1:nrow(df), ticktext = unique(df$Task),
                           domain = c(0, 0.9)),
              margin = m,
              # Annotations
              
              annotations = list(
                # Add total duration and total resources used
                # x and y coordinates are based on a domain of [0,1] and not
                # actual x-axis and y-axis values
                
                list(xref = "paper", yref = "paper",
                     x = 0.80, y = 0.1,
                     text = paste0("Total Duration: ", sum(df$Duration), " days<br>"),
                     font = list(color = "#ffff66", size = 12),
                     ax = 0, ay = 0,
                     align = "left"),
                
                # Add client name and title on top
                
                list(xref = "paper", yref = "paper",
                     x = 0.1, y = 1, xanchor = "left",
                     text = paste0(client, " Project Schedule"),
                     font = list(color = "#f2f2f2", size = 20, family = "Times New Roman"),
                     ax = 0, ay = 0,
                     align = "left")
              ),
              
              plot_bgcolor = "#333333",  # Chart area color
              paper_bgcolor = "#333333")  # Axis area color

  return(p)
}
## ---- end


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
                                analysis$featureMatrix$wordTypes,
                                analysis$featureMatrix$wordsPerSent,
                                analysis$featureMatrix$meanWordLength
                                )
  names(summaryAnalysis) <- c('Genre', 'Size (MB)', 'Sentences', 
                              'Tokens', 'Words', 'Types', 'Mean Sentence Length', 
                              'Mean Word Length')
  return(summaryAnalysis)
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
                    '% Special Chars', '% Contractions', 
                    '% Abbreviations', '% Profanity')
  
  return(audit)
}

## ---- end
#==============================================================================#
#                            Model Corpus Development                          #
#==============================================================================#
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
#' This function takes as its parameter, the coverage table and returns a 
#' coverage plot and table
#' 
#' @param estimates - sample sizes estimate
#' @return coveragePlots - the line chart and table plots
#' @author John James
#' @export
plotCoverage <- function(estimates) {
  
  coverage <- as.data.frame(estimates$coverage)
  
  # Render plot
  coveragePlot <- ggplot(coverage, aes(x=size, y=coverage, group=register)) +
    scale_fill_discrete(name = "Registers") +
    geom_line(aes(color=register)) +
    theme_minimal() + 
    labs(title = 'Coverage by Sample Size', x = 'Tokens', y = 'Coverage(%)') +
    scale_color_brewer(palette = 'Dark2') 
  
  # Prepare table
  names(estimates$sampleSize) <- c('Register', '%', 'Tokens', 'Sample V', 'Extrapolated V',
                                   'Estimated Voov', 'Estimated Noov', 'OOV Rate', 'Coverage')
  coveragePlots <- list(
    plot = coveragePlot,
    tbl = estimates$sampleSize
  )
  
  return(coveragePlots)
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
  design <- coverage[c(1:3), c(1, 3)]
  design$sentLength <- sentLength
  design$sents <- design$'95%' / design$sentLength 
  design$trainSent <- coverage[c(1:3), c(3)] * .8 / design$sentLength
  design$valSent <- coverage[c(1:3), c(3)] * .1 / design$sentLength
  design$testSent <- coverage[c(1:3), c(3)] * .1 / design$sentLength
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

## ---- plot_diversity_table
#------------------------------------------------------------------------------#
#                             plotDiversityTable                               #
#------------------------------------------------------------------------------#
#'  plotDiversityTable
#' 
#' This function takes as its parameter, the growth objects from the 
#' diversity analysis and produces lexical measures table for each register.
#' 
#' @param growth - growth objects from the diversity analysis
#' @return diversity - the diversity table
#' @author John James
#' @export
plotDiversityTable <- function(growth) {
  
  
  registers <- c('Blogs', 'News', 'Twitter')
  diversity <- rbindlist(lapply(seq_along(growth), function(x) {
    measures <- tail(growth[[x]]@data$data, 1)
    measures <- measures[c(-1,-2,-3,-4,-5,-6,-12)]
    measures$Category <- registers[x]
    measures
  }))
  diversity <- diversity[,c(7,3,6,4,5,1,2)]
  return(diversity)
}
## ---- end


## ---- plot_diversity_figure
#------------------------------------------------------------------------------#
#                             plotDiversityFigure                              #
#------------------------------------------------------------------------------#
#'  plotDiversityFigure
#' 
#' This function takes as its parameter, the lexical measures from the 
#' diversity analysis and produces the diversity measurment plots. 
#' 
#' @param measures - diversity analysis
#' @return diversityPlot - the diversity plot
#' @author John James
#' @export
plotDiversityFigure <- function(measures) {
  
  
  
  # Plot Type Token 
  typeTokenPlot <- ggplot(data = measures[[1]], aes(x = Tokens, y = Types, group = Register,
                                                    colour = Register)) +
    geom_line() + geom_point() + theme_minimal(base_size = 8) +
    scale_x_continuous(breaks = round(seq(0, max(measures[[1]]$Tokens), by = 100000), 0)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  
  # Plot Growth Rate 
  growthRatePlot <- ggplot(data = measures[[2]], aes(x = Tokens, y = Growth.Rate, group = Register,
                                                     colour = Register)) +
    geom_line() + geom_point() + theme_minimal(base_size = 8) +
    scale_x_continuous(breaks = round(seq(0, max(measures[[2]]$Tokens), by = 100000), 0)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  
  # Plot TTR 
  ttrPlot <- ggplot(data = measures[[3]], aes(x = Tokens, y = Type.Token.Ratio, group = Register,
                                              colour = Register)) +
    geom_line() + geom_point() + theme_minimal(base_size = 8) +
    scale_x_continuous(breaks = round(seq(0, max(measures[[3]]$Tokens), by = 100000), 0)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  
  # Plot Mean Log Frequency 
  mlfPlot <- ggplot(data = measures[[4]], aes(x = Tokens, y = Mean.Log.Frequency, group = Register,
                                              colour = Register)) +
    geom_line() + geom_point() + theme_minimal(base_size = 8) +
    scale_x_continuous(breaks = round(seq(0, max(measures[[4]]$Tokens), by = 100000), 0)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  
  # Plot Herdans
  herdanPlot <- ggplot(data = measures[[5]], aes(x = Tokens, y = Herdan.s.C, group = Register,
                                                 colour = Register)) +
    geom_line() + geom_point() + theme_minimal(base_size = 8) +
    scale_x_continuous(breaks = round(seq(0, max(measures[[5]]$Tokens), by = 100000), 0)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  
  # Plot Guiraud's R
  guiraudPlot <- ggplot(data = measures[[6]], aes(x = Tokens, y = Guiraud.s.R, group = Register,
                                                  colour = Register)) +
    geom_line() + geom_point() + theme_minimal(base_size = 8) +
    scale_x_continuous(breaks = round(seq(0, max(measures[[6]]$Tokens), by = 100000), 0)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  
  # Plot Yule's K
  yulePlot <- ggplot(data = measures[[7]], aes(x = Tokens, y = Yule.s.K, group = Register,
                                               colour = Register)) +
    geom_line() + geom_point() + theme_minimal(base_size = 8) +
    scale_x_continuous(breaks = round(seq(0, max(measures[[7]]$Tokens), by = 100000), 0)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  
  # Plot Zipf's Slope
  zipfPlot <- ggplot(data = measures[[8]], aes(x = Tokens, y = Zipf.s.Slope, group = Register,
                                               colour = Register)) +
    geom_line() + geom_point() + theme_minimal(base_size = 8) +
    scale_x_continuous(breaks = round(seq(0, max(measures[[8]]$Tokens), by = 100000), 0)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  
  diversityPlots <- list(typeTokenPlot,	growthRatePlot,	ttrPlot,	mlfPlot,	herdanPlot,	
                         guiraudPlot,	yulePlot,	zipfPlot)
  return(diversityPlots)
}
## ---- end

## ---- plot_nGrams
#------------------------------------------------------------------------------#
#                                   plotNgrams                                 #
#------------------------------------------------------------------------------#
#'  plotNgrams
#' 
#' This function plots top features for the document feature matrix provided.  
#' 
#' @param nGram - the document feature matrix meta data
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
  dfNGram <- data.table(Feature = as.factor(names(topFeatures)), 
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
    topTokens = sum(dfNGram$Frequency),
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
#' @return bp - a box plot of features
#' @author John James
#' @export
plotLexicalFeatures <- function(densityAnalysis, posTags) {
  
  posTags <- subset(posTags, Study == TRUE, select = c('Tag', 'Description'))
  
  
  # Extract Features
  features <- subset(densityAnalysis, tags %in% posTags$Tag)
  names(features) <- c('Chunk', 'Tag', 'Freq')
  
  # Add descriptions
  pos <- subset(posTags, select = c(Tag, Description))
  names(pos) <- c('Tag', 'Feature')
  features <- merge(features, pos, by = 'Tag')
  
  # Render plot
  features <- ggplot(features, aes(x=Feature, y = Freq)) + 
    geom_boxplot(aes(fill = Feature)) + labs(x = "Feature", y = "Frequency") + 
    scale_fill_brewer(palette="Paired") + theme_minimal() +
    theme(axis.text.x = element_text(angle=45,hjust=1))
  
  return(features)
}

## ---- end  


## ---- plot_coverage
#------------------------------------------------------------------------------#
#                                plotNGramCoverage                             #
#------------------------------------------------------------------------------#
#'  plotNGramCoverage 
#' 
#' This function takes as its parameter, the coverage table and returns a 
#' coverage plot and table
#' 
#' @param coverage - sample sizes estimate
#' @param type - the type of nGram
#' @param color - the color of the line
#' @return coveragePlots - the line chart and table plots
#' @author John James
#' @export
plotNGramCoverage <- function(coverage, type, color) {
  
  coverage <- as.data.frame(coverage)
  
  vLines <- with(coverage, approx(Coverage, Rank, c(50,75,95)))
  
  
  # Render plot
  coveragePlot <- ggplot(coverage, aes(x=Rank, y=Coverage)) +
    geom_line(colour=color) + 
    geom_vline(xintercept = vLines$y) +
    ggplot2::annotate('text', x=vLines$y, y=c(50, 75, 95), label=round(vLines$y), color='grey10') +
    theme_minimal() + 
    labs(title =  paste(type, 'Coverage by Rank'), x = 'Rank', y = 'Coverage(%)') +
    scale_color_brewer(palette = 'Dark2') 

  return(coveragePlot)
}
## ---- end

