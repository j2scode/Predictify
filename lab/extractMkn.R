extractMkn <- function(lm) {
  
  # This function writes data frames to single excel file with multiple tabs
  library(xlsx)
  
  # Load Models
  mknUnigrams <- loadObject(lm$counts[[1]])
  mknBigrams <- loadObject(lm$counts[[2]])
  mknTrigrams <- loadObject(lm$counts[[3]])
  mknQuadgrams <- loadObject(lm$counts[[4]])
  mknDiscounts <- loadObject(lm$discounts)
  mknNGramCountSummary <- loadObject(lm$summary)
  
  write.xlsx(mknUnigrams, file="./notes/mknTables.xlsx", sheetName="Unigram", row.names=FALSE)
  write.xlsx(mknBigrams, file="./notes/mknTables.xlsx", sheetName="Bigram", row.names=FALSE, append = TRUE)
  write.xlsx(mknTrigrams, file="./notes/mknTables.xlsx", sheetName="Trigram", row.names=FALSE, append = TRUE)
  write.xlsx(mknQuadgrams, file="./notes/mknTables.xlsx", sheetName="Quadgram", row.names=FALSE, append = TRUE)
  write.xlsx(mknDiscounts, file="./notes/mknTables.xlsx", sheetName="Discounts", row.names=FALSE, append = TRUE)
  write.xlsx(mknNGramCountSummary, file="./notes/mknTables.xlsx", sheetName="Summary", row.names=FALSE, append = TRUE)
  
}