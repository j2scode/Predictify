## ---- mkn_adjusted_counts

#==============================================================================#
#                               mknHistories                                   #
#==============================================================================#
#'  mknHistories
#' 
#' This function takes as its parameter, the meta data for mkn model and,
#' for each nGram except the highest order nGrams, calculates the number of 
#' histories in which the nGram appears once, twice and 3 or more times.
#' 
#' @param mkn - the meta data for the MKN language model
#' @param N - the order of the nGram model
#' @author John James
#' @export
mknHistories <- function(mkn, N) {
  
  startTime <- Sys.time()
  
  message(paste('\nCounting sequences for which the ngram occurs in history at', startTime))
  
  model <- lapply(seq_along(mkn$counts), function(x) {
    loadObject(mkn$counts[[x]])
  })

  lapply(seq_along(model), function(x) {
    if (x < N) {
      message(paste('...counting number of histories for', 
                    mkn$counts[[x]]$fileDesc))
      
      current <- model[[x]]
      higher  <- model[[x+1]]

      # Extract histories
      histories1  <- higher[count == 1,.(context)]
      histories2  <- higher[count == 2,.(context)]
      histories3  <- higher[count > 2,.(context)]
      
      # Count number of rows for each group
      counts1 <- histories1[,.(n1wdot = .N), by = context]
      counts2 <- histories2[,.(n2wdot = .N), by = context]
      counts3 <- histories3[,.(n3pwdot = .N), by = context]
      
      # Merge counts into current ngram table
      current <- merge(current, counts1, by.x = 'nGram', by.y = 'context', all.x = TRUE)
      current <- merge(current, counts2, by.x = 'nGram', by.y = 'context', all.x = TRUE)
      current <- merge(current, counts3, by.x = 'nGram', by.y = 'context', all.x = TRUE)

      # Clear all NA values
      for (i in seq_along(current)) set(current, i=which(is.na(current[[i]])), j=i, value=0)
      
      # Save  counts
      mkn$counts[[x]]$data <- current
      saveObject(mkn$counts[[x]])
      
    }
  })
  
  # Log Results
  logR('MKN Histories', startTime, ' ',' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('MKN History Counts Calculated at ', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
#mknAdjustedCounts(lm$mkn)