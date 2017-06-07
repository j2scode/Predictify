## ---- mkn_lambda

#==============================================================================#
#                               mknLambda                                      #
#==============================================================================#
#'  mknLambda
#' 
#' This function calculates the scaling factor lambda for lower order model
#' 
#' @param mkn - the meta data for the MKN language model
#' @author John James
#' @export
mknLambda <- function(mkn) {
  
  startTime <- Sys.time()
  memory.limit(20000)
  
  message(paste('\nCalculating scaling factor lambda at', startTime))
  
  message('...loading nGram counts and discounts')
  model <- lapply(seq_along(mkn$counts), function(x) {
    loadObject(mkn$counts[[x]])
  })
  discounts <- loadObject(mkn$discounts)
  summary <- loadObject(mkn$summary)

  lapply(seq_along(model), function(x) {
    
    if (x < mkn$mOrder) {
      message(paste('...calculating scaling factor lambda for', 
                    mkn$counts[[x]]$fileDesc))
      #Obtain Model
      current <- model[[x]]

      # Extract discounts   
      D1 <- discounts[x+1,4]$D1
      D2 <- discounts[x+1,5]$D2
      D3 <- discounts[x+1,6]$D3
      
      # Calculate DnNn for context and add to lower level N-gram
      D <- current[,.(nGram, n1wdot, n2wdot, n3pwdot)]
      D <- D[, D1N1 := D1 * n1wdot]
      D <- D[, D2N2 := D2 * n2wdot]
      D <- D[, D3N3 := D3 * n3pwdot]
      D <- D[, DnNn := D1N1 + D2N2 + D3N3] 
      D <- D[, c('n1wdot', 'n2wdot', 'n3pwdot') := NULL]
      current <- merge(current, D, by = 'nGram')
      
      # Compute lambda
      if (x < mkn$mOrder-1) {
        current <- current[, lambda := DnNn / summary[x+1,2]$Count]
      } else {
        current <- current[, lambda := DnNn / count]
      }

      # Save  counts
      mkn$counts[[x]]$data <- current
      saveObject(mkn$counts[[x]])
      
    }
  })
  
  # Log Results
  logR('MKN Lambda Counts', startTime, mkn$counts[[1]]$directory,' ')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('MKN Lambda Calculated at ', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
#mknAdjustedCounts(lm$mkn)