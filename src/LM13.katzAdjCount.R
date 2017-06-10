## ---- katz_adj_count

#==============================================================================#
#                              katzAdjCount                                    #
#==============================================================================#
#'  katzDiscount
#' 
#' This function calculates the adjusted or discounted counts.
#' 
#' @param katz - the meta data for the Katz language model
#' @param discount - the amount of nGram frequency discount.  If null, good-
#'                   turing estimates will be used.
#' @param directories - the project directory structure
#' 
#' @author John James
#' @export
katzAdjCount <- function(katz, discount = NULL, directories) {
  
  startTime <- Sys.time()
  
  message(paste("\nCalculating Katz Adjusted Counts for Katz model at", startTime))
  
  summaryCounts <- rbindlist(lapply(seq_along(katz$counts), function(x) {
    if (x > 1) {
      message(paste('...calculating Katz Adjusted Counts for', katz$counts[[x]]$fileDesc))
      
      # Load counts and frequencies of counts
      current <- loadObject(katz$counts[[x]])
      freqs <- loadObject(katz$freq[[x]])
      k <- 5 # Threshold up to which, discounts are applied
  
      if (is.null(discount)) {
        # Compute Good Turing Estimate
        # Calculate discount ratio for r > k
        current[ r > k, dr := 1]
    
        # Compute r*
        rStar <- unlist(lapply(seq(1:k), function(s) {
          rStar <- (s + 1) * freqs[r == (s+1), Nr] / freqs[r == s, Nr]
          rStar
        }))
    
        # Compute expected value component of equation
        e <- ((k+1) * freqs[r == (k+1), Nr] / freqs[r == 1, Nr])
        
        # Compute discount ratio for 1<= r <= 5
        disc <- unlist(lapply(seq(1:k), function(s) {
          dr <- ((rStar[s] / s) - e) / (1 - e)
          dr
        }))
        
        # Populate data table with discount ratio values
        setkey(current, r)
        for (i in 1:k) {
          current <- current[r == as.numeric(i), dr := disc[i]]
        }
      
        # Populate table with cKatz
        current[, cKatzNGram := dr * r]
      } else {
        # Use fixed discount
        current[, cKatzNGram := ifelse(r > k, r, r - discount)]
      }    
      
      # Save counts
      katz$counts[[x]]$data <- current
      saveObject(katz$counts[[x]])
      
      # Update summary with total adjusted counts
      ttlKatz <- loadObject(katz$summary)
      ttlKatz[mOrder == x, cKatzNGram := sum(current[, cKatzNGram])]
      ttlKatz[mOrder == x]
    }
  }))
  
  # Save Summary Counts
  katz$summary$data <- summaryCounts
  saveObject(katz$summary)

  # Log Results
  logR(paste(katz$mName, 'katz-adjusted-counts'), startTime, 
       katz$counts[[1]]$directory, 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('...Katz Adjusted Counts Completed at ', endTime))
  message(paste('...Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(summaryCounts)
  
}
## ---- end
#summary <- katzAbsCount(corpora$train$processedData$nGrams$text, lm$katz)