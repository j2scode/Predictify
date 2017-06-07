## ---- katz_discount

#==============================================================================#
#                              katzDiscount                                    #
#==============================================================================#
#'  katzDiscount
#' 
#' This function calculates the discount ratio and katz cound discount
#' 
#' @param katz - the meta data for the Katz language model
#' 
#' @author John James
#' @export
katzDiscount <- function(katz) {
  
  startTime <- Sys.time()
  
  message(paste("\nCalculating Katz Discount Ratio for Katz model at", startTime))
  
  lapply(seq_along(katz$counts), function(x) {
    message(paste('...calculating Katz Discount Ratio for', katz$counts[[x]]$fileDesc))
    
    # Load counts and frequencies of counts
    current <- loadObject(katz$counts[[x]])
    freqs <- loadObject(katz$freq[[x]])

    # Calculate discount ratio for r > k
    k <- 5
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
  
    # Populate table with drr, the applied discount
    current <- current[, drr := dr * r]
    
    # Save counts
    katz$counts[[x]]$data <- current
    saveObject(katz$counts[[x]])

  })
  
  # Log Results
  logR(paste(katz$mName, 'katz-discounts'), startTime, 
       katz$counts[[1]]$directory, 'various')
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('...Katz Discounts completed at ', endTime))
  message(paste('...Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
}
## ---- end
#summary <- katzAbsCount(corpora$train$processedData$nGrams$text, lm$katz)