## ---- mkn_discount

#==============================================================================#
#                                mknDiscount                                   #
#==============================================================================#
#'  mknDiscount
#' 
#' This function takes as its parameter, the meta data for mkn model and returns
#' a data table containing the D1, D2, D3+ discounts for each ngram, saves to 
#' disc and returns it to the calling environment
#' 
#' @param mkn - the meta data for the MKN language model
#' @return discounts - data frame with discounts for each ngram
#' @author John James
#' @export
mknDiscount <- function(mkn) {
  
  startTime <- Sys.time()
  
  message(paste("\nCalculating discounts for MKN model at", startTime))
  
  discounts <- rbindlist(lapply(seq_along(mkn$args$counts), function(x) {
    message(paste('...calculating discounts for', mkn$args$counts[[x]]$fileDesc))
    nGram <- loadObject(mkn$args$counts[[x]])
    setkey(nGram, count)
    n1 <- nrow(nGram[count == 1])
    n2 <- nrow(nGram[count == 2])
    n3 <- nrow(nGram[count == 3])
    n4 <- nrow(nGram[count == 4])
    D <- n1 / (n1 + 2*n2)
    D0 <- 0
    D1 <- 1 - (2*D*(n2/n1))
    D2 <- 2 - (3*D*(n3/n2))
    D3 <- 3 - (4*D*(n4/n3))
    data.frame(nGramOrder = x, D = D, D0 = D0, D1 = D1, D2 = D2, D3 = D3) 
  }))
  
  mkn$args$discounts$data <- discounts
  saveObject(mkn$args$discounts)

  # Log Results
  logR('MKN Discount Calculation', startTime, mkn$args$discounts$directory,
       mkn$args$discounts$fileName)
  
  # Alert User
  endTime <- Sys.time()
  message(paste0('MKN Discounts Calculated at ', endTime))
  message(paste('Elapsed time is', round(difftime(endTime, startTime,  units = 'auto'), 2)))
  
  return(mkn$args$discounts$data)
}
## ---- end
#discounts <- mknDiscounts(lm$mkn)