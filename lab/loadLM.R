## ---- load_lm
#==============================================================================#
#                                 loadLM                                       #
#==============================================================================#
#'  loadLM
#' 
#' This function loads the language models from a list into individual data 
#' tables 
#' 
#' @param mkn - the meta data for the language model
#' @author John James
#' @export
loadLM <- function(mkn) {
  
  
  startTime <- Sys.time()
  message(paste("Loading", mkn$mDesc, 'at', startTime))
  
  n1 <- loadObject(mkn$counts[[1]])
  n2 <- loadObject(mkn$counts[[2]])
  n3 <- loadObject(mkn$counts[[3]])
  n4 <- loadObject(mkn$counts[[4]])

  discounts <- loadObject(mkn$discounts)
  summary <- loadObject(mkn$summary)

}
## ---- end
#loadLM(lm$mkn$alpha)