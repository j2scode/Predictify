## ---- not_in

#==============================================================================#
#                                notIN                                         #
#==============================================================================#
#'  notIn
#' 
#' This function provices "not in" functionality
#' 
#' @param x - the meta data for the processed training set ngrams
#' @param y - the meta data for the MKN language model
#' @return result - returns a logical 
#' @author John James
#' @export
"%!in%" <- function(x,y) {
  !('%in%'(x,y))
}