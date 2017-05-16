#==============================================================================#
#                              xChangeText                                     #
#==============================================================================#
#'  xChangeText
#' 
#' This function takes as its parameter, a tm volatile corpus, and a data frame
#' with two columns, key and value.  The function replaces every instance of key
#' with value in the corpus and returns it to the calling environment.
#' 
#' @param korpus - the tm corpus object
#' @param keyValue - the data frame containing the key / value pairs
#' @return clean - the "cleaned" corpus with keys replaced with values.
#' @author John James
#' @export
xChangeText <- function(korpus, keyValue) {
  
  # Define content transformer
  xChange <- content_transformer(function(x, pattern, replace) gsub(pattern, replace, x, perl = TRUE))
  
  # Define function to iterate through key/value pairs.
  
}