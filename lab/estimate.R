estimate <- function(s, n) {
  if (n == 1) {
    alpha <- model[[n]][ nGram == s ]
    if (nrow(alpha) > 0) {
      return(alpha[, alpha])
    } else {
      return(model[[n]][ nGram == 'UNK'][, alpha])
    }
    
  } else {
    current <- model[[n]][nGram == s][,.(nGram, context, suffix, alpha, lambda)]
    
    # If ngram found, return alpha and lambda from training data and recurse
    if (nrow(current) > 0) {
      alpha  <- current[, alpha]
      lambda <- current[, lambda]
      suffix  <- paste(unlist(strsplit(s, " "))[2:n], collapse = ' ')
      return(alpha + lambda * estimate(suffix, n-1))
      
      # If not found, set alpha to zero, compute lambda and recurse
    } else {
      alpha <- 0 # initialize alpha

      # Set context and suffix
      kontext <- paste(unlist(strsplit(s, " "))[1:n-1], collapse = ' ')
      suffix  <- paste(unlist(strsplit(s, " "))[2:n], collapse = ' ')
      
      # Get lambda for new context and if available...
      setkey(model[[n]], context)
      current <- model[[n]][context == kontext]
      if (nrow(current) > 0) {
        lambda <- current[,lambda][1]
        return(alpha + lambda * estimate(suffix, n-1))

      # If context not available, use context for unknown nGram
      } else {
        kontext <- paste(rep("UNK", n-1), collapse = ' ')
        lambda <- model[[n]][context == kontext][1][, lambda]
        return(alpha + lambda * estimate(suffix, n-1))
      }
    }
  }
}  