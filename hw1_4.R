G.1 <- function(x) {
  # returns the value of the function $G_{1}$ in problem 4.
  #
  # Args:
  #   x: the value at which to evaluate the function
  #
  # Returns:
  #   $G_{1}(x)$
  
  e <- exp(1)
  
  return((x + e^(-x))/2.0)
}

G.2 <- function(x) {
  # returns the value of the function $G_{2}$ in problem 4.
  #
  # Args:
  #   x: the value at which to evaluate the function
  #
  # Returns:
  #   $G_{2}(x)$
  
  e <- exp(1)
  
  return(e^(-x))
}

G.3 <- function(x) {
  # returns the value of the function $G_{3}$ in problem 4.
  #
  # Args:
  #   x: the value at which to evaluate the function
  #
  # Returns:
  #   $G_{3}(x)$
  
  e <- exp(1)
  
  return(-log(x))
}

G2.prime <- function(x) {
  # returns the value of the function $g'_{x}$ in problem 4.
  #
  # Args:
  #   x: the value at which to evaluate the function
  #
  # Returns:
  #   $g'(x)$

  return(x + log(x))
}

IterRecord <- function(inputfunction, seed, n_iter) {
  
  # iterate a recursive function over a list of numbers, recording each input and output.
  #
  # Args:
  #   inputfunction: the "seed" or $X_{0}$, initial value
  #   seed: the input for the first run of inputfunction
  #   n_iter: the number of iterations to be performed
  #   
  #   
  # 
  # Returns:
  #   results: list of function outputs.
  
  results <- list(seed)
  
  for (i in 2:n_iter) {
    results[i] <- inputfunction(as.numeric(results[i-1]))
  }
  
  return(as.numeric(results))
}
