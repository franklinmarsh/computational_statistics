MCG <- function(x, a = 166425,c = 1013904223 ,m = 2^32) {
  # mixed congruential generator function
  #
  # Args:
  #   x: the "seed" or $X_{0}$, initial value
  #   a: the multiplier, 0 < a < m
  #   c: the increment, 0 <= c < m
  #   m: the modulus
  # 
  # Returns:
  #   x_1: the next number in the sequence.
  
  return(((a*x + c) %% m))
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
  
  for (i in 2:n_iter)
    results[i] <- inputfunction(results[i-1])
  
  return(results)
}