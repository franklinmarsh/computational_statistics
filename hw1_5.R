MCG <- function(x, a = 1664255,c = 1013904223 ,m = 2^10) {
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
  
  for (i in 2:n_iter) {
    results[i] <- inputfunction(as.numeric(results[i-1]))
  }
  
  return(results)
}

rand_numbers = IterRecord(MCG, 2, 1000)
xi <- rand_numbers[1:length(rand_numbers) - 1]
xi_plusone <- rand_numbers[2:length(rand_numbers)]

plot(xi, xi_plusone)

MiddleSquare <- function(seed) {
  # compute a 'random' sequence of four-digit numbers using the Middle Square method.
  #
  # Args:
  #   seed: the intial four digit number
  # 
  # Returns:
  #   rand: new quasi-random number
  
  options("scipen" = 100, "digits" = 4) #disable scientific notation so the character operation works.
  seed_squared <- toString(seed^(2.0))
  rand <- as.numeric(substr(seed_squared, 3,6)) #take the middle slice out of an 8 digit number and return to numeric 
  options("scipen"= 8, "digits" = 4)
  
  return(rand)
}
