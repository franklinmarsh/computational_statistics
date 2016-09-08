G.Prime <- function(x) {
  #returns the value of the function G.prime evaluated at x.
  #
  # Args:
  #   x: the value at which to evaluate the function
  #
  # Returns:
  #   g'(x)  
  return((1 + 1/x - log(x)) / ((1+x) ^ 2))
}


Bisector <- function(ifunc, a0, b0) {
  # finds the root of a function (inputfunc) using the bisection method
  #
  # Args:
  #   ifunc: the function that we wish to find the root of
  #   a0: initial guess for the left bound of the bisector
  #   b0: initial guess for the right bound of the bisector
  
  #the initial guess, constructed out of the left and right bounds
  iters = 0
  

  if (ifunc(a0)*ifunc(b0) <= 0) {
    x0 <- (a0 + b0)/2.0
    
  # if ifunc(a0) > 0 vs. if ifunc(a0) < 0   
    
  }
  
  
}