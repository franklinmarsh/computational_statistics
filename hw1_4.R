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
  # returns the value of the function $G_{3}$ in problem 4.
  #
  # Args:
  #   x: the value at which to evaluate the function
  #
  # Returns:
  #   $G_{3}(x)$

  return(x + log(x))
}

Bisector <- function(ifunc, a0, b0) {
  # finds the root of a function (inputfunc) using the bisection method
  #
  # Args:
  #   ifunc: the function that we wish to find the root of
  #   a0: initial guess for the left bound of the bisector
  #   b0: initial guess for the right bound of the bisector
  
  x0 <- (a0 + b0)/2.0 #the initial guess, constructed out of the left and right bounds
  iters = 0
  
  while (iters < 5) {
    if (ifunc(a0)*ifunc(b0) <= 0) {
    }
  }
    
  
}