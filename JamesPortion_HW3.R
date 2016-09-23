# Pick a random neighbor
RandomNeighbor <- function (neighbors) {
  # Input is a vector of the neighbors
  index <- runif (1, 1, length(neighbors))
  return (neighbors[index])
}

# A function which returns TRUE if the algrithm says to switch and FALSE if not
ChangeCheck <- function (nX, nY) {
  # Inputs are n(x) and n(y) - the number of neighbors of the start x and the
  # proposed neighbor to switch to y
  # If n(x) is greater than n(Y) always switch
  if (nX >= nY) {
    return (TRUE)
  }
  # Otherwise switch with probability n(x)/n(y)
  else {
    # Create a unif between 0 and 1 and if it is less than n(x)/n(y) switch
    p <- nX/nY
    u <- runif(1, 0, 1)
    if (u <= p) {
      return (TRUE)
    }
    else {
      return (FALSE)
    }
  }
}