# A function to generate the largest permutation given a vector of values or an m
# which implies that the vector of values is the first m natural numbers
GenX0 <- function(m, initialPerm) {
  # The input should be either an inital permutation of an m
  # If both are given, the m will be used
  if (!is.null(m)){
    return(c(1:m))
  }
  # If m is null, sort the initial vector into increasing order
  else {
    return (sort(initialPerm(decreasing = FALSE)))
  }
}

GenOneSwap <- function(x0) {
  
  out <- x0 #initialize empty list to hold all possible swaps
  
  while (identical(out, x0) == TRUE) { #if the input and output are identical
        j <- sample((1:length(x0)), 1) #pick an index at random
        k <- sample((1:length(x0)), 1) #pick another index at random
    out <- replace(x0, x0[c(j,k)], x0[c(k,j)]) #swap those two elements.
  }
  return(out) 
}

FindSize <- function(x) {
  
  # A function to check the size of a permutation
  # Input is a vector of the permutation in order
  sizes <- c()
  # Loop through each index of the permutation and multiply by the index
  for (j in (1:length(x))) {
    # Store these values in a vector
    sizes[j] <- x[j]*j
  }
  # Return the sum of the vector of products
  return(sum(sizes))
}