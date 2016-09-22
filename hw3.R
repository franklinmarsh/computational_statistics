GenX0 <- function(m) {
  return(c(1:m))
}

# A function which generates all m choose 2 possibles 2 elements swaps from a permutation
GenSwaps <- function(x0) {
  # Input is a vector of the permutation in order
  out <- c() #initialize empty list to hold all possible permutations with 1 swap
  i <- 0
  
  for (j in (1:length(x0))) {
    i <- i + 1
    for (k in (1:length(x0))) {
      i < i + 1
      out[i] <- replace(x0, c(j,k), x0[c(k,j)])  
    }
  }
  return(out)
}

# A function to check the size of a permutation
FindSize <- function(x) {
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