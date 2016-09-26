# A function to generate the largest permutation given a vector of values or an m
# which implies that the vector of values is the first m natural numbers
GenX0 <- function(m = NULL, initialPerm = NULL) {
  # The input should be either an inital permutation of an m
  # If both are given, the m will be used
  if (!is.null(m)){
    return(c(1:m))
  }
  # If m is null, sort the initial vector into increasing order
  else {
    return (sort(initialPerm, decreasing = FALSE))
  }
}

# A function which swaps two elements given a vector and the indeces to be swapped
Swap <- function (x, i, j) {
  # x is vector of perm'n and i and j are the indeces to be swapped
  # Save the ith element
  tempI <- x[i]
  # Change the ith element to the jth element
  x[i] <- x[j]
  # Change the jth element to the ith element
  x[j] <- tempI
  # Return the new vector
  return (x)
}

# A function which generates all m choose 2 possibles 2 elements swaps from a permutation
GenSwaps <- function(x) {
  # Input is a vector of the permutation in order
  # Initialize empty matrix to hold all possible permutations with 1 swap
  # 1 Column = 1 Permutation
  output <- matrix(data = NA, nrow = choose(length(x)-1, 2), ncol = length(x))
  index <- 1
  # Loop through each element and generate the swaps with all the elements after it
  for (i in (1:length(x))) {
    for (j in (i+1:length(x)-1)) {
      # Add each new potential neighbor to the vector to store them
      output[index,] = Swap(x, i, j)
      index <- index + 1
    }
  }
  return(output)
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

# A function to pick a random neighbor
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

# A function to find which of the possible swaps are neighbors when given the swaps and the k size cutoff
WhichNeighbors <- function (possibleSwaps, k) {
  neighbors <- c()
  # Loop through each swap checking if its size is greater than k
  for (j in 1:length(possibleSwaps)) {
    if (FindSize(possibleSwaps[j]) >= k){
      # If the swap produces a neighbor store it in the neighbors vector
      append(neighbors, possibleSwaps[j], after = length(neighbors))
    }
  }
  return (neighbors)
}

# A markov chain monte carlo algorithm to find the average size of permutations
# whose size is greater than a given k
MCMC <- function (m = NULL, initPerm = NULL, k) {
  # The input should be either an inital permutation or an m implying use the first m natural numbers
  # and a k which is the lower cutoff for "large" permutations
  # Start by creating a vector to store the mean of the sizes of large perm'ns
  # Also store the number that have been added so we can calc avg size
  bigPermAvg <- c(NULL, 0)
  # Generate the first permutation (the largest one)
  x <- GenX0(m, initPerm)
  # Confirm its size is greater than k
  if (FindSize(x) < k) {
    return (NULL)
  }
  # Store the size of x0 and update the number in bigPermAvg[2]
  bigPermAvg[1] <- FindSize(x)
  bigPermAvg[2] <- bigPermAvg[2] + 1
  # Designate the number of iterations to loop through
  n = 1000
    for (i in 1:n) {
      # Generate all possible neighbors of x
      xSwaps <- GenSwaps(x)
      # Find which of the possible neighbors actually are neighbors (i.e. size >= k)
      xNeighbors <- WhichNeighbors(xSwaps, k)
      # Loop through possible y permutations until we find one to switch to
      swapped <- FALSE
      while (!swapped){
        # Pick a random neighbor to possibly switch to
        y <- RandomNeighbor(xNeighbors)
        # Generate the neighbors of y
        ySwaps <- GenSwaps(y)
        yNeighbors <- WhichNeighbors(ySwaps, k)
        # If n(x) > n(y) or we choose to switch with prob n(x)/n(y) switch to the y permutation
        if (ChangeCheck(length(xNeighbors), length(yNeighbors))) {
          x <- y
          swapped <- TRUE
        }
      }
      # Update the average big size and the number of perm'ns that have been used
      bigPermAvg[2] <- bigPermAvg[2] + 1
      c <- bigPermAvg[2]
      bigPermAvg[1] <- bigPermAvg[1]*((c-1)/c)+(FindSize(x)/c)
    }
  # Find the average size of all of the big permutations we've visited and return this value
  return (bigPermAvg)
}