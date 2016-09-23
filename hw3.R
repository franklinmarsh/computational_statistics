GenX0 <- function(m) {
  return(c(1:m))
}

GenSwaps <- function(x0) {
  
  out <- list() #initialize empty list to hold all possible swaps
  i <- 0
  
  for (j in x0) {
    i <- i + 1
    for (k in (1:length(x0))) {
      i < i + 1
        out[[i]] <- replace(x0, c(j,k), x0[c(k,j)])  
    }
  }
  return(out)
}

GenOneSwap <- function(x0) {
  
  out <- x0 #initialize empty list to hold all possible swaps

  j <- sample((1:length(x0)),1) #pull and a random index from x0
  k <- sample((1:length(x0)),1) #pull another random index from x0
  
  while (identical(out, x0) == TRUE) { #if we generate an identical object, keep going
      out <- replace(x0, x0[c(j,k)], x0[c(k,j)])  
  }
  return(out)
}


FindSize <- function(x) {
  
  sizes <- c()
  
  for (j in (1:length(x))) {
      sizes[j] <- x[j]*j
  }
  
  return(sum(sizes))
}

FindSize(GenOneSwap(GenX0(4)))

