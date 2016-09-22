GenX0 <- function(m) {
  return(c(1:m))
}

GenSwaps <- function(x0) {
  
  out <- c() #initialize empty list to hold all possible swaps
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

FindSize <- function(x) {
  
  sizes <- c()
  
  for (j in (1:length(x))) {
      sizes[j] <- x[j]*j
  }
  
  return(sum(sizes))
}