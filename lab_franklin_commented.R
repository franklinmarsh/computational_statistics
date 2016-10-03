h <- function(x) {
  # the function that we will be maximizing
  return(sin(10*x) - x^(2))
}

Walk <- function(h, eps, tau, input) {
  
  e <- exp(1) #make e easy to write
  
  candidate <- input + runif(1,-eps, eps)
  prob <- e^((h(candidate) - h(input))/(tau))
  
  if (prob > 1) {
    out <- candidate
  }
  
  else {
    out <- input
  }
  
  return(out)
}

SimAnnealingWalk <- function(h, x, eps, tau_range, tau_delta) {
  
  path <- c(x)
  
  tau_schedule <- seq(tau_range[1], tau_range[2], -tau_delta)
  n_iters <- length(tau_schedule)
  
  for (i in (2:(n_iters + 1))) {
    path[i] <- Walk(h,eps,tau_schedule[i-1],path[i-1])
  }
  
  #plot magic
  plot(path, h(path), pch = '.')
  #abline(v = path[-1], col = 'red')
  return(path)
}

#run the code and produce a plot
mypath <- SimAnnealingWalk(h, 0, 0.1, c(1E10,(1E10 - 100)), 0.01)
abcissa <- seq(-10,10, 0.01)
plot(abcissa, h(abcissa), type = 'l')
abline(v = mypath[length(mypath)])
print(mypath[length(mypath)])