h <- function(x) {
  # the function that we will be maximizing
  return(sin(10*x) - x^(2))
}


Step <- function(h, h_range, tau, input) {
  
  e <- exp(1) #make e easy to write
  
  candidate <- runif(1,h_range[1], h_range[2])
  prob <- e^((h(candidate) - h(input))/(tau))
  
  if (prob > 1) {
    out <- candidate
  }
  
  else {
    out <- input
  }
  
  return(out)
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

SimAnnealing <- function(h, h_range, tau_range,tau_delta) {
  
  path <- c(runif(1,h_range[1], h_range[2]))
  
  tau_schedule <- seq(tau_range[1], tau_range[2], -tau_delta)
  n_iters <- length(tau_schedule)
  
  for (i in (2:(n_iters + 1))) {
    path[i] <- Step(h, h_range, tau_schedule[i-1],path[i-1])
  }
  return(path)
}

SimAnnealingWalk <- function(h, x, eps, tau_range, tau_delta) {
  
  path <- c(x)
  
  tau_schedule <- seq(tau_range[1], tau_range[2], -tau_delta)
  n_iters <- length(tau_schedule)
  
  for (i in (2:(n_iters + 1))) {
    path[i] <- Walk(h,eps,tau_schedule[i-1],path[i-1])
  }
  return(path)
}