


f <- function(x) {
  return(-x^{2} + 10*sin(x))
}

Gen0 <- function(n, locale) {
  return(runif(n, locale[1],locale[2]))
} 

BreedProb <- function(orgs, f) {
  p <- exp(f(orgs))/sum(exp(f(orgs))) #probability of breeding array, length n organisms
  df <- data.frame(orgs,p)
  df <- df[order("p")]
  return(df)
}

Breed <- function(bp) {
  lucky_couple <- sort(bp[[2]], decreasing = TRUE)[1:2]
  print(lucky_couple[1])
  print(lucky_couple[2])
  babies <- runif(length(bp[[1]]),lucky_couple[2], lucky_couple[1])
  return(babies)
}



BreedProb.2 <- function(orgs, f) {
  p <- exp(f(orgs))/sum(exp(f(orgs))) #probability of breeding array, length n organisms
  df <- data.frame(orgs,p)
  output <- df[order(df$p),] # orders by probability, ascending 
  return(output) # we want to return organisms ordered by probability of reproduction
}

Breed.2 <- function(bp) { # functionally this has stayed the same
  # select parents using multinom
  newparents = rmultinom(1, size = 2, prob = output$p) # not sure how to select parents
  lucky_couple <- sort(bp[[1]], decreasing = TRUE)[1:2] # organisms with highest reproduction probabilities
  print(lucky_couple[1])
  print(lucky_couple[2])
  babies <- runif(length(bp[[1]]),lucky_couple[2], lucky_couple[1]) 
  return(babies)
}


generations <- 3
orgs <- list(Gen0(10, c(-10,10)))
bp = c()
for (g in (2:(generations + 1))) {
  bp <- BreedProb.2(orgs[[g-1]],f)
  orgs[[g]] <- Breed.2(bp)
}
orgs 


## plotting
xvec = seq(-10,10,.01)
plot(f(xvec)~xvec, type = 'l', ylim = c(-100,20), xlim = c(0,10))
colvec = c('red', 'orange', 'green', 'purple', 'cyan')

for(ii in c(1:(generations+1))){
  points( orgs[[ii]],rep(0, length(orgs[[ii]])) , col = colvec[ii])
}
# might be overlapping too much to see a difference