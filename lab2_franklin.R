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
             

generations <- 1
orgs <- list(Gen0(10, c(-10,10)))

for (g in (2:(generations + 1))) {
  bp <- BreedProb(orgs[[g-1]],f)
  orgs[[g]] <- Breed(bp)
}