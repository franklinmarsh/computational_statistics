h <- function(x) {
  # the function that we will be maximizing
  return(sin(10*x) - x^(2))
}

#'function that explores local neighborhood
#'@param x inital starting value
#'@param tao the value of tao
#'@param h function that we want to maximize
#'@param num.iter number of iterations
#'
explore<-function(x, tao, h, num.iter, eps){
  i=1
  while(i<num.iter){
  y <- x + eps*runif(1,-1,1)
  val=min(1, exp((h(y)-h(x))/tao)) #probability of moving
  if(runif(1)<val) #update according to the probability
    x=y
  i=i+1
  }
  return(x)
}

#'function that simulates annealing
#'@param x inital starting value
#'@param h function that we want to maximize
#'@param num.iter number of iterations
#'@param tao.length length of tao
#'
sim.annealing<-function(x, h, tau_range, num.iter, tao.length, eps){
  
  path <- c(x)
  tao<-(seq(tau_range[1], tau_range[2], length.out=tao.length))

  for(i in (2:length(tao) + 1)){
    x=explore(x, tao[i - 1], h, num.iter,eps)
    path[i] <- x 
  }
  return(path)
}

mypath <- sim.annealing(-5, h,c(7,-3), 10, 1000, 0.1)
plot(mypath, type = 'l')
print(mypath[length(mypath)])