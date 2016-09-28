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
explore<-function(x, tao, h, num.iter){
  i=1
  while(i<num.iter){
  y=runif(1, -2*pi, 2*pi)
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
sim.annealing<-function(x, h, num.iter, tao.length){
  tao<-seq(.001, 100, length.out=tao.length)
  for(i in 1:length(tao)){
    x=explore(x, tao[i], h, num.iter)
  }
  return(x)
}