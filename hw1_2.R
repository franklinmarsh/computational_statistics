init <- 0.5 #initialize the machine-e test variable
final <- 1 # the final value that we will arrive at is 1

while (final + init >= final) #while 1 plus the macihne-e test value is still greater than 1
  init <- init/2.0 #divide our initial guess in half

print(init)