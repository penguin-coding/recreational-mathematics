
sieve.with.marker = function(n){
  # purpose : generates the sieve of eratosthenes' with a counter on each number, 
  #           representing the number of times it was crossed out.
  # input   : a positive integer, n ; the maximum value to be represented by the
  #           sieve
  # output  : a vector of length n-1 where the ith element represents the number
  #           of times the jth element of the vector (2,3,4,...,n) was crossed 
  #           out in eratosthenes' sieve
  
  # standard input checks:
  n <- as.integer(n)
  if (n<0 | n%%1!=0) stop('sieve.with.marker expects positive integer input')
  
  u <- floor(sqrt(n)) + 1 # The highest number we need consider
  
  output <- rep(0,n-1)    # The vector which stores the number of crosses
  
  for(i in 2:u){
    
    # The number the index of the list of interest for number i is i-1
    # i.e index 2-1=1 represents the number i=2.
    
    for (j in i+1:n-1){
      # check each element is divisible by i with a 'good enough' test:
      if (abs((j+1)%%i)<1e-8) output[j] <- output[j] + 1 # if yes, add a cross
    }
  }
  
  output <- output[1:n-1] # trim the NAs created by the process
  class(output) <- 'sieve.output'
  return(output)
}

cross.count <- function(n, sieve=sieve.with.marker(n)){
  # purpose : returns the number of crosses the number n has in eratosthenes'
  #           sieve
  # input   : n, a positive integer
  # output  : a null or positive integer
  # standard input checks:
  
  n <- as.integer(n)
  
  if (n<0 | n%%1!=0) stop('cross.count expects positive integer input')
  
  if (class(sieve)!='sieve.output'){
    stop('sieve must be generated using the sieve.with.marker function')
  }
  
  if (length(sieve)+1<n) stop('sieve maximum value must be increased')
  
  return(sieve[n-1])
}

sieve20000 <-  sieve.with.marker(20000)

x <- seq(2,1500)
y <- sapply(x, cross.count)
plot(x,y,pch='.')
x[y==max(y)]

n <- 500000
x1 <- seq(2,n)
y1 <- sieve.with.marker(n)
plot(x1,y1,pch='.')
