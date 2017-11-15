select <- function(i,output,est.cdf,dataset){
  # purpose : subfunction of make.selection. Adds to the given output vector,
  #           the selected inidivudal's number, its value, and its score.
  #
  # input   : i       - the selected individual's index in the population
  #           output  - the output vector to modify
  #           est.cdf - the estimated cdf of the population
  #           dataset - the dataset the selected individual came from
  #
  # output  : the output argument with all relevant modifications made
  output[2] <- i
  output[3] <- dataset[i]
  output[4] <- est.cdf(dataset[i])
  return(output)
}

make.selection <- function(dataset,method='no change',look.n=0.37,
                           ssc=0.75){
  # purpose : uses the 37% rule to attempt to select the best candidate from 
  #           a simulated population of 'applicants'.
  #
  # intput  : datset - a numeric vector of observations
  #           method - name of a method to use which modifies the most simple
  #                    outline of the algorithm. 
  #                    options are - second.saviour.
  #           look.n - the percentage of the data set we use as our looking set
  #           ssc    - parameter value which sets certain parameters for the 
  #                    method selected
  #
  # output  : a named vector containing:
  #           pop.max   - the TRUE best candidate in the population
  #           number    - the candidate selected was the 'number' candidate to 
  #                       be considered.
  #           selection - the value for the selected candidate
  #           score     - the probability of picking a candidate as good as the
  #                       selected candidate, or a better one, if the candidate
  #                       had been chosen at random from the population. This
  #                       calculation is made using the empirical CDF of the 
  #                       data.
  
  dataset <- na.omit(dataset)
  if (class(dataset)!='numeric') stop('input dataset must be numeric')
  
  if (look.n<=0 | look.n>=1) stop('look.n must be between 0 and 1')
  
  output <- vector(length=4)
  names(output) <- c('pop.max','number','selection','score')
  
  output[1] <- max(dataset)      # the (unknown) best candidate value
  est.cdf <- ecdf(dataset)       # get the ecdf from the data
  
  n <- length(dataset)
  
  ifelse(look.n<0.5,
         stop.looking <- ceiling(n*look.n),   # to avoid having a null looking
         stop.looking <- floor(n*look.n))     # set or a full one
  
  lookset <- dataset[1:stop.looking]
  
  observed.best <- max(lookset)
  second.best <- max(lookset[which(lookset!=observed.best)])
  
  for (i in stop.looking:n){
    
    if (dataset[i]>observed.best){               # if the best wasn't in the 
      output <- select(i,output,est.cdf,dataset) # the first 37%, we will find
      return(output)                             # them this way
    }
    
    else if (i>=floor(n*ssc) & method == 'second.saviour' # one method of 
             & dataset[i]>second.best){           # minimising loss when the
      output <- select(i,output,est.cdf,dataset)  # best was in the first 37%     
      return(output) # is to use the second best as our threshold for the last
      }              # 25% of the population we observe
  }
  
  output[2] <- n                   # if the best candidate was already in
  output[3] <- dataset[n]          # the ones we saw, we are forced to 
  output[4] <- est.cdf(dataset[i]) # select the last candidate
  return(output)                      
}

draw.ss.method.graph <- function(points,replicate,pop.size){
  # purpose : tries various values between 0 and 1 for the ssc paramter for the
  #           second.saviour method, and plots the score they produce. 
  #
  # inputs  : points    - integer number of points to use to generate plot
  #           replicate - the number of times to repeat the experiment on
  #                       simulated populations to obtain the estimated average
  #                       score for that value of ssc.
  #           pop.size  - the size of the candidate populations which we use 
  #                       for the simulations
  #
  # outputs : none
  if (points<1 | points%%1!=0 | replicate<1 | replicate%%1!=0 | 
      pop.size<1 | pop.size%%1!=0) stop('points must be a positive integer')
  
  x <-  seq(0,1,length=points)
  y <-  vector(length=points)
  
  for (i in 1:points){
    y[i] <- rowMeans(replicate(replicate,make.selection(rnorm(pop.size),
                                                   method='second.saviour',
                                                   ssc=x[i])))[4]
  }
  
  plot(x,y,type='l',main='Graph of method score given ssc',ylab='score',
       xlab='ssc')
}

draw.look.n.graph <- function(N=50, S=2000, pop.size=500, lower=0.01,
                              upper=0.99){
  # inputs :
  #          N        - The number of different look.n values to try
  #          S        - The number of simulations to run for each look.n
  #          pop.size - The size of the population we sample from
  x <-  seq(lower,upper,length=N)
  y <-  vector(length=N)
  
  for (i in 1:N){
    y[i] <- mean(replicate(S, make.selection(rnorm(pop.size),look.n=x[i])[4]))
  }
  
  plot(x,y,type='l',main='Graph of method score given look.n',ylab='score',
       xlab='look.n')
  
  return(x[which(y==max(y))])
}