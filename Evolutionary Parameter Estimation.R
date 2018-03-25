assign.guess.value <- function(guess,
                             answer,
                             penalty='linear'){
  # purpose : Calculates the fitness of an individual using its current values, 
  #           the ideal values, and a given method of calculating loss. 
  # input   : guess   - The values of the individual of interest
  #           answer  - The ideal values that the previous values should be 
  #                     compared to 
  #           penalty - The method which is used to calculate fitness (or 'loss'
  #                     compared to the true values for the individual).
  #                     Available options are 'linear', which represents
  #                     absolute loss, and 'quadratic', which represents 
  #                     quadratic loss. 
  # output  : A real number, giving the fitness/loss of the individual. 
    
  if (length(guess)!=length(answer)) stop('mismatch input dimensions')
    
  diff <- abs(guess - answer)
    
  if (penalty=='linear') return(sum(diff))
  else if (penalty=='quadratic') return(sum(diff^2))
  else stop('invalid penalty choice')
}
  
mutate <- function(vector,
                   degree,
                   probability=0.1){
  # purpose : Modifies a vector according to 'degree' and 'probability'
  # input   : vector      - The vector of values which we wish to mutate
  #           degree      - A percentage, the maximum change which we are
  #                         allowed to perform on any entry of the vector.
  #           probability - The probability any entry in the vector has of being 
  #                         mutated
  # output  : A vector of same length as the input 'vector'. With values
  #           modified according to the above rules. 
    
  n <- length(vector)
    
  if(class(vector)!='numeric' | any(is.na(vector))) stop('invalid input vector')
  if(degree<=0 | degree>1) stop('invalid degree of mutation')
  if(probability<=0 | probability>1) stop('invalid probability of mutation')
    
  for (i in 1:n){
    if (rbinom(1,1,probability)){
      m <- vector[i]*degree
      mutation <- runif(1,0,m)
      sign.coeff <- rbinom(1,1,0.5)
      # sign coeff is either 0 or 1. If it is 0, the first half of the below
      # line is 0, and we are left with -mutation (a decrease in current value),
      # if sign.coeff is 1, the second half becomes zero and we are left with 
      # mutation (an increase in current value).
      mutation <- sign.coeff*mutation + (1-sign.coeff)*mutation*(-1)
        
      vector[i] <- vector[i] + mutation
    }
  }
    
  return(vector)
}
  
create.initial.population <- function(initial.vector,
                                      size,
                                      mutation.count,
                                      mutation.args=NULL){
  # purpose : Create an initial population from a base organism, with mutations
  #           to create 'genetic' diversity
  # input   : initial.vector - The vector containing the start values
  #           size           - The size of the population to be created
  #           mutation.count - The number of times we should mutate the created
  #                            population in order to acheive sufficient 
  #                            'genetic' diversity
  #           mutation.args  - The list of arguments to be passed to the
  #                            'mutate' function, which produces the mutations
  #                             we require. May have options, 'degree' and 
  #                             'probability' which must both be in (0,1].
  # output  : A list of length 'size', each entry is an individual in the 
  #           generated starting population, mutated according to the specified
  #           parameters. 
  
  if (class(initial.vector)!='numeric' | any(is.na(initial.vector))){
    stop('invalid initial vector')
  }
    
  if (size%%1!=0 | size<=1) stop('invalid popualtion size')
  if (mutation.count%%1!=0 | mutation.count<=1) stop('invalid mutation count')
    
  # a call to do.call will sanity check mutation.args for us
    
  output <- list()
    
  for (i in 1:size){
    output[[i]] <- initial.vector
    for (j in 1:mutation.count){
      mutation.args.ij <- mutation.args
      mutation.args.ij$vector <- output[[i]]
      output[[i]] <- do.call(mutate,mutation.args.ij)
    }
  }
    
  class(output) <- 'population'
  return(output)
}
  
kill.weaklings <- function(population,
                           cull.percentage,
                           true.values,
                           penalty){
  # purpose : Kills the genetically unfit members of a population for a single 
  #           generation
  # inputs  : population      - The population list which contains all of the 
  #                             individuals.
  #           cull.percentage - The percentage of the population that dies
  #                             through being unfit.
  #           true.values     - The vector of optimal values, which allows us to
  #                             calculate a measure of fitness for each 
  #                             individual. 
  #           penalty         - The way to calculate the penalty used in fitness
  #                             evaluation. Can be 'quadratic' or 'linear'
  # output  : The same population object with the correct number of weakest 
  #           individuals removed
  
  if (class(population)!='population') stop('invalid population')
  if (cull.percentage<=0 | cull.percentage>1) stop('invalid cull percentage')
  # true.values will get checked by other functions as we pass it as an argument
  
  n <- length(population)
  fitness.vector <- rep(NA, n)
  
  # Calculate  fitness for all individuals:
  for (i in 1:n){
    fitness.vector[i] <- assign.guess.value(population[[i]],
                                            true.values, 
                                            penalty=penalty)
  }
  
  # Decide who to kill based on fitness:
  fitness.data <- data.frame(index=1:n, fitness <- fitness.vector)
  order.fitness.data <- fitness.data[order(fitness.data$fitness),]
  number.to.keep <- n - floor(n*cull.percentage)
  get.to.live.indices <- order.fitness.data$index[1:number.to.keep]
  
  # Kill the unfit individuals:
  output <- list()
  for (i in 1:number.to.keep){
    chosen.index <- get.to.live.indices[i]
    output[[i]] <- population[[chosen.index]]
  }
  
  class(output) <- 'population'
  return(output)
}

reproduce <- function(population,
                      desired.size,
                      method,
                      mutation.pars=NULL,
                      M=T){
  # purpose : Simulates reproduction within a population to acheive a desired
  #           population size
  # inputs  : population    - The object containing the currect population. Must
  #                           have class 'population'
  #           desired.size  - An positive integer representing the desired final
  #                           population size once reproduction has taken place
  #           method        - Either 'sexual' or 'asexual'. If 'sexual',
  #                           offspring are produced by averaging two randomly
  #                           selected parents from the population. If 'asexual'
  #                           then the children are simply produced by cloning 
  #                           randomly selected parents from the initial
  #                           population
  #           mutation.pars - A list containing the 'degree' and 'probability'
  #                           of the mutations to take place
  #           M             - Boolean, if FALSE, children are not mutated at all
  # output  : A list, containing the new population.
  
  if (class(population)!='population') stop('invalid input population')
  n <- length(population)
  if (desired.size%%1!=0 | desired.size<=n) stop('invalid desired size')
  # mutation.pars get checked by the mutate function
  if (class(M)!='logical') stop('invalid M')
  if ((!method %in% c('sexual','asexual')) & M==T) stop('invalid method')
  
  new.population <- list()
  N <- desired.size - n    # The number of new individuals to produce
  
  # STEP 1: Produce offspring sexually or asexually
  if (method=='sexual'){
    for (i in 1:N){
      parent.index <- sample(1:n, 2, replace=F)
      child <- (population[[parent.index[1]]] + population[[parent.index[2]]])/2
      new.population[[i]] <- child
    }
  }
  
  else{
    for (i in 1:N){
      parent.index <- sample(1:n, 1, replace=F)
      new.population[[i]] <- population[[parent.index]]
    }
  }
  
  # Step 2: If appropriate, mutate children to ensure genetic variety
  if (M){
    for (i in 1:N){
      mutation.args.i <- mutation.pars
      mutation.args.i$vector <- new.population[[i]]
      new.population[[i]] <- do.call(mutate,mutation.args.i)
    }
  }
  
  # Step 3: incorporate children into existing population
  output <- c(population,new.population)
  class(output) <- 'population'
  return(output)
}
  
    Evolve <- function(population,
                   ideal.params,
                   generations=50,
                   cull.percentage=0.1,
                   mutation.pars=list(degree=0.2, probability=0.2),
                   penalty='linear',
                   method='sexual',
                   M=T){
  # purpose : Goes through the process of producing deaths and births in a 
  #           population according to the rules set out by the given parameters
  # inputs  : population      - The starting population object
  #           ideal.params    - The vector of values which represents the
  #                             ideal parameters for perfect fitness
  #           generations     - The integer number of death/birth cycles to 
  #                             go through
  #           cull.percentage - A number in (0,1) which describes the percentage
  #                             of the population which should be killed if they
  #                             are the weakest. i.e for 0.2, the weakest fifth
  #                             of the population is killed at each cycle. An 
  #                             analogous concept is survival to reproduction,
  #                             the (1 - cull.percentage) fittest portion of the
  #                             population will survive (and potentially
  #                             reproduce).
  #           mutation.pars   - The 'degree' and 'probability' which describe 
  #                             the behaviour of mutations. See the 'mutate'
  #                             function for more details. 
  #           penalty         - The way to calculate penalty for fitness, 
  #                             in terms of deviation from the ideal values. 
  #                             Either 'linear' or 'quadratic'
  #           method          - The method by which children are produced, can 
  #                             be either 'sexual' or 'asexual'. See reproduce
  #                             function for more detail. 
  #           M               - Should children be subjected to potential 
  #                             Mutation before they are able to breed? If this
  #                             set to FALSE, there is no other opportunity for
  #                             genetic mutation within the population
  # output  : The new population after 'generations' number of births and deaths
  
  if (class(population)!='population') stop('invalid population')
  n <- length(population)
  if (generations%%1!=0 | generations<1) stop('invalid generation count')
  # all other parameters get checked when they pass through lower level funcs
  
  for (i in 1:generations){
    # remove weak individuals:
    population <- kill.weaklings(population,
                                 cull.percentage,
                                 ideal.params,
                                 penalty)
    
    # breed from the remaining population:
    population <- reproduce(population,
                            n,
                            method,
                            mutation.pars,
                            M)
  }
  
  class(population) <- 'population'
  return(population)
}
  
summary.population <- function(population,
                               penalty,
                               ideal.params){
  # purpose : produces a list containing the best, worst and median fitness
  #           of the current population
  # input   : population   - the population of interest
  #           penalty      - 'linear' or 'quadratic', the method by which loss
  #                          of fitness compared to the ideal individual is 
  #                          calculated
  #           ideal.params - The parameters at which there is no loss of fitness
  # output  : A named list, containing, 'best', 'median' and 'worst'. Each of
  #           these is a named vector, containing 'value' and 'index', where 
  #           value is the fitness associated to the individual, and index gives
  #           the index of the associated individual in the population.
  
  n <- length(population)
  fitness.vector <- rep(NA, n)
  
  # Calculate  fitness for all individuals:
  for (i in 1:n){
    fitness.vector[i] <- assign.guess.value(population[[i]],
                                            ideal.params, 
                                            penalty)
  }
  
  fitness.data <- data.frame(index=1:n, fitness <- fitness.vector)
  order.fitness.data <- fitness.data[order(fitness.data$fitness),]
  
  best <- order.fitness.data[1,]
  median <- order.fitness.data[floor(n/2),] # approx assuming large pop
  worst <- order.fitness.data[n,]
  NAMES <- c('index', 'fitness')
  names(best) <- NAMES
  names(median) <- NAMES
  names(worst) <- NAMES
  return(list(worst=worst, median=median, best=best))
}
  
  
### END OF DRIVER CODE

# Proof of concept:
set.seed(0)

ideal.params <- runif(10, 0, 100)
mutation.pars <- list(degree=0.2,probability=0.2)

population <- create.initial.population(rep(c(1,2),5), 100, 50, mutation.pars)

Evolved <-  Evolve(population, ideal.params, generations = 20000)

summary(Evolved, ideal.params = ideal.params, penalty = 'linear')

Evolved[[1]]
ideal.params