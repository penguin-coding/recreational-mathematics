FizzBuzz = function(input, multiples=c(3,5), rules=c('Fizz','Buzz')){
  # purpose : For an input number 'input', returns the correct answer when 
  #           playing the game 'Fizz Buzz'
  #
  # inputs  : input     - An integer for which we want to play the game
  #           multiples - The numbers on which we have a given rule. For the
  #                       standard Fizz Buzz game, this is c(3,5)
  #           rules     - The replacement to include when 'input' is divisible
  #                       by an entry of multiples. The indices should match, 
  #                       for example, for 'Fizz Buzz' rules = c('Fizz','Buzz')
  #
  # output  : Either a number, or character containing the required replacements 
  
  
  # Input checks:
  if (input%%1!=0 | input<1) stop('input must be a positive integer')
  
  if (any(multiples%%1!=0) | any(multiples<1)){
    stop('all multiples must be positive integers')
  }
  
  if (length(rules)!=length(multiples)){
    stop('multiples and rules lengths must match')
  }
  
  if (any(sapply(rules, class)!='character')) stop('all rules must be strings')
  
  if (!identical(sort(multiples),multiples)){
    stop('multiples must be in increasing order')
  }
  # End of input checks
  
  
  if (all(input<multiples)) return(input) # A trivial case
  
  output <- ''
  
  for (i in (1:length(multiples))){
    
    if (input%%multiples[i]==0){ # if a multiple divides the input
      
      output <- paste(output,rules[i],sep='') # change the output accordingly
    }
    
  }
  
  ifelse(output == '', return(input), return(output))
}

FizzBuzzChain <- function(end=100, multiples=c(3,5), rules=c('Fizz','Buzz'),
                          start=1, step=1){
  # purpose : plays FizzBuzz with the chosen multiples and replacement rules
  #           from integer 'start' to integer 'end'.
  #
  # inputs  : end       - The integer at which we stop playing the game
  #           multiples - The divisors which lead to replacement
  #           rules     - The charatcers which replace each multiple, in order
  #           start     - The integer at which we start playing the game
  #           step      - The increment at which we play the game
  
  x <- seq(start,end,step)
  return(sapply(x, FizzBuzz, multiples=multiples, rules=rules))
}