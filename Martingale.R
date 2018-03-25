
roll.ball <- function(zeros=1){
  if(rbinom(1,1,1/(36 + zeros))) return(0)
  else{
   if(rbinom(1,1,0.5)) return('r')
   return('b')
  }
}

MartingaleGame <- function(wins,min,max,budget,zeros=1){
  # purpose : performs a game of roulette betting on red using the martingale
  #           system for a maximum of 'rolls' number of spins of the roulette
  #           wheel.
  # inputs  : wins   - the integer number of maximum wins of the roulette wheel
  #                    to consider
  #           min    - the table minimum
  #           max    - the table maximum
  #           budget - the player's budget to play the game.
  
  if (wins<1 | wins%%1!=0) stop('invalid number of rolls')
  if (min<=0) stop('invalid minimum')
  if (max <= min) stop('invalid maximum')
  if (budget <= min) stop('invalid budget')
  
  fail.count <- 0
  victories <- 0
  starting.budget <- budget
  
  while(victories<wins | budget<starting.budget){
    # step 1: Calculate the bet price and pay:
    bet.price <- min^(fail.count+1)
    if(bet.price>max) return(budget)
    budget <- budget-bet.price
    if (budget<=0) return(budget)
    
    # step 2: Do a roulette roll and calculate winnings
    roll.result <- roll.ball(zeros)
    
    if (roll.result=='r'){
      budget <- budget + 2*bet.price
      victories <- victories + 1
      fail.count <- 0
    }
    
    else fail.count <- fail.count + 1
  }
  
  return(budget)

}