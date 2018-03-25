Left.Right.Centre <- function(players=5,dice=3,starting.cash=3,trace=T){
  # purpose : plays a game of left right centre with visual representation for
  #           the user
  # inputs  : players       - The number of players in the game, with default 5.
  #           dice          - The (maximum) number of dice each player rolls on
  #                           their turn.
  #           starting.cash - The number of counters each player starts the game
  #           trace         - If true prints details of the game at each turn
  #                           with.
  # output  : graphs to display the state of the game
  
  if(players<1 | players%%1!=0) stop('invalid players input')
  if(dice   <1 | dice%%1!=0)    stop('invalid dice input')
  if(starting.cash<1 | starting.cash%%1!=0) stop('invalid cash input')
  if (dice>starting.cash) stop('too many dice')
  
  bank <- rep(starting.cash, players) # store financial status of all players
  
  while(sum(bank!=0)!=1){ # while more than 1 player still has money...
    
    if (trace){
      print(bank)
      cat('\nNEW ROUND \n')
    }
    
    for (i in (1:players)){ # each player takes their turn...
      print(i)
    }
  }
  print(bank)
}

RollDice <- function(quantity){
  # purpose : Rolls a user specified quantity of dice, obtaining the result,
  #           'left', 'right' or 'centre' for each die.
  # input   : quantity, the positive integer number of dice to roll
  # output  : a named list containing the number of 'left', 'right' and 'centre'
  #           rolls produced by the dice
 
  # produce the dice rolls:
  rolls <- runif(quantity)
  
  # separate rolls into left right and centre results:
  left <- sum(rolls<(1/3))
  right <- sum(rolls<(2/3)) - left
  centre <- quantity - left - right
  
  return(list(left=left,right=right,centre=centre))
}