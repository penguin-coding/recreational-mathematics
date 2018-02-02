MakeDeck <- function(card.number=52){
  # input   : integer number of cards which the deck should contain
  # output  : an ordered vector of integers from 1:card.number, with class
  #           deck.object
  # purpose : produces a deck object 
  
  if (card.number < 1 | card.number%%1!=0){
    stop('integer number of cards expected')
  }
  
  deck <- 1:card.number
  class(deck) <- 'deck.object'
  return(deck)
}

ShuffleDeck <- function(deck.object){
  # input   : A deck object as produced by MakeDeck
  # output  : A deck object with shuffled cards
  # purpose : Shuffle the cards of a deck
  
  if (class(deck.object)!='deck.object') stop('expected a deck.object')
  
  n <- length(deck.object)
  
  shuffled <- sample(deck.object, n, replace=F)
  class(shuffled) <- 'deck.object'
  
  return(shuffled)
}

SplitDeck <- function(deck.object, players=4){
  # input   : - deck.object
  #           - integer number of players
  # output  : list object containing players number of decks
  # purpose : splits the deck.object into players number of decks each of 
  #           equal card number
  
  n <- length(deck.object)
  
  if (class(deck.object)!='deck.object') stop('expected a deck.object')
  if (n%%players!=0 | players<1) stop('invalid player number')
  
  output <- as.list(rep(NA,players))
  
  cards.per.deck <- n%%players
  
  for (card in 1:n){
    
    destination <- card%%players + 1 # which player should the card go to?
    
    # append the card to the end of 
    output[[destination]][[length(output[[destination]] + 1)]] <- 
      deck.object[card]
  }
  
  return(output)
}