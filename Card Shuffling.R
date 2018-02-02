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
  
  output <- replicate(players, vector())
  
  cards.per.deck <- n%%players
  
  for (card in 1:n){
    
    destination <- card%%players + 1 # which player should the card go to?
    
    l <- length(output[[destination]])

    # append the card to the end of the correct player's stack
    output[[destination]][l + 1] <-  deck.object[card]
  }
  
  return(output)
}

TwoDeckGame <- function(Deck1, Deck2){
  # input   : Two decks of the class deck.object
  # output  : A named vecotor. The name represents the winning deck name,
  #           the elements are the card values for both candidate decks
  #           combined
  # purpose : Play a game of 'battaille' between the two submitted decks
  # note    : This deck handles draws by cycling through the cards, meaning
  #           it is theoretically possible for decks to get stuck in loops
  #           if they contain cards of the same value
  
  if (class(Deck1)!='deck.object' | class(Deck2)!='deck.object'){
    stop('Expected deck.object inputs')}
    
  Deck1.overflow <- vector()
  Deck2.overflow <- vector()
  
  while(length(Deck1) > 0 & length(Deck2)>0){
    
    # first check decks have cards to play, if not, shuffle their overflow 
    # and refill the deck with it
    
    if (length(Deck1)==0){
      Deck1 <- ShuffleDeck(Deck1.overflow)
      Deck1.overflow <- vector()
    }
    
    if (length(Deck2)==0){
      Deck2 <- ShuffleDeck(Deck2.overflow)
      Deck2.overflow <- vector()
    }
    
    # Compare the first element of each deck:
    Card1 <- Deck1[1] ; Card2 <- Deck2[1]
    Deck1 <- Deck1[2:length(Deck1)]
    Deck2 <- Deck2[2:length(Deck2)]
    
    if (Card1==Card2){
      # cycle them in case of a draw
      Deck1.overflow[length(Deck1.overflow)+1] <- Card1
      Deck2.overflow[length(Deck2.overflow)+1] <- Card2
    }
    
    else if (Card1>Card2){
      # send them to overflow 1 if Card1 wins
      Deck1.overflow[length(Deck1.overflow)+1] <- Card1
      Deck1.overflow[length(Deck1.overflow)+1] <- Card2
    }
    
    else{
      # send them to overflow 2 if Card2 wins
      Deck2.overflow[length(Deck2.overflow)+1] <- Card1
      Deck2.overflow[length(Deck2.overflow)+1] <- Card2
    }
  }
  
  return(list(Deck1.overflow, Deck2.overflow))
}

d <- MakeDeck(52)
d <- ShuffleDeck(d)
s <- SplitDeck(d,4)
