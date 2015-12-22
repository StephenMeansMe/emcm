################################################################################
## EXPLORATIONS IN MONTE CARLO METHODS | GENERIC FUNCTIONS FOR CHAPTER 02     ##
## ========================================================================== ##
## Stephen Peterson, 14-17 December 2015                                      ##
################################################################################

st.petersburg <- function(rounds){
  fortune    <- rep(-5, rounds)  # Initial bet for each game is $5.00
  
  for (i in 1:rounds) {
    payoff     <- 1
    while(TRUE) {
      # Begin the game
      flip <- sample(0:1, size = 1)  # Flip the coin
      if (flip == 0) {
        break()  # Game ends on a tails 
      } else {
        fortune[i] <- fortune[i] + payoff
        payoff  <- payoff * 2  # Payoff doubles each heads
      }
    }
  }
  return(fortune)
}

roll.until.6 <- function(num.trials){
  num.rounds <- rep(1, num.trials)
  for (i in 1:num.trials) {
    dice.face <- sample(1:6, size = 1)
    while(dice.face != 6) {
      num.rounds[i] = num.rounds[i] + 1
      dice.face <- sample(1:6, size = 1)
    }
  }
  return(num.rounds)
}