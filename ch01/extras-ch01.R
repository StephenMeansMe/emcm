################################################################################
## EXPLORATIONS IN MONTE CARLO METHODS | GENERIC FUNCTIONS FOR CHAPTER 01     ##
## ========================================================================== ##
## Stephen Peterson, 07-10 December 2015                                      ##
################################################################################

# ------------------------------------------------------------------------------
# Buffon's needle experiment | Used in Problems 1, 2
# ------------------------------------------------------------------------------
buffon <- function(trials){
  x <- runif(trials, min = 0, max = 1)
  y <- runif(trials, min = 0, max = 1)
  hits <- sum(x^2 + y^2 < 1)
  pi.est <- 4 * hits / trials
}

# ------------------------------------------------------------------------------
# Rolling batches of dice | Used in Problems 3
# ------------------------------------------------------------------------------
roll.d6 <- function(dice, rolls){
  # Rolling m d6's, n times
  result <- matrix(nrow = dice, ncol = rolls)
  for (i in 1:dice){
    result[i, ] <- sample(1:6, rolls, replace = TRUE)
  }
  result
}

coupon.collect <- function(word, probs = NULL){
  word.split <- strsplit(word, split = "")[[1]]
  coupons <- unique(word.split)
  needed  <- tapply(word.split, word.split, length)
  if (is.null(probs)){
    probs <- rep(1 / length(coupons), length(coupons))
  }
  
  collected <- rep(0, length(coupons))
  num.boxes <- 0
  while (sum(collected >= needed) < length(needed)){
    num.boxes <- num.boxes + 1
    letter <- sample(coupons, size = 1, prob = probs)
    collected <- collected + (letter == coupons)
  }
  num.boxes
}

word.search <- function(alphabet.length = c(4, 5, 6, 7), 
                        window.length = c(4, 5)){
  # Slide a window along a random sequence of characters looking for words
  alphabet <- letters[1:alphabet.length]
  if(window.length == 4){
    combo.table <- expand.grid(alphabet, 
                               alphabet, 
                               alphabet, 
                               alphabet)
  }
  if(window.length == 5){
    combo.table <- expand.grid(alphabet, 
                               alphabet, 
                               alphabet, 
                               alphabet, 
                               alphabet)
  }
  
  # Generate all the possible words of length w from alphabet L
  words <- apply(combo.table, 1, paste, sep = "", collapse = "")
  
  # We just need 1 of each possible word
  needed <- rep(1, length(words))
  collected <- rep(0, length(words))
  
  # Generate the first window full of letters
  window.letters <- sample(alphabet, size = window.length, replace = TRUE)
  num.chars.gen <- window.length
  new.word <- paste(window.letters, sep = "", collapse = "")
  collected <- collected + (new.word == words)
  
  while(sum(collected >= needed) < length(needed)){
    # "Advance" the search window by 1 letter
    window.letters[1:(window.length - 1)] <- window.letters[2:window.length]
    # Generate new letter for the "leading" edge of the search window
    window.letters[window.length] <- sample(alphabet, size = 1)
    # Update number of characters generated
    num.chars.gen <- num.chars.gen + 1
    # Create new word
    new.word <- paste(window.letters, sep = "", collapse = "")
    # Update tally of collected words
    collected <- collected + (new.word == words)
  }
  num.chars.gen
}

random.walk <- function(num.steps, coin.probs, coin.steps, to.plot = FALSE){
  position <- numeric(num.steps + 1)
  steps <- 0:num.steps
  position[1] <- 0
  for(i in 2:(num.steps + 1)){
    coin <- sample(c("H", "T"), size = 1, prob = coin.probs)
    position[i] <- position[i - 1] + coin.steps[coin][[1]]
  }
  if(to.plot){
    plot(steps, position, 
         type = "o",
         main = "Sample Random Walk")
  }
  position[length(position)]
}

poker.hand  <- function(target = c("pair", "two.pair")){
  face.values <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", 
                   "J", "Q", "K", "A")
  deck <- rep(face.values, 4)  # We don't care about suit values
  hand <- sample(deck, size = 5, replace = FALSE)
  if(target == "pair"){
    check <- sum(diag(table(hand, hand)) == 2) == 1
  }
  if(target == "two.pair"){
    check <- sum(diag(table(hand, hand)) == 2) == 2
  }
  check
}

craps.game <- function(){
  point  <- 0
  result <- NA
  
  # the game
  # first roll
  roll <- sum(roll.d6(dice = 2, rolls = 1))
  if(roll == 7 | roll == 11){
    result <- "WIN"
    return(result)
  } else if(roll == 2 | roll == 3 | roll == 12) {
    result <- "LOSE"
    return(result)
  } else if(point == 0) {
    point <- roll
  }
  
  # Main game
  while(TRUE){
    roll <- sum(roll.d6(dice = 2, rolls = 1))
    if(roll == 7){
      result <- "LOSE"
      return(result)
    } else if (roll == point){
      result <- "WIN"
      return(result)
    }
  }
}

hiv.test <- function(accuracy, patient){
  test <- sample(c(patient, !patient), 
                 size = 1, 
                 prob = c(accuracy, 1 - accuracy))
  test
}

monty.hall <- function(num.trials = 1,
                       do.switch  = FALSE){
  # ----------------------------------------------------------------------------
  # Simulates one shot of the "classic" Monty Hall problem
  # ----------------------------------------------------------------------------
  # Randomly select a door for the prize and a door the contestant chose
  prize.door <- sample(3, size = num.trials, replace = TRUE)
  play.door  <- sample(3, size = num.trials, replace = TRUE)
  # Decide if the contestant switches (default: don't switch)
  if(do.switch) {
    win.pct <- sum(play.door != prize.door) / num.trials
  } else {
    win.pct <- sum(play.door == prize.door) / num.trials
  }
  return(win.pct)
}

monty.hall2 <- function(num.trials = 1,
                        q,
                        do.switch  = FALSE){
  doors <- 1:3
  # Randomly select a door for the prize; contestant always picks door no. 1
  prize.door <- sample(doors, size = num.trials, replace = TRUE)
  play.door  <- rep(1, num.trials)
  # Monty picks a door
  monty.door <- numeric(num.trials)
  for (i in 1:num.trials) {
    monty.door[i] <- if(prize.door[i] == play.door[i]) {
      sample(doors[-c(prize.door[i])],
             size = 1,
             prob = c(q, 1 - q))
    } else {
      monty.door[i] <- doors[-c(prize.door[i], play.door[i])]
    }
  }
  if(do.switch) {
    wins <- numeric(num.trials)
    for (i in 1:num.trials) {
      wins[i] <- doors[-c(play.door[i], monty.door[i])] == prize.door[i]
    }
    win.pct <- sum(wins) / num.trials
  } else {
    win.pct <- sum(play.door == prize.door) / num.trials
  }
  return(win.pct)
}

miniopoly <- function(max.turns = 20){
  turn <- 1
  card.squares  <- c(2, 7, 17, 22, 33, 36)
  card.outcomes <- c(-150, -100, 50, 100, 200)
  free.squares  <- c(0, 10, 20)
  passed.go     <- 200
  in.jail       <- FALSE
  jail.square   <- 10
  jail.outcome  <- -10
  square.price  <- -29
  dice.doubles  <- c("1 1", "2 2", "3 3", "4 4", "5 5", "6 6")
  
  player.money    <- rep(0, max.turns)
  prev.square     <- 0
  curr.square     <- 0
  player.money[1] <- passed.go  # Player starts with $200
  while(player.money[turn] > 0 & turn <= 20){
    turn <- turn + 1
    roll <- roll.d6(dice = 2, rolls = 1)
    roll.values <- paste(roll, collapse = " ")
    # Check if player is in jail
    if (in.jail) {
      # Get out jail on doubles
      if (roll.values %in% dice.doubles) {
        in.jail       <- FALSE
        curr.square <- jail.square + sum(roll) 
      } else {
        # Otherwise pay out and stay
        player.money[turn] <- player.money[turn - 1] + jail.outcome
      }
    } else {
      # We can move freely
      prev.square <- curr.square
      curr.square <- (curr.square + sum(roll)) %% 40
      # Parse landing on a square
      if (curr.square %in% free.squares) {
        player.money[turn] <- player.money[turn - 1]
      } else {
        if (curr.square %in% card.squares) {
          picked.card <- sample(card.outcomes, size = 1)
          player.money[turn] <- player.money[turn - 1] + picked.card
        } else {
          if (curr.square == 30) {
            in.jail <- TRUE
            player.money[turn] <- player.money[turn - 1]
          } else {
            player.money[turn] <- player.money[turn - 1] + square.price
          }
        }
      }
      # Check landing on or passing Go
      if (curr.square == 0 | curr.square < prev.square) {
        # We passed Go!
        player.money[turn] <- player.money[turn - 1] + passed.go
      }
      # Check for bankruptcy
      if (player.money[turn] <= 0) {
        player.money[turn] <- 0
      }
    }
  }
  return(player.money)
}

rand.prob.dist1 <- function(num.samples = 1, N = 4){
  sample.df <- as.data.frame(matrix(nrow = num.samples, ncol = N))
  colnames(sample.df) <- paste("p", 1:N, sep = "")
  for (i in 1:num.samples) {
    x <- runif(N)
    p <- numeric(N)
    p[1] <- 1 + x[1] - x[N]
    p[2:N] <- x[2:N] - x[1:(N - 1)]
    sample.df[i, ] <- p
  }
  return(sample.df)
}

rand.prob.dist2 <- function(num.samples = 1, N = 4){
  sample.df <- as.data.frame(matrix(nrow = num.samples, ncol = N))
  colnames(sample.df) <- paste("p", 1:N, sep = "")
  for (i in 1:num.samples) {
    x <- runif(N)
    x.sum <- sum(x)
    sample.df[i, ] <- x / x.sum
  }
  return(sample.df)
}