################################################################################
## EXPLORATIONS IN MONTE CARLO METHODS | CHAPTER 01, PROBLEMS 01 - 08         ##
## ========================================================================== ##
## Stephen Peterson, 07 December 2015                                         ##
################################################################################

seed.ch01pr01_08 <- 20151207

source("/data/doc/me/projects/r/explorations-in-monte-carlo-methods/extras-ch01.R")

ch01pr01 <- function(){
  set.seed(seed.ch01pr01_08)
  # ----------------------------------------------------------------------------
  # 01. Performs a large number of trials of Buffon's needle problem to estimate
  #    the value of pi. Plots the error as a function of the number of trials.
  # ----------------------------------------------------------------------------
  
  # Simulation
  est.pi <- numeric(6)
  num.trials <- 10^(1:6)
  names(est.pi) <- num.trials
  for (i in 1:6){
   est.pi[i] <- buffon(num.trials[i])
  }
  message("Estimated value of pi after trials:")
  print(est.pi)
  
  # Plotting
  plot.new()
  plot(num.trials, abs(est.pi - pi), type = "o")
  dev.print(device = png, 
            filename = "plot-ch01pr01.png", 
            width = 480, 
            height = 480)
  close.screen(all.screens = TRUE)
}

ch01pr02 <- function(){
  set.seed(seed.ch01pr01_08)
  # ----------------------------------------------------------------------------
  # 02. Performing 2,000,000 trials of Buffon's needle problem for 1/pi.
  # ----------------------------------------------------------------------------
  
  num.experiments <- c(10^4, 10^3, 10^2)
  num.trials      <- c(2*10^2, 2*10^3, 2*10^4)
  
  # Target value
  message("True value of 1 / pi:")
  print(1 / pi)
  
  # Partition A: 10,000 "super experiments" of 200 trials each
  message("Partition A: 10,000 'super experiments' of 200 trials each")
  partition.a <- numeric(num.experiments[1])
  for (i in 1:num.experiments[1]){
    partition.a[i] <- buffon(num.trials[1])
  }
  print(summary(1 / partition.a))
  
  # Partition B: 1,000 "super experiments" of 2000 trials each
  message("Partition B: 1,000 'super experiments' of 2000 trials each")
  partition.b <- numeric(num.experiments[2])
  for (j in 1:num.experiments[2]){
    partition.b[j] <- buffon(num.trials[2])
  }
  print(summary(1 / partition.b))
  
  # Partition C: 100 "super experiments" of 20,000 trials each
  message("Partition C: 100 'super experiments' of 20,000 trials each:")
  partition.c <- numeric(num.experiments[3])
  for (k in 1:num.experiments[3]){
    partition.c[k] <- buffon(num.trials[3])
  }
  print(summary(1 / partition.c))
  
  # Plotting
  plot.new()
  split.screen(c(1,3))
  screen(1)
  to.plot <- 1 / partition.a
  hist(to.plot,
       main = "10,000 experiments; 200 trials each",
       xlab = "Estimated value of 1 / pi")
  abline(v   = mean(to.plot),
         col = "red",
         lwd = 5)
  abline(v   = mean(to.plot) + c(-1, 1) * sd(to.plot),
         col = "blue",
         lwd = 3,
         lty = "dashed")
  arrows(x0   = mean(to.plot) - sd(to.plot), y0 = 0,
         x1   = mean(to.plot) + sd(to.plot), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  abline(v   = 1 / pi,
         col = "green",
         lwd = 5)
  
  screen(2)
  to.plot <- 1 / partition.b
  hist(to.plot,
       main = "1,000 experiments; 2,000 trials each",
       xlab = "Estimated value of 1 / pi")
  abline(v   = mean(to.plot),
         col = "red",
         lwd = 5)
  abline(v   = mean(to.plot) + c(-1, 1) * sd(to.plot),
         col = "blue",
         lwd = 3,
         lty = "dashed")
  arrows(x0   = mean(to.plot) - sd(to.plot), y0 = 0,
         x1   = mean(to.plot) + sd(to.plot), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  abline(v   = 1 / pi,
         col = "green",
         lwd = 5)
  
  screen(3)
  to.plot <- 1 / partition.c
  hist(to.plot,
       main = "100 experiments; 20,000 trials each",
       xlab = "Estimated value of 1 / pi")
  abline(v   = mean(to.plot),
         col = "red",
         lwd = 5)
  abline(v   = mean(to.plot) + c(-1, 1) * sd(to.plot),
         col = "blue",
         lwd = 3,
         lty = "dashed")
  arrows(x0   = mean(to.plot) - sd(to.plot), y0 = 0,
         x1   = mean(to.plot) + sd(to.plot), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  abline(v   = 1 / pi,
         col = "green",
         lwd = 5)
  
  dev.print(device = png,
            filename = "plot-ch01pr02.png",
            height = 1*480,
            width  = 3*480)
  close.screen(all.screens = TRUE)
}

ch01pr03 <- function(){
  set.seed(seed.ch01pr01_08)
  # ----------------------------------------------------------------------------
  # 03. Simulating 1000 rolls of a pair of dice, with histograms
  # ---------------------------------------------------------------------------- 

  split.screen(c(1,3))
  
  # Histogram A: The sum of the dice
  data.a <- roll.d6(2, 1000)
  diesum2 <- colSums(data.a)
  screen(1)
  hist(diesum2,
       main = "2d6, summed")
  
  # Histogram B: Each pair of numbers
  diepairs <- paste(data.a[1,], data.a[2,], sep = ",")
  diepairs.plot <- tapply(diepairs, diepairs, length)
  screen(2)
  barplot(diepairs.plot,
       main = "2d6, paired")
  
  # Histogram C: As part A, but for 6 dice and 4000 rolls
  data.c  <- roll.d6(6, 4000) 
  diesum6 <- colSums(data.c)
  screen(3)
  hist(diesum6,
       main = "6d6, summed")
  dev.print(device = png, 
            filename = "plot-ch01pr03.png", 
            width = 1440, 
            height = 480)
  close.screen(all.screens = TRUE)
}

ch01pr04 <- function(){
  set.seed(seed.ch01pr01_08)
  # ----------------------------------------------------------------------------
  # 04. Suppose a "reward" r(x) is given upon the roll of a pair of dice as
  #     follows: r(x) = 1/x, x = 2,3,...,12. Estimate the expectation of r by
  #     simulation. (True expectation = 0.1678)
  # ----------------------------------------------------------------------------
  
  # Our true expectation
  true.mu <- 0.1678
  
  # Set number of experiments, number of rolls per experiment
  num.experiments <- 1000
  num.rolls <- 1000
  
  ch01pr04.results <- numeric(num.experiments)
  for (i in 1:num.experiments){
    ch01pr04.results[i] <- mean(1 / colSums(roll.d6(2, num.rolls)))
  }
  print(summary(ch01pr04.results))
  
  # Plotting
  plot.new()
  hist(ch01pr04.results)
  abline(v = true.mu, col = "red")
  dev.print(device = png, 
            filename = "plot-ch01pr04.png", 
            width = 480, 
            height = 480)
  close.screen(all.screens = TRUE)
}

ch01pr05 <- function(){
  set.seed(seed.ch01pr01_08)
  # ----------------------------------------------------------------------------
  # 05. In boxes of cereal the manufacturer puts coupons bearing one of the
  #     letters of the word "milk." If all four letters are collected
  # ----------------------------------------------------------------------------

  num.experiments <- 1000
  
  # A. Assume the letters of "milk" have uniform distribution
  message("Seeking 'MILK', assuming uniform distribution:")
  word.a  <- "milk"
  results.a <- numeric(num.experiments)
  for (i in 1:num.experiments){
    results.a[i] <- coupon.collect(word.a)
  }
  print(summary(results.a))

  # B. Assume that "k" has 1/10 the probability of the others of occurring.
  message("Seeking 'MILK', assuming probabilities 10/31, 10/31, 10/31, 1/31:")
  word.b  <- "milk"
  probs.b <- c(10/31, 10/31, 10/31, 1/31)
  results.b <- numeric(num.experiments)
  for (j in 1:num.experiments){
    results.b[j] <- coupon.collect(word.b)
  }
  print(summary(results.b))
  
  # C. Assume uniform distribution, but for the word "BattleCreek"
  message("Seeking 'BATTLECREEK', assuming uniform distribution:")
  word.c <- "BattleCreek"
  results.c <- numeric(num.experiments)
  for (k in 1:num.experiments){
    results.c[k] <- coupon.collect(word.c)
  }
  print(summary(results.c))

  # Plotting
  plot.new()
  split.screen(c(1,3))
  
  screen(1)
  hist(results.a)
  abline(v = mean(results.a), col = "blue")

  screen(2)
  hist(results.b)
  abline(v = mean(results.b), col = "blue")
  
  screen(3)
  hist(results.c)
  abline(v = mean(results.c), col = "blue")
  
  dev.print(device = png, "plot-ch01pr05.png", width = 1440, height = 480)
  close.screen(all.screens = TRUE)
}

ch01pr06 <- function(){
  set.seed(seed.ch01pr01_08)
  # ----------------------------------------------------------------------------
  # 06. Generate a sequence of characters from an alphabet with each character 
  #     chosen independently and with equal probability. How long before each
  #     possible word of a given length can be found in the sequence?
  # ----------------------------------------------------------------------------
  
  num.experiments <- 100
  
  # A1. Alphabet size 4, window size 4
  results.a1 <- numeric(num.experiments)
  # A2. Alphabet size 4, window size 5
  results.a2 <- numeric(num.experiments)
  # B1. Alphabet size 5, window size 4
  results.b1 <- numeric(num.experiments)
  # B2. Alphabet size 5, window size 5
  results.b2 <- numeric(num.experiments)
  # C1. Alphabet size 6, window size 4
  results.c1 <- numeric(num.experiments)
  # C2. Alphabet size 6, window size 5
  results.c2 <- numeric(num.experiments)
  # D1. Alphabet size 7, window size 4
  results.d1 <- numeric(num.experiments)
  # D2. Alphabet size 7, window size 5
  results.d2 <- numeric(num.experiments)
  
  # Perform simulations
  for(i in 1:num.experiments){
    results.a1[i] <- word.search(alphabet.length = 4, window.length = 4)
    results.a2[i] <- word.search(alphabet.length = 4, window.length = 5)
    #results.b1[i] <- word.search(alphabet.length = 5, window.length = 4)
    #results.b2[i] <- word.search(alphabet.length = 5, window.length = 5)
    #results.c1[i] <- word.search(alphabet.length = 6, window.length = 4)
    #results.c2[i] <- word.search(alphabet.length = 6, window.length = 5)
    #results.d1[i] <- word.search(alphabet.length = 7, window.length = 4)
    #results.d2[i] <- word.search(alphabet.length = 7, window.length = 5)
  }

  plot.new()
#  split.screen(c(2,4))
  split.screen(c(1,2))
  screen(1)
  hist(results.a1)
  abline(v = mean(results.a1), col = "red")
  
  screen(2)
  hist(results.a2)
  abline(v = mean(results.a2), col = "red")
  
#   screen(3)
#   hist(results.b1)
#   abline(v = mean(results.b1), col = "red")
#   
#   screen(4)
#   hist(results.b2)
#   abline(v = mean(results.b2), col = "red")
#   
#   screen(5)
#   hist(results.c1)
#   abline(v = mean(results.c1), col = "red")
#   
#   screen(6)
#   hist(results.c2)
#   abline(v = mean(results.c2), col = "red")
#   
#   screen(7)
#   hist(results.d1)
#   abline(v = mean(results.d1), col = "red")
#   
#   screen(8)
#   hist(results.d2)
#   abline(v = mean(results.d2), col = "red")
#   
  dev.print(device = png, 
            filename = "plot-ch01pr06.png",
            height = 1*480,
            width  = 2*480)
#            height = 2*480,
#            width  = 4*480)
  close.screen(all.screens = TRUE)
}

ch01pr07 <- function(){
  set.seed(seed.ch01pr01_08)
  # ----------------------------------------------------------------------------
  # 07. Simulate 1000 coin tosses and make histograms!
  # ----------------------------------------------------------------------------
  coin.tosses <- sample(c("H", "T"), size = 1000, replace = TRUE)
  
  plot.new()
  split.screen(c(1,3))
  # A. Histogram
  coin.counts <- tapply(coin.tosses, coin.tosses, length)

  screen(1)
  barplot(coin.counts)
  
  # B. Histogram for nonoverlapping pairs (x_n, x_{n+1})
  coin.pairs <- character(1000 - 1)
  for(i in 1:length(coin.pairs)){
    coin.pairs[i] <- paste(coin.tosses[i:(i+1)], sep = "", collapse = "")
  }
  pair.counts <- tapply(coin.pairs, coin.pairs, length)
  
  screen(2)
  barplot(pair.counts)
  
  # C. Histogram for nonoverlapping triples (x_n, x_{n+1}, x_{n+2})
  coin.triples <- character(1000 - 2)
  for(j in 1:length(coin.triples)){
    coin.triples[j] <- paste(coin.tosses[j:(j+2)], sep = "", collapse = "")
  }
  triple.counts <- tapply(coin.triples, coin.triples, length)
  
  screen(3)
  barplot(triple.counts)
  
  dev.print(device = png,
            filename = "plot-ch01pr07.png",
            height = 1*480,
            width  = 2*480)
  close.screen(all.screens = TRUE)
  
  # D. Histogram for runs; k "heads" in a row
}

ch01pr08 <- function(){
  set.seed(seed.ch01pr01_08)
  # ----------------------------------------------------------------------------
  # 08. Simulate a random walk with drift
  # ----------------------------------------------------------------------------
  num.steps <- 30
  coin.probs <- c(0.6, 0.4)
  coin.steps <- c("H" = 1, "T" = -1)
  
  # Plot a sample walk
  plot.new()
  split.screen(c(1,2))
  screen(1)
  random.walk(num.steps = num.steps, 
              coin.steps = coin.steps, 
              coin.probs = coin.probs,
              to.plot = TRUE)
  
  # Perform simulation
  num.experiments <- 200
  results <- numeric(num.experiments)
  for(i in 1:num.experiments){
    results[i] <- random.walk(num.steps = num.steps, 
                              coin.steps = coin.steps, 
                              coin.probs = coin.probs,
                              to.plot = FALSE)
  }
  print(c("mean" = mean(results), "std.dev" = sd(results)))
  
  screen(2)
  hist(results)
  abline(v = mean(results), col = "red")
  abline(v = mean(results) + c(-1, 1) * sd(results), col = "blue")
  
  dev.print(device = png,
            filename = "plot-ch01pr08.png",
            height = 1*480,
            width  = 2*480)
}
