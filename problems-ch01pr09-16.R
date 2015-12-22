################################################################################
## EXPLORATIONS IN MONTE CARLO METHODS | CHAPTER 01, PROBLEMS 09 - 16         ##
## ========================================================================== ##
## Stephen Peterson, 08 December 2015                                         ##
################################################################################

set.seed(20151208)
source("~/Documents/me/projects/r/explorations-in-monte-carlo-methods/extras-ch01.R")

ch01pr09 <- function(){
  # 09. Modified gambler's ruin with time limits
}

ch01pr10 <- function(){
  true.chance <- 0.422569
  # Estimates by simulation the chance of being dealt a "pair" in poker
  num.experiments <- 150
  num.trials      <- 100
  raw.results <- matrix(nrow = num.experiments, ncol = num.trials)
  for(i in 1:num.experiments){
    for (j in 1:num.trials){
      raw.results[i, j] <- poker.hand(target = "pair") 
    }
  }
  results <- apply(raw.results, 1, mean)
  message("Chance of being dealt a 'pair' in poker:")
  print(summary(results)[4])
  
  plot.new()
  hist(results,
       main = "Chance of 'pair' poker hand",
       sub  = "RED = Mean estimate, GREEN = True chance, BLUE = 1-sd interval",
       xlab = "Estimated probability")
  abline(v = mean(results), 
         col = "red",
         lwd = 5)
  abline(v = true.chance, 
         col = "green",
         lwd = 2.5)
  abline(v   = mean(results) + c(-1, 1) * sd(results),
         col = "blue",
         lty = "dashed")
  arrows(x0   = mean(results) - sd(results), y0 = 0,
         x1   = mean(results) + sd(results), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  dev.print(device = png,
            filename = "plot-ch01pr10.png",
            height = 1*480,
            width  = 1*480)
  close.screen(all.screens = TRUE)
}

ch01pr11 <- function(){
  true.chance <- 0.047539
  # Estimates by simulation the chance of being dealt "two pair" in poker
  num.experiments <- 150
  num.trials      <- 100
  raw.results <- matrix(nrow = num.experiments, ncol = num.trials)
  for(i in 1:num.experiments){
    for(j in 1:num.trials){
      raw.results[i, j] <- poker.hand(target = "two.pair")
    }
  }
  results <- apply(raw.results, 1, mean)
  message("Chance of being dealt 'two pair' in poker:")
  print(summary(results)[4])
  
  # Plotting
  plot.new()
  hist(results,
       main = "Chance of 'two pair' poker hand",
       sub  = "RED = Mean estimate, GREEN = True chance, BLUE = 1-sd interval",
       xlab = "Estimated probability")
  abline(v = mean(results), 
         col = "red",
         lwd = 5)
  abline(v = true.chance, 
         col = "green",
         lwd = 2.5)
  abline(v   = mean(results) + c(-1, 1) * sd(results),
         col = "blue",
         lty = "dashed")
  arrows(x0   = mean(results) - sd(results), y0 = 0,
         x1   = mean(results) + sd(results), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  dev.print(device = png,
            filename = "plot-ch01pr11.png",
            height = 1*480,
            width  = 1*480)
  close.screen(all.screens = TRUE)
}

ch01pr12 <- function(){
  # Simulates the game of Craps
  num.experiments <- 1000
  results <- numeric(num.experiments)
  for (i in 1:num.experiments) {
    results[i] <- craps.game()
  }
  to.plot <- tapply(results, results, length) / num.experiments
  
  message("Chance of outcomes in Craps:")
  print(to.plot)
  
  # Plotting
  plot.new()
  barplot(to.plot)
  dev.print(device = png,
            filename = "plot-ch01pr12.png",
            height = 1*480,
            width  = 1*480)
  close.screen(all.screens = TRUE)
}

ch01pr13 <- function(){
  # Simulates Bayesian probability problem
  pop.size <- 100000
  hiv.rate <- 0.03
  test.acc <- 0.98
  true.chance <- (test.acc * hiv.rate) / (test.acc * hiv.rate + (1 - test.acc) * (1 - hiv.rate))
  population <- sample(c(TRUE, FALSE), 
                       size = pop.size,
                       prob = c(hiv.rate, 1 - hiv.rate),
                       replace = TRUE)
  num.experiments <- 30
  num.trials <- 100
  num.patients <- num.experiments * num.trials
  patients.vec <- sample(population, 
                         size = num.patients,
                         replace = FALSE)
  results.vec <- rep(FALSE, num.patients)
  
  for (i in 1:num.patients) {
    results.vec[i] <- hiv.test(test.acc, patients.vec[i])
  }
  results.mat <- matrix(results.vec, 
                        nrow = num.experiments, 
                        ncol = num.trials)
  patients.mat <- matrix(patients.vec, 
                         nrow = num.experiments, 
                         ncol = num.trials)
  compare.mat <- (results.mat * patients.mat)
  print(sum(rowSums(compare.mat)))
  print(sum(rowSums(results.mat)))
  results <- apply(compare.mat, 1, sum) / apply(results.mat, 1, sum)

  message("Expected chance of HIV given positive test result:")
  print(mean(results))
  
  # Plotting
  plot.new()
  hist(results,
       breaks = 0.1 * 0:10,
       main   = "Expected chance of HIV given positive test result",
       xlab   = "Estimated chance")
  abline(v = mean(results),
         col = "red",
         lwd = 5)
  abline(v = true.chance, 
         col = "green",
         lwd = 2.5)
  abline(v   = mean(results) + c(-1, 1) * sd(results),
         col = "blue",
         lty = "dashed")
  arrows(x0   = mean(results) - sd(results), y0 = 0,
         x1   = mean(results) + sd(results), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  dev.print(device = png,
            filename = "plot-ch01pr14.png",
            height = 1*480,
            width  = 1*480)
  close.screen(all.screens = TRUE)
}

ch01pr14 <- function(){
  require(microbenchmark)
  
  # A. Find the time per random number generation. For example, how long does it
  #    take to generate 1,000,000 random numbers? (Representative times are
  #    between 0.2 and 10 microseconds per number)
  rng.time <- microbenchmark(runif(10^6))
  print(summary(rng.time$time / 10^6))
}

# ch01pr15 <- function(){} Skip; RNG

# ch01pr16 <- function(){} Skip; RNG