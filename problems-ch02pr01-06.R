################################################################################
## EXPLORATIONS IN MONTE CARLO METHODS | CHAPTER 02, PROBLEMS 01 - 06         ##
## ========================================================================== ##
## Stephen Peterson, 14 December 2015                                         ##
################################################################################

seed.ch02pr01_06 <- 20151214

source("/data/doc/me/projects/r/explorations-in-monte-carlo-methods/extras-ch02.R")

ch02pr01 <- function(){
  set.seed(seed.ch02pr01_06)
  # 01. St. Petersburg game
  data <- matrix(nrow = 3, ncol = 10000)
  num.rounds <- c(100, 1000, 10000)
  
  for (i in 1:3) {
    data[i, ] <- st.petersburg(num.rounds[i])
  }
  
  # Plotting
  plot.new()
  split.screen(c(1,3))
  for (i in 1:3) {
    screen(i)
    results <- data[i, ]
    hist(results,
         main = paste("Ending fortune for", num.rounds[i], "rounds", collapse = " "))
#     abline(v   = mean(results),
#            col = "red",
#            lwd = 5)
#     abline(v   = mean(results) + c(-1, 1) * sd(results),
#            col = "blue",
#            lty = "dashed",
#            lwd = 3)
  }
  dev.print(device   = png,
            filename = "plot-ch02pr01.png",
            height   = 1*480,
            width    = 3*480)
  close.screen(all.screens = TRUE)
}

ch02pr02 <- function(){
  set.seed(seed.ch02pr01_06)
  # 02. Paris Salon game
  num.experiments <- 1000
  num.trials <- 200
  
  raw.data <- matrix(nrow = num.experiments, ncol = num.trials)
  for (i in 1:num.experiments) {
    raw.data[i, ] <- roll.until.6(num.trials)
  }
  game.data    <- 1 * (raw.data > 4) - 1 * (raw.data <= 4)
  game.results <- apply(game.data, 1, mean)
  results <- apply(raw.data, 1, mean) 
  
  message("Expected number of rolls before throwing a 6:")
  print(mean(results))
  
  message("Expected winnings in the Paris Salon game:")
  print(mean(game.results))
  
  # Plotting
  plot.new()
  split.screen(c(1,2))
  screen(1)
  hist(results,
       main = "Avg. number of rolls until a 6",
       xlab = "Rolls")
  abline(v   = 6,
         col = "green",
         lwd = 2)
  abline(v   = mean(results),
         col = "red",
         lwd = 2)
  abline(v   = mean(results) + c(-1, 1) * sd(results),
         col = "blue",
         lty = "dashed",
         lwd = 1)
  
  screen(2)
  hist(game.results,
       main = "Avg. winnings in Paris Salon",
       xlab = "Money")
  abline(v   = mean(game.results),
         col = "red",
         lwd = 2)
  abline(v   = mean(game.results) + c(-1, 1) * sd(game.results),
         col = "blue",
         lty = "dashed",
         lwd = 1)
  
  dev.print(device   = png,
            filename = "plot-ch02pr02.png",
            height   = 1*480,
            width    = 2*480)
  close.screen(all.screens = TRUE)
}

ch02pr03 <- function(){}

ch02pr04 <- function(){}

ch02pr05 <- function(){}

ch02pr06 <- function(){}