################################################################################
## EXPLORATIONS IN MONTE CARLO METHODS | CHAPTER 01, PROBLEMS 17 - 24         ##
## ========================================================================== ##
## Stephen Peterson, 09 December 2015                                         ##
################################################################################

set.seed(20151209)

#ch01pr17 <- function(){}

#ch01pr18 <- function(){}

#ch01pr19 <- function(){}

#ch01pr20 <- function(){}

#ch01pr21 <- function(){}

#ch01pr22 <- function(){}

#ch01pr23 <- function(){}

ch01pr24 <- function(){
  # 24. Experimentally tests the correct strategy for the Monty Hall problem
  
  num.doors <- 3
  num.experiments <- 30
  num.trials <- 1000
  
  # A. Contestant doesn't switch
  true.chance.stay <- 1/3
  results.stay <- numeric(num.experiments)
  for (i in 1:num.experiments) {
    results.stay[i] <- monty.hall(num.trials = num.trials, do.switch = FALSE)
  }
  message("Expected chance of winning on a stay:")
  print(mean(results.stay))
  
  # B. Contestant does switch
  true.chance.switch <- 2/3
  results.switch <- numeric(num.experiments)
  for (i in 1:num.experiments) {
    results.switch[i] <- monty.hall(num.trials = num.trials, do.switch = TRUE)
  }
  message("Expected chance of winning on a switch:")
  print(mean(results.switch))
  
  # Plotting
  plot.new()
  split.screen(c(1,2))
  screen(1)
  hist(results.stay,
       main = "Winning Monty Hall by staying",
       sub  = "RED = Mean estimate, GREEN = True chance, BLUE = 1-sd interval",
       xlab = "Estimated probability")
  abline(v   = mean(results.stay), 
         col = "red",
         lwd = 5)
  abline(v   = true.chance.stay, 
         col = "green",
         lwd = 2.5)
  abline(v   = mean(results.stay) + c(-1, 1) * sd(results.stay),
         col = "blue",
         lty = "dashed")
  arrows(x0   = mean(results.stay) - sd(results.stay), y0 = 0,
         x1   = mean(results.stay) + sd(results.stay), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  
  screen(2)
  hist(results.switch,
       main = "Winning Monty Hall by switching",
       sub  = "RED = Mean estimate, GREEN = True chance, BLUE = 1-sd interval",
       xlab = "Estimated probability")
  abline(v   = mean(results.switch), 
         col = "red",
         lwd = 5)
  abline(v   = true.chance.switch, 
         col = "green",
         lwd = 2.5)
  abline(v   = mean(results.switch) + c(-1, 1) * sd(results.switch),
         col = "blue",
         lty = "dashed")
  arrows(x0   = mean(results.switch) - sd(results.switch), y0 = 0,
         x1   = mean(results.switch) + sd(results.switch), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  dev.print(device   = png,
            filename = "plot-ch01pr24.png",
            height   = 1*480,
            width    = 2*480)
  close.screen(all.screens = TRUE)
}
