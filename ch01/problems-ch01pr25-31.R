################################################################################
## EXPLORATIONS IN MONTE CARLO METHODS | CHAPTER 01, PROBLEMS 25 - 31         ##
## ========================================================================== ##
## Stephen Peterson, 10 December 2015                                         ##
################################################################################

set.seed(20151210)

ch01pr25 <- function(){
  # 25. Different version of Monty Hall
  monty.prob <- 0.5
  num.experiments <- 30
  num.trials <- 1000
  # A. Contestant doesn't switch
  true.chance.stay <- 1/3
  results.stay <- numeric(num.experiments)
  for (i in 1:num.experiments) {
    results.stay[i] <- monty.hall2(num.trials = num.trials, 
                                   q          = monty.prob,
                                   do.switch = FALSE)
  }
  message("Expected chance of winning on a stay:")
  print(mean(results.stay))
  
  # B. Contestant does switch
  true.chance.switch <- 2/3
  results.switch <- numeric(num.experiments)
  for (i in 1:num.experiments) {
    results.switch[i] <- monty.hall2(num.trials = num.trials, 
                                     q          = monty.prob,
                                     do.switch = TRUE)
  }
  message("Expected chance of winning on a switch:")
  print(mean(results.switch))
  
  # Plotting
  plot.new()
  split.screen(c(1,2))
  screen(1)
  hist(results.stay,
       main = "Winning Monty Hall II by staying",
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
       main = "Winning Monty Hall II by switching",
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
            filename = "plot-ch01pr25.png",
            height   = 1*480,
            width    = 2*480)
  close.screen(all.screens = TRUE)
}

ch01pr26 <- function(){
  # 26. Monty Hall II, but with a biased Monty
  monty.prob <- 0.25  # Monty chooses door no. 2 with this probability
  num.experiments <- 30
  num.trials <- 1000
  # A. Contestant doesn't switch
  true.chance.stay <- 1 - 1 / (2 - monty.prob)
  results.stay <- numeric(num.experiments)
  for (i in 1:num.experiments) {
    results.stay[i] <- monty.hall2(num.trials = num.trials, 
                                   q          = monty.prob,
                                   do.switch  = FALSE)
  }
  message("True chance of winning on a stay:")
  print(true.chance.stay)
  message("Estimated chance of winning on a stay:")
  print(mean(results.stay))
  
  # B. Contestant does switch
  true.chance.switch <- 1 / (2 - monty.prob)
  results.switch <- numeric(num.experiments)
  for (i in 1:num.experiments) {
    results.switch[i] <- monty.hall2(num.trials = num.trials, 
                                     q          = monty.prob,
                                     do.switch  = TRUE)
  }
  message("True chance of winning on a switch:")
  print(true.chance.switch)
  message("Estimated chance of winning on a switch:")
  print(mean(results.switch))
  
  # Plotting
  plot.new()
  split.screen(c(1,2))
  screen(1)
  hist(results.stay,
       main = "Winning Biased Monty II by staying",
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
       main = "Winning Biased Monty II by switching",
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
            filename = "plot-ch01pr26.png",
            height   = 1*480,
            width    = 2*480)
  close.screen(all.screens = TRUE)
}

#ch01pr27 <- function(){}

ch01pr28 <- function(){
  # Playing "Miniopoly"
  num.experiments <- 30
  num.trials <- 100
  raw.results <- matrix(nrow = num.experiments, ncol = num.trials)
  results <- numeric(num.experiments)
  for (i in 1:num.experiments) {
    for (j in 1:num.trials) {
      raw.results[i, j] <- miniopoly()[20]
    }
    results[i] <- mean(raw.results[i, ])
  }
  message("Average fortune after playing Miniopoly:")
  print(mean(results))
  
  plot.new()
  hist(results,
       main = "Average fortune after playing Miniopoly")
  abline(v   = mean(results),
         col = "red",
         lwd = 5)
  abline(v   = mean(results) + c(-1, 1) * sd(results),
         col = "blue", 
         lty = "dashed")
  arrows(x0   = mean(results) - sd(results), y0 = 0,
         x1   = mean(results) + sd(results), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  dev.print(device   = png,
            filename = "plot-ch01pr28.png",
            height   = 1*480,
            width    = 1*480)
  close.screen(all.screens = TRUE)
}

ch01pr29 <- function(){
  require(MASS)
  require(dplyr)
  num.samples = 500
  # Try when N = 4
  data4.df <<- tbl_df(rand.prob.dist1(num.samples = num.samples, N = 4))
  # Individual distributions
  # Plotting
  plot.new()
  split.screen(c(2,2))
  for (i in 1:4) {
    screen(i)
    hist(data4.df[, i][[1]],
         probability = TRUE,
         main = colnames(data4.df)[i],
         xlab = NULL,
         ylab = NULL)
    curve(dnorm(x, mean = mean(data4.df[, i][[1]]), sd = 0.5), 
          add = TRUE, 
          col = "darkgreen", 
          lwd = 2)
  }
  dev.print(device   = png,
            filename = "plot-ch01pr29-1.png",
            height   = 2*480,
            width    = 3*480)
  close.screen(all.screens = TRUE)
  
  # Joint distribution
  y <- transmute(data4.df, 
            y1 = p1 - p3,
            y2 = p2 - p4)
  to.plot <- kde2d(y$y1, y$y2)
  # Plotting
  plot.new()
  persp(to.plot, box = FALSE, main = "y = (p1 - p3, p2 - p4)", theta = 45)
  dev.print(device   = png,
            filename = "plot-ch01pr29-2.png",
            height   = 1*480,
            width    = 1*480)
  close.screen(all.screens = TRUE)
  
  # Try when N = 6
  data6.df <<- tbl_df(rand.prob.dist1(num.samples = num.samples, N = 6))
  plot.new()
  split.screen(c(2,3))
  for (i in 1:6) {
    screen(i)
    hist(data6.df[, i][[1]],
         probability = TRUE,
         main = colnames(data6.df)[i],
         xlab = NULL,
         ylab = NULL)
    curve(dnorm(x, mean = mean(data6.df[, i][[1]]), sd = 0.5), 
          add = TRUE, 
          col = "darkgreen", 
          lwd = 2)
  }
  dev.print(device   = png,
            filename = "plot-ch01pr29-3.png",
            height   = 2*480,
            width    = 3*480)
  close.screen(all.screens = TRUE)
}

ch01pr30 <- function(){
  # Investigating a different probability distribution
  require(MASS)
  require(dplyr)
  num.samples = 500
  data.df <<- tbl_df(rand.prob.dist2(num.samples = num.samples))
  
  # Plotting
  combos  <- as.matrix(expand.grid(1:4, 1:4)) 
  to.drop <- c(1:4, 6:8, 11:12, 16)
  to.plot <<- combos[-to.drop, ]
  
  plot.new()
  splits <- split.screen(c(2, 3))
  for (i in 1:6) {
    screen(i)
    x.i <- data.df[, to.plot[i,]][[1]]
    y.i <- data.df[, to.plot[i,]][[2]]
    den3d <- kde2d(x.i, y.i)
    persp(den3d, 
          theta = 30,
          phi   = 30,
          main  = paste(colnames(data.df)[to.plot[i,]], collapse = ", "))
  }
  dev.print(device = png,
            filename = "plot-ch01pr30.png",
            height = 2*480,
            width = 3*480)
  close.screen(splits, all.screens = TRUE)
}

ch01pr31 <- function(){
  # 31. Buffon's experiment is a biased estimator
  exact.result <- sum((100 / 1:100) * 
                        choose(100, 1:100) * 
                        (1/pi)^(1:100) * 
                        (1 - 1/pi)^(100 - 1:100))  # ~3.21
  num.experiments <- 2000
  num.trials <- 100
  
  results <- numeric(num.experiments)
  for (i in 1:num.experiments) {
    results[i] <- buffon(trials = num.trials)
  }
  message("Exact result for pi estimate:")
  print(exact.result)
  message("Average estimated value for pi:")
  print(mean(results))
  
  plot.new()
  hist(results,
       main = "Histogram of estimated pi values")
  abline(v   = mean(results),
         col = "red",
         lwd = 5)
  abline(v   = mean(results) + c(-1, 1) * sd(results),
         col = "blue",
         lty = "dashed")
  arrows(x0   = mean(results) - sd(results), y0 = 0,
         x1   = mean(results) + sd(results), y1 = 0,
         col  = "blue",
         code = 3,
         lwd  = 3)
  abline(v   = exact.result,
         col = "yellow",
         lwd = 3)
  abline(v   = pi,
         col = "green",
         lwd = 5)
  
  dev.print(device   = png,
            filename = "plot-ch01pr31.png",
            height   = 1*480,
            width    = 1*480)
}