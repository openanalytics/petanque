#library(oaPlots)
#library(RColorBrewer)
#library(dplyr)
#library(plotrix)
#source("determineOutcome.R")
#source("drawFunctions.R")
#source("plotDistribution.R")
#source("generateOptions.R")
#source("newGame.R")
#source("messages.R")
#
#
#posDF <- newGame()

throwBall <- function(distribution = "normal", param1 = 5, 
		param2 = 1, posDF = posDF, step = Inf, distance = NULL) {
  
  i <- min(which(!posDF$thrown))
  if (is.null(distance))
    distance <- distanceFromDistribution(distribution = distribution, param1 = param1, param2 = param2)
  
  if (step >= 1) {
    refreshPlot(posDF)
    
    drawDistribution(distribution = distribution, param1 = param1, param2 = param2)
    
  }
  if (step >= 2) {
    animateThrow(distance, color = oaColors(posDF$color[i]), step = step-1)
  }
  if (step >= 6) {
    segments(x0 = distance, y0 = 1.1, y1 = 0, col = oaColors(posDF$color[i]), lwd = 4)
    
    posDF$y[i] <- ifelse(distance > 10 | distance < 0, -0.3, 0.09)
    points(x = distance, y = posDF$y[i], col = oaColors(posDF$color[i]),
        cex = 3, pch = 19)
  }

  if (step >= 7) {	
    if(distance < 0) {
      msg <- generateNegativeMessage()
      text(msg, x = 5, y = -0.4, font = 2, col = oaColors(posDF$color[i]))
    }
    
    if(distance > 10) {
      msg <- generatePositiveMessage(distance)
      text(msg, x = 5, y = -0.4, font = 2, col = oaColors(posDF$color[i]))
    }
    
    posDF$thrown[i] <- TRUE
    posDF$x[i] <- distance
    # TODO posDF <- detectColission()
    if(i < 7) {
      drawHuman(color = posDF$color[i+1]) #; Sys.sleep(1)
    }
  }
	return(posDF)
}


distanceFromDistribution <- function(distribution, param1, param2) {
  # TODO add distributions
  switch(distribution,
      "normal" = rnorm(1, mean = param1, sd = param2),
      "uniform" = runif(1, param1, param2),
      "poisson" = rpois(1, lambda = param1),
      "bernoulli" = rbinom(1, 1, prob = param1),
      "binomial" = rbinom(1, size = param1, prob = param2),
      "geometric" = rgeom(1, prob = param1),
      "degenerate" = param1,
      "chisq" = rchisq(1, df = param1),
      "weibull" = rweibull(1, shape = param1, scale = param2)
  )
}