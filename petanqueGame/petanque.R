library(oaPlots)
library(RColorBrewer)
library(dplyr)
library(plotrix)
source("determineOutcome.R")
source("drawFunctions.R")
source("plotDistribution.R")
source("generateOptions.R")
source("newGame.R")


posDF <- newGame()



throwBall <- function(distribution = "normal", param1 = 5, 
		param2 = 1, posDF = posDF){
	
	refreshPlot(posDF)
	i <- min(which(!posDF$thrown))
	
	# TODO add distributions
	if(distribution == "normal")
		distance <- rnorm(1, mean = param1, sd = param2)
	if(distribution == "uniform")
		distance <- runif(1, param1, param2)

	drawDistribution(distribution = distribution, param1 = param1, param2 = param2)
	animateThrow(distance, color = oaColors(posDF$color[i]))
	segments(x0 = distance, y0 = 1.1, y1 = 0, col = oaColors(posDF$color[i]), lwd = 4)
	
	posDF$y[i] <- ifelse(distance > 10 | distance < 0, -0.3, 0.09)
	points(x = distance, y = posDF$y[i], col = oaColors(posDF$color[i]),
			cex = 3, pch = 19)  # TODO as circle

	posDF$thrown[i] <- TRUE
	posDF$x[i] <- distance
	
	# TODO posDF <- detectColission()
	if(i < 7) {
		drawHuman(color = posDF$color[i+1]); Sys.sleep(1)
	}
		
	return(posDF)
}

options <- generateOptions()
printOptions(options)


posDF <- pickOption(options, optionSelected = 1, posDF); options <- generateOptions(); printOptions(options)
posDF <- pickOption(options, optionSelected = 2, posDF); options <- generateOptions(); printOptions(options)
posDF <- pickOption(options, optionSelected = 3, posDF); options <- generateOptions(); printOptions(options)

# throw all the balls
#posDF <- throwBall(distribution = "normal", param1 = 5, param2 = 1.5, posDF); Sys.sleep(2)
#posDF <- throwBall(distribution = "normal", param1 = 11, param2 = 1, posDF); Sys.sleep(2)
#posDF <- throwBall(distribution = "normal", param1 = 7, param2 = 2, posDF); Sys.sleep(2)
#posDF <- throwBall(distribution = "normal", param1 = 5, param2 = 1, posDF); Sys.sleep(2)
#posDF <- throwBall(distribution = "normal", param1 = 4.5, param2 = 3, posDF); Sys.sleep(2)
#posDF <- throwBall(distribution = "normal", param1 = 5, param2 = 1.5, posDF); Sys.sleep(2)

determineOutcome(posDF)
newGame()




















