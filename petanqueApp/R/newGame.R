#' @import oaPlots
#' @import oaColors
#' @import RColorBrewer
#' @import plotrix
newGame <- function() {
	drawField()
	drawHuman("orange")
	
	# define the data frame which tracks the positions
	posDF <- data.frame(x = numeric(7), 
			type = c("target", rep(c("p1", "p2"), 3)),
			width = c(0.1, rep(0.2, 6)), 
			thrown = c(TRUE, rep(FALSE, 6)), 
			color = c("red", rep(c("orange", "blue"), 3)), 
			yPos = rep(0.09, 7))
	posDF$color <- as.character(posDF$color)
	# determine little ball positions
	posDF$x[1] <- runif(1, 3, 7)
	points(x = posDF$x[1], y = 0.05, col = oaColors("red"), pch = 19, cex = 1.5)
	
	return(posDF)
}