drawDistribution <- function(distribution, param1, param2) {
	
	if(distribution == "normal")
		drawGaussian(param1, param2)
	if(distribution == "uniform")
		drawUniform(param1, param2)
	
}




drawGaussian <- function(param1 = 5, param2 = 2) {
	
	xVec <- seq(-1, 12, length.out = 1300)
	yVec <- dnorm(xVec, mean = param1, sd = param2)
	yVec <- yVec / max(yVec)
	
	points(xVec, yVec, type = "l", lwd = 3, col = grey(0.7))
	segments(x0 = 0, y0 = 0, x1 = 10, lwd = 3)
	
}

drawUniform <- function(param1 = 3, param2 = 7) {
	
	segments(x0 = param1, y0 = 0, y1 = 0.5, lwd = 3, col = grey(0.7))
	segments(x0 = param1, y0 = 0.5, x1 = param2, lwd = 3, col = grey(0.7))
	segments(x0 = param2, y0 = 0, y1 = 0.5, lwd = 3, col = grey(0.7))
	
}






