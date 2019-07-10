#' Draw statistical distributions.
#' @inheritParams distanceFromDistribution
#' @return no returned value, a plot is drawn in the current device.
drawDistribution <- function(distribution, param1, param2) {
	
	if(distribution == "normal")
		drawGaussian(param1, param2)
	if(distribution == "uniform")
		drawUniform(param1, param2)
	if(distribution == "poisson")
		drawPoisson(param1)
	if(distribution == "bernoulli")
		drawBernoulli(param1)
	if(distribution == "binomial")
		drawBinomial(param1, param2)
	if(distribution == "geometric")
		drawGeometric(param1)
	if(distribution == "degenerate")
		drawDegenerate(param1)
	if(distribution == "chisq")
		drawChiSquared(param1)
	if(distribution == "weibull")
		drawWeibull(param1, param2)
}

#' Draw Weibull distribution.
#' @inheritParams drawDistribution
#' @inherit drawDistribution return
#' @importFrom grDevices grey
#' @importFrom graphics points segments
#' @importFrom stats dweibull
drawWeibull <- function(param1, param2) {
	
	xVec <- seq(0.1, 12, length.out = 1200)
	yVec <- dweibull(xVec, shape = param1, scale = param2)
	yVec <- yVec / max(yVec)
	
	points(xVec, yVec, type = "l", lwd = 3, col = grey(0.7))
	segments(x0 = 0, y0 = 0, x1 = 10, lwd = 3)
	
}

#' Draw Chi-squared distribution.
#' @inheritParams drawDistribution
#' @inherit drawDistribution return
#' @importFrom grDevices grey
#' @importFrom stats dchisq
drawChiSquared <- function(param1) {
	
	xVec <- seq(0.1, 12, length.out = 1200)
	yVec <- dchisq(xVec, df = param1)
	yVec <- yVec / max(yVec)
	
	points(xVec, yVec, type = "l", lwd = 3, col = grey(0.7))
	segments(x0 = 0, y0 = 0, x1 = 10, lwd = 3)
	
}

#' Draw degenerate distribution.
#' @inheritParams drawDistribution
#' @inherit drawDistribution return
#' @importFrom graphics arrows
drawDegenerate <- function(param1) {
	
	arrows(x0 = param1, y1 = 0.3, y0 = 1, length = 0.5, lwd = 3, col = grey(0.7))
	
}

#' Draw geometric distribution.
#' @inheritParams drawDistribution
#' @inherit drawDistribution return
#' @importFrom stats dgeom
#' @importFrom grDevices grey
drawGeometric <- function(param1) {
	
	xVec <- 0:12
	yVec <- dgeom(xVec, prob = param1)
	yVec <- yVec / max(yVec)
	
	for(i in 1:length(xVec)) {
		segments(x0 = xVec[i] - 0.5, y0 = 0, y1 = yVec[i], lwd = 3, col = grey(0.7))
		segments(x0 = xVec[i] + 0.5, y0 = 0, y1 = yVec[i], lwd = 3, col = grey(0.7))
		segments(y0 = yVec[i], x0 = xVec[i] - 0.5, x1 = xVec[i] + 0.5, lwd = 3, col = grey(0.7))
	}
	
}

#' Draw binomial distribution.
#' @inheritParams drawDistribution
#' @inherit drawDistribution return
#' @importFrom grDevices grey
#' @importFrom stats dbinom
drawBinomial <- function(param1, param2) {
	
	xVec <- 0:param1
	yVec <- dbinom(xVec, size = param1, prob = param2)
	yVec <- yVec / max(yVec)
	
	for(i in 1:length(xVec)) {
		segments(x0 = xVec[i] - 0.5, y0 = 0, y1 = yVec[i], lwd = 3, col = grey(0.7))
		segments(x0 = xVec[i] + 0.5, y0 = 0, y1 = yVec[i], lwd = 3, col = grey(0.7))
		segments(y0 = yVec[i], x0 = xVec[i] - 0.5, x1 = xVec[i] + 0.5, lwd = 3, col = grey(0.7))
	}
	
}

#' Draw Bernouilli distribution.
#' @inheritParams drawDistribution
#' @inherit drawDistribution return
#' @importFrom grDevices grey
drawBernoulli <- function(param1) {
	
	segments(x0 = -0.5, y0 = 0, y1 = 1 - param1, lwd = 3, col = grey(0.7))
	segments(x0 = 0.5, y0 = 0, y1 = 1 - param1, lwd = 3, col = grey(0.7))
	segments(y0 = 1 - param1, x0 = -0.5, x1 = 0.5, lwd = 3, col = grey(0.7))

	segments(x0 = 1.5, y0 = 0, y1 = param1, lwd = 3, col = grey(0.7))
	segments(x0 = 0.5, y0 = 0, y1 = param1, lwd = 3, col = grey(0.7))
	segments(y0 = param1, x0 = 1.5, x1 = 0.5, lwd = 3, col = grey(0.7))
	
}

#' Draw Poisson distribution.
#' @inheritParams drawDistribution
#' @inherit drawDistribution return
#' @importFrom stats dpois
#' @importFrom grDevices grey
drawPoisson <- function(param1) {
	
	xVec <- 0:12
	yVec <- dpois(xVec, lambda = param1)
	yVec <- yVec / max(yVec)
	
	for(i in 1:length(xVec)) {
		segments(x0 = xVec[i] - 0.5, y0 = 0, y1 = yVec[i], lwd = 3, col = grey(0.7))
		segments(x0 = xVec[i] + 0.5, y0 = 0, y1 = yVec[i], lwd = 3, col = grey(0.7))
		segments(y0 = yVec[i], x0 = xVec[i] - 0.5, x1 = xVec[i] + 0.5, lwd = 3, col = grey(0.7))
	}
	
}

#' Draw Gaussian distribution.
#' @inheritParams drawDistribution
#' @inherit drawDistribution return
#' @importFrom stats dnorm
#' @importFrom grDevices grey
drawGaussian <- function(param1 = 5, param2 = 2) {
	
	xVec <- seq(-1, 12, length.out = 1300)
	yVec <- dnorm(xVec, mean = param1, sd = param2)
	yVec <- yVec / max(yVec)
	
	points(xVec, yVec, type = "l", lwd = 3, col = grey(0.7))
	segments(x0 = 0, y0 = 0, x1 = 10, lwd = 3)
	
}

#' Draw Uniform distribution.
#' @inheritParams drawDistribution
#' @inherit drawDistribution return
#' @importFrom grDevices grey
drawUniform <- function(param1 = 3, param2 = 7) {
	
	segments(x0 = param1, y0 = 0, y1 = 0.5, lwd = 3, col = grey(0.7))
	segments(x0 = param1, y0 = 0.5, x1 = param2, lwd = 3, col = grey(0.7))
	segments(x0 = param2, y0 = 0, y1 = 0.5, lwd = 3, col = grey(0.7))
	
}






