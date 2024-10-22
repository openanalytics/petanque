


#' Generate a set of statistical distributions and associated parameters.
#' @param n Number of distributions/parameter sets to generate.
#' @inheritParams animateCollision
#' @return Sub-list of length \code{n} 
#' with 'type' of distribution, 'param1': first parameter,
#' 'param2': second parameter.
#' @importFrom stats rbeta
generateOptions <- function(posDF = NULL, n = 3) {
	
	distributions <- c("normal", "uniform", "poisson", "bernoulli", "binomial", 
			"geometric", "degenerate", "chisq", "weibull")
	type <- sample(distributions, n, replace = FALSE, 
			prob = c(15, 8, 8, 1, 8, 6, 3, 5, 4))
	maxTarget <- 10
	
	juiced <- sample(1:3, 1)
	targetLoc <- posDF$x[1] + runif(1, -1.5, 1.5)
	
	out <- list()
	for(i in 1:length(type)) {
		if(type[i] == "normal") {
			param1 <- round(runif(1, 1, 9), 1)
			param2 <- round(runif(1, 0.5, 3.5), 1)
			
			if(i == juiced)
				param1 <- targetLoc
		}
		
		if(type[i] == "uniform") {
			param1 <- round(runif(1, -1, 7), 1)
			param2 <- round(param1 + runif(1, 0.1, 7), 1)
			
			if(i == juiced) {
				diff <-  targetLoc - mean(c(param1, param2)) 
				param1 <- param1 + diff
				param2 <- param2 + diff
			}
		}
		
		if(type[i] == "poisson") {
			param1 <- round(8 * rbeta(1, 1.5, 0.6), 1)
			param2 <- NA
			
			if(i == juiced)
				param1 <- targetLoc
		}
		
		if(type[i] == "bernoulli") {
			param1 <- round(runif(1, 0.01, 0.99), 2)
			param2 <- NA
		}
		
		if(type[i] == "binomial") {
			param1 <- round(maxTarget * runif(1, 0.5, 1.3))+ 1
			param2 <- round(rbeta(1, 1, 0.9), 2)
			
			if(i == juiced) {
				param2 <- round(runif(1, 0.3, 0.7), 2)
				param1 <- ceiling(targetLoc / param2)
			}
		}
		
		if(type[i] == "geometric") {
			param1 <- round(runif(1, 0.1, 0.8), 1)
			param2 <- NA
			
			if(i == juiced)
				param1 <- round(1 / (1 + targetLoc), 1)
		}
		
		if(type[i] == "degenerate") {
			param1 <- round(runif(1, 0.5, 11), 2)
			param2 <- NA
			
			if(i == juiced)
				param1 <- targetLoc
		}
		
		if(type[i] == "chisq") {
			param1 <- round(10 * rbeta(1,1.5,0.6), 1)
			param2 <- NA
			
			if(i == juiced)
				param1 <- targetLoc
		}
		
		if(type[i] == "weibull") {
			param1 <- 0.8
			param2 <- round(runif(1, maxTarget/5 , maxTarget*1.5), 1)
			
			if(i == juiced)
				param2 <- targetLoc / gamma(2.25)
		}
		
		out[[i]] <- list(type = type[i], param1 = param1, param2 = param2)
	}
	
	
	return(out)
	
}

#' Pick a distribution from a set of statistical distributions/parameters,
#' and throw the ball with \code{\link{throwBall}}.
#' @param options List with set of statistical distributions/parameters,
#' as generated by: \code{\link{generateOptions}}.
#' @param optionSelected Integer with selected option,
#' 1 by default.
#' @inheritParams animateCollision
#' @return Updated \code{posDF} after the throw.
pickOption <- function(options, optionSelected = 1, posDF) {
	
	picked <- options[[optionSelected]]
	posDF <- throwBall(distribution = unlist(picked["type"]), 
			param1 = unlist(picked['param1']), param2 = unlist(picked['param2']), posDF); 
	return(posDF)
}

#' Print different options of statistical distributions/set of parameters.
#' @inheritParams pickOption
#' @return No return value, options are printed in the console.
printOptions <- function(options) {
	for(i in 1:3) {
		vec <- options[[i]]
		
		if(!is.na(vec['param2'])) {
			cat('Option ', i, ": ", unlist(vec['type']), "(", unlist(vec['param1']),
					", ", unlist(vec['param2']), ")\n", sep = "")
		} else {
			cat('Option ', i, ": ", unlist(vec['type']), "(", unlist(vec['param1']),
					")\n", sep = "")
		}
	}
	
}

#' Print information for a specific distribution.
#' @param distr List with distribution 'type', parameters: 'param1' and 'param2'.
#' @param inButton Logical, if TRUE (FALSE by default)
#' should the info fit into a button?
#' @return string with HTML code
printDistr <- function(distr, inButton = FALSE) {
  HTML(paste0(
				  # Distribution with first letter capital
				  toupper(substring(distr$type, 1, 1)), substring(distr$type, 2), 
				  if (inButton) "<br>" else " ", 
				  "(", 
		  switch(distr$type,
				  normal = paste0("\u03bc = ", round(distr$param1, 1), 
              ", \u03c3 = ", round(distr$param2, 1)),
				  uniform = paste0("min = ", round(distr$param1, 1), ", max = ", round(distr$param2, 1)),
				  poisson = paste0("\u03bb = ", round(distr$param1, 1)),
				  bernoulli = paste0("prob = ", round(distr$param1, 1)),
				  binomial = paste0("size = ", round(distr$param1, 1), ", prob = ", round(distr$param2, 1)),
				  geometric = paste0("prob = ", round(distr$param1, 1)),
				  degenerate = paste0("x = ", round(distr$param1, 1)),
				  chisq = paste0("df = ", round(distr$param1, 1)),
				  weibull = paste0("shape = ", round(distr$param1, 1), ", scale = ", round(distr$param2, 1))
				  ),
		  ")"))
}
