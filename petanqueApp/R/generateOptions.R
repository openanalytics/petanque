


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

pickOption <- function(options, optionSelected = 1, posDF) {
	
	picked <- options[[optionSelected]]
	posDF <- throwBall(distribution = unlist(picked["type"]), 
			param1 = unlist(picked['param1']), param2 = unlist(picked['param2']), posDF); 
	return(posDF)
}

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

printDistr <- function(distr) {
  paste0(distr$type, " (", round(distr$param1, 1), 
      if (!is.na(distr$param2)) paste0(", ", round(distr$param2, 1)),
      ")")
}
