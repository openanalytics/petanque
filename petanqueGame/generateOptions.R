


generateOptions <- function() {
	
	distributions <- c("normal", "uniform")
	type <- sample(distributions, 3, replace = TRUE)
	
	out <- list()
	for(i in 1:length(type)) {
		if(type[i] == "normal") {
			param1 <- round(runif(1, 1, 9), 1)
			param2 <- round(runif(1, 0.5, 3.5), 1)
			out[[i]] <- data.frame(type = type[i], param1 = param1, param2 = param2)
			out[[i]][, 1] <- as.character(out[[i]][, 1])
		}
		
		if(type[i] == "uniform") {
			param1 <- round(runif(1, -1, 7), 1)
			param2 <- round(param1 + runif(1, 0.1, 7), 1)
			out[[i]] <- data.frame(type = type[i], param1 = param1, param2 = param2)
		}
		
		
		out[[i]] %>% mutate_if(is.factor, as.character) -> out[[i]]
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
		
		if(!is.null(param2)) {
			cat('Option ', i, ": ", unlist(vec['type']), "(", unlist(vec['param1']),
					", ", unlist(vec['param2']), ")\n", sep = "")
		} else {
			cat('Option ', i, ": ", unlist(vec['type']), "(", unlist(vec['param1']),
					")\n", sep = "")
		}
	}
	
}