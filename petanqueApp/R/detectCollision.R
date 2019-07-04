detectCollision <- function(posDF, collisNo = 0) {
	
	# TODO out of bounds balls...
	sitting <- posDF[which(posDF$thrown & posDF$travelDist == 0 & 
							posDF$x >= 0 & posDF$x <= 10), ]
	flying <- posDF[which(posDF$thrown & posDF$travelDist != 0), ]
	
	for(i in 1:nrow(sitting)) {
		ballDists <- rep(sitting$x[i], nrow(flying)) - flying$x
		if(any(abs(ballDists) <= 0.4)) { # TODO width hardcoded collision detected
			idx <- which(abs(ballDists) == min(abs(ballDists)))
			if(ballDists[idx] < 0)
				sitting$travelDist[i] <- -3 / (2^(collisNo))
			if(ballDists[idx] > 0)
				sitting$travelDist[i] <- 3 / (2^(collisNo))
			if(ballDists[idx] == 0 & sitting$x[i] < 5)
				sitting$travelDist[i] <- -3 / (2^(collisNo))
			if(ballDists[idx] == 0 & sitting$x[i] >= 5)
				sitting$travelDist[i] <- 3 / (2^(collisNo))
		}
	}
	
	# combine, sort, return
	flying$travelDist <- rep(0, nrow(flying))
	out <- rbind.data.frame(sitting, flying)
	
	if(any(!posDF$thrown)) {
		posDF <- rbind.data.frame(out, posDF[which(!posDF$thrown), ])
	} else {
		posDF <- out
	}
		
	posDF <- posDF[order(posDF$id), ]
	
	return(posDF)
	
}