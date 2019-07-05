#' Detect collision
#' @param posDF Data.frame with balls position
#' @param collisNo Integer with collision number
#' @return Updated \code{posDF}
#' @author Jason Waddell, small update Laure Cougnaud
#' @export
detectCollision <- function(posDF, collisNo = 0) {
	
	# determine left and right sides of the ball
	posDF$xMin <- with(posDF, x-width/2)
	posDF$xMax <- with(posDF, x+width/2)
	
	# TODO out of bounds balls...
	idxSitting <- which(posDF$thrown & posDF$travelDist == 0 & posDF$x >= 0 & posDF$x <= 10)
	sitting <- posDF[idxSitting, ]
	idxFlying <- which(posDF$thrown & posDF$travelDist != 0)
	flying <- posDF[idxFlying, ]
	
	if(length(idxFlying) > 0){
		
		for(i in 1:nrow(sitting)) {
			
			sittingI <- sitting[i, ]
			
			# compute distance with left/right sides of the ball
			idxHit <- which(sign(sittingI$xMin - flying$xMax) != sign(sittingI$xMax - flying$xMin))
			
			if(length(idxHit) > 0) {
				
				ballDists <- sittingI[, "x"] - flying[idxHit, "x"]
				
				# only consider the closest ball
				idxThrowHit <- which.min(abs(ballDists))
				dHit <- ballDists[idxThrowHit]
				
				throwHitInfo <- flying[idxHit, ][idxThrowHit, ]
				
				# get kinetic energy
				throwBallKE <- getKE(
					m = getMass(throwHitInfo$type), 
					v = getSpeedFromDistance(d = throwHitInfo$travelDist)
				)
				
				# Elastic collision: conservation of kinetic energy
				# get speed of each hit ball
				vHit <- getVFromKE(
					KE = throwBallKE, 
					m = getMass(sittingI[, "type"])
				)
				
				# get distance of hit ball
				signDistHit <- sign(dHit)
				# if no distance, throw the ball in the opposite side of the field
				if(signDistHit == 0)	signDistHit <- -1*sign(sittingI$x - 5)
				a <- ifelse(sittingI$type == "target", 1000, 10)
				sitting$travelDist[i] <-  signDistHit * getDistanceFromSpeed(v0 = vHit, a = a)
	#			if(ballDists[idx] < 0)
	#				sitting$travelDist[i] <- -3 / (2^(collisNo))
	#			if(ballDists[idx] > 0)
	#				sitting$travelDist[i] <- 3 / (2^(collisNo))
	#			if(ballDists[idx] == 0 & sitting$x[i] < 5)
	#				sitting$travelDist[i] <- -3 / (2^(collisNo))
	#			if(ballDists[idx] == 0 & sitting$x[i] >= 5)
	#				sitting$travelDist[i] <- 3 / (2^(collisNo))
			}
		}
		
		# combine, sort, return
		flying$travelDist <- rep(0, nrow(flying))
		
	}
	out <- rbind.data.frame(sitting, flying)
	
	idxNotUsed <- setdiff(seq_len(nrow(posDF)), c(idxFlying, idxSitting))
	posDF <- rbind.data.frame(out, posDF[idxNotUsed, ])
		
	posDF <- posDF[order(posDF$id), ]
	
	posDF[, c("xMin", "xMax")] <- NULL
	
	return(posDF)
	
}