# Modelling of the collision between Petanque balls
# 
# Author: lcougnaud
###############################################################################

#' Model collisions between petanque balls
#' @param data Data.frame with position: 'x', 
#' r: 'radius' and 'm': mass of each ball 
#' @param throwBallID ID of the thrown ball.
#' @param throwBallV Velocity of the thrown ball (m/s).
#' @return List with data.frame with iterative positions of the balls (in case of successive collisions).
#' @inheritParams getDistanceFromSpeed
#' @example inst/examples/collisionPetanque.R
#' @author Laure Cougnaud
#' @export
collisionPetanque <- function(data, throwBallID, throwBallV, a = 10){
	
	# determine left and right sides of the ball
	data$xMin <- with(data, x-r)
	data$xMax <- with(data, x+r)
	
	# information on thrown ball
	throwBallInfo <- data[match(throwBallID, data$ID), ]

	# extract hit balls:
	data$hit <- (
		data$ID != throwBallID &
		sign(data$xMin - throwBallInfo$xMax) != sign(data$xMax - throwBallInfo$xMin)
	)
	
	res <- if(any(data$hit)){
		
		throwBallKE <- getKE(m = throwBallInfo$m, v = throwBallV)
		
		# Elastic collision: conservation of kinetic energy
		# assume that the energy is equally spread between the hit balls
		# and that the thrown ball stops (the hit ball has no initial speed)
		hitBallKE <- throwBallKE/length(ballHit)
		
		# get speed of each hit ball
		data$v <- with(data, ifelse(hit, getVFromKE(KE = hitBallKE, m = m), NA))
		
		# get corresponding distance
		data$d <- with(data, ifelse(hit, getDistanceFromSpeed(v0 = v, a = a), NA))
		
		# update new coordinates
		data$x <- with(data, ifelse(hit, x+d, x))
		
		hitBallIDs <- subset(data, hit)$ID
		res <- lapply(hitBallIDs, function(hitBallID){
			collisionPetanque(
				data = data, throwBallID = ID, 
				throwBallV = subset(data, ID == hitBallID)$v
			)
		})
		
	}else list(data)
	
	return(res)
	
}

#' Get distance from initial speed
#' @param v0 Initial speed, in m/s
#' @param a Acceleration (currently fixed), decceleration of 10m/s^2 by default.
#' @return Achieved distance, in m
#' @author Laure Cougnaud, Maxim Nazarov
#' @export
getDistanceFromSpeed <- function(v0, a = 10){
	# vEnd = vInit + 2 * a * d
	# vEnd = 0
	d <- (v0^2)/(2*abs(a))
	return(d) 
}


#' Get initial speed of the projectile from the distance
#' 
#' From https://en.wikipedia.org/wiki/Projectile_motion, 
#' section: 'Maximum distance of projectile'
#' @param d Distance in m
#' @param theta Launching angle, 45 by default
#' @return Speed in m/s
#' @author Laure Cougnaud
#' @export
getSpeedFromDistance <- function(d, theta = 45){
	g <- 9.81 # gravitation constant (m/s^2)
	v0 <- sqrt((g*d)/sin(2*theta))
	return(v0)
}

#' Get Kinetic Energy
#' @param m Mass of the object in kg
#' @param v Object velocity, in m/s
#' @return Kinetic Energy in Joules
#' @author Laure Cougnaud
#' @export
getKE <- function(m, v){
	return(1/2 * m * v^2)
}

#' Get velocity from Kinetic Energy and Mass
#' @param KE Kinetic Energy in Joules
#' @param m Mass of the object in kg
#' @return Object velocity, in m/s
#' @author Laure Cougnaud
#' @export
getVFromKE <- function(KE, m){
	return(sqrt((2*KE)/m))
}
	

