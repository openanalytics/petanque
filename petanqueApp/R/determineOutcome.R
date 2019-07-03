determineOutcome <- function(posDF) {
	
	# remove the target ball
	tmp <- posDF[-1, ]
	
	# set all the 'off field' balls to set value (15)
	if(any(tmp$x > 10 | tmp$x < 0))
		tmp$x[which(tmp$x > 10 | tmp$x < 0)] <- 15
	
	tmp$x <- abs(tmp$x - posDF$x[1])
	tmp <- tmp[order(tmp$x), ] # sort by abs diff between ball and target
	
	# determine winner
	winner <- tmp$type[1]
	
	# determine number of points
	pointsWon <- which(tmp$type != winner)[1] - 1
	
	return(list(winner = winner, pointsWon = pointsWon))
}
