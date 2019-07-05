#' @export
drawField <- function() {
	blankPlot(xlim = c(-2, 13), ylim = c(-0.5, 1))
	segments(x0 = 0, y0 = 0, x1 = 10, lwd = 3)
	text(x = 0, y = -0.1, "0", font = 2, adj = c(0.5, 1))
	text(x = 5, y = -0.1, "5", font = 2, adj = c(0.5, 1))
	text(x = 10, y = -0.1, "10", font = 2, adj = c(0.5, 1))
	
	# tree 1
	tCenter <- 12
	tWidth <- 0.3
	id <- 4
	trunkInner <- brewer.pal(11, "BrBG")[id]
	trunkOuter <- brewer.pal(11, "BrBG")[id-1]
	polygon(x = c(tCenter + tWidth, tCenter + tWidth, tCenter + 1.5 *tWidth,
					tCenter - 1.5 *tWidth, tCenter - tWidth, tCenter - tWidth,
					tCenter + tWidth), y = c(1.5, 0.2, 0.01, 0.01, 0.2, 1.5, 1.5), 
			col = trunkInner, border = trunkOuter, lwd = 3)
	pal <- brewer.pal(8, "Greens")
	draw.circle(x = 12, y = 0.9, radius = 0.95, nv = 60, border = pal[8], col = pal[6])
	draw.circle(x = 12.35, y = 0.8, radius = 0.6, nv = 60, border = pal[8], col = pal[7])
	
	# tree 2
	tCenter <- -1.7
	tWidth <- 0.15
	id <- 3
	trunkInner <- brewer.pal(11, "BrBG")[id]
	trunkOuter <- brewer.pal(11, "BrBG")[id-1]
	polygon(x = c(tCenter + tWidth, tCenter + tWidth, tCenter + 1.2*tWidth,
					tCenter - 1.2*tWidth, tCenter - tWidth, tCenter - tWidth,
					tCenter + tWidth), y = c(0.6, 0.2, 0.01, 0.01, 0.2, 0.6, 0.6), 
			col = trunkInner, border = trunkOuter, lwd = 3)
	pal <- brewer.pal(8, "Greens")
	draw.circle(x = -1.7, y = 0.6, radius = 0.95, nv = 60, border = pal[8], col = pal[8])
	draw.circle(x = -1.8, y = 0.5, radius = 0.6, nv = 60, border = pal[8], col = pal[7])
	
	# tree 3
	tCenter <- 13
	tWidth <- 0.15
	id <- 3
	trunkInner <- brewer.pal(11, "BrBG")[id]
	trunkOuter <- brewer.pal(11, "BrBG")[id-1]
	polygon(x = c(tCenter + tWidth, tCenter + tWidth, tCenter + 1.2*tWidth,
					tCenter - 1.2*tWidth, tCenter - tWidth, tCenter - tWidth,
					tCenter + tWidth), y = c(0.4, 0.2, 0.01, 0.01, 0.2, 0.4, 0.4), 
			col = trunkInner, border = trunkOuter, lwd = 3)
	pal <- brewer.pal(8, "Greens")
	draw.circle(x = 13, y = 0.4, radius = 0.6, nv = 60, border = pal[8], col = pal[8])
	#draw.circle(x = -1.8, y = 0.5, radius = 0.6, nv = 60, border = pal[8], col = pal[6])
	
	drawFlower(x = 11, stemHeight = 0.2, colNum = 1)
	drawFlower(x = 11.5, stemHeight = 0.3, colNum = 2)
}

#' @export
drawFlower <- function(x, stemHeight, colNum = 2){

	ygrid <- seq(0.01, stemHeight, length.out = 100)
	perc <- seq(-pi, pi, length.out = 100)
	points(x = x + 0.02*sin(perc), y = ygrid, type = "l", lwd = 3, 
			col = oaColors("green"))
	
	colVec <- brewer.pal(6, "Set1")[c(colNum, 6)]
	points(x = x + 2 * c(-0.015, -0.025, -0.015, 0.015, 0.025, 0.015),
			y = stemHeight + 2 * c(-0.015, 0, 0.015, 0.015, 0, -0.015), 
			col = colVec[1], pch = 19, cex = 3.5)
	points(x = x, y = stemHeight, 
			col = colVec[2], pch = 19, cex = 2)
}

#' @export
drawHuman <- function(color = "orange") {
	
	#rect(xleft = -0.6, xright = -0.01, ybottom = 0, ytop = 0.7, col = "white", border = "white")
	segments(x0 = -0.3, x1 = -0.4, y0 = 0, y1 = 0.2, lwd = 2) # leg 1
	segments(x0 = -0.5, x1 = -0.4, y0 = 0, y1 = 0.2, lwd = 2) # leg 2
	segments(x0 = -0.4, y0 = 0.35, y1 = 0.2, lwd = 2) # torso
	
	points(x = -0.4, y = 0.44, cex = 3, pch = 19) # head
	
	if(color %in% c("orange", "blue")) {
		segments(x0 = -0.4, x1 = -0.1, y0 = 0.27, y1 = 0.31, lwd = 2) # arm
		points(x = -0.1, y = 0.29, cex = 1, pch = 19, col = oaColors(color))
	}
		
}

## NB: step = Inf preserves 'usual' animation, otherwise 'shiny' animation is used
#' @export
animateThrow <- function(distance, color, step = Inf) {
  if (step >= 1) {
    points(x = 1.2 * distance / 5, y = 0.4, cex = 1, pch = 19, col = color)
    if (is.infinite(step))
      Sys.sleep(0.5)
  }
  if (step >= 2) {
    points(x = 2.2 * distance / 5, y = 0.65, cex = 1.5, pch = 19, col = color)
    if (is.infinite(step))
      Sys.sleep(0.5)
  }
  if (step >= 3) {
    points(x = 3.5 * distance / 5, y = 0.6, cex = 2, pch = 19, col = color)
    if (is.infinite(step))
      Sys.sleep(0.5)
  }
  if (step >= 4) {
    points(x = 4.3 * distance / 5, y = 0.4, cex = 2.5, pch = 19, col = color)
    if (is.infinite(step))
      Sys.sleep(0.5)
  }
  # points(x = distance, y = 0, cex = 3, pch = 19, col = color); Sys.sleep(1/30)

	# then wipe it
	
}
#' @export
animateCollision <- function(posDF) {  # startX, distance, color
	
	flying <- posDF[which(posDF$thrown & posDF$travelDist != 0), ]
	
	for(jAnimation in 1:4) {
		for(i in 1:nrow(flying)) {
			if(jAnimation == 1)
				points(x = flying$x[i] + 1.2 * flying$travelDist[i] / 5,
						y = 0.4/2, cex = 1, pch = 19, col = oaColors(flying$color[i])) 
			if(jAnimation == 2)
				points(x = flying$x[i] + 2.2 * flying$travelDist[i] / 5,
						y = 0.65/2, cex = 1, pch = 19, col = oaColors(flying$color[i])) 
			if(jAnimation == 3)
				points(x = flying$x[i] + 3.5 * flying$travelDist[i] / 5,
						y = 0.6/2, cex = 1, pch = 19, col = oaColors(flying$color[i])) 
			if(jAnimation == 4)
				points(x = flying$x[i] + 4.3 * flying$travelDist[i] / 5,
						y = 0.4/2, cex = 1, pch = 19, col = oaColors(flying$color[i])) 
		}
		Sys.sleep(0.5)
	}
	
	posDF$x <- posDF$x + posDF$travelDist
	draw.circle(x = posDF$x[1], y = posDF$y[1], col = oaColors(posDF$color[1]),  
			radius = posDF$width[1]/2, nv = 120, border = oaColors(posDF$color[1]))
	#points(x = posDF$x[1], y = 0.05, col = oaColors("red"), pch = 19, cex = 1.5)
	
	# determine y values
	for(i in 1:nrow(posDF)) {
		if(i == 1) {
			if(posDF$x[i] > 10) {
				posDF$x[i] <- 10
				draw.circle(x = posDF$x[1], y = posDF$y[1], col = oaColors(posDF$color[1]),  
						radius = posDF$width[1]/2, nv = 120, border = oaColors(posDF$color[1]))
			}	
			if(posDF$x[i] < 0) {
				posDF$x[i] <- 0
				draw.circle(x = posDF$x[1], y = posDF$y[1], col = oaColors(posDF$color[1]),  
						radius = posDF$width[1]/2, nv = 120, border = oaColors(posDF$color[1]))
			}
				
		}
				
		posDF$y[i] <- ifelse(posDF$x[i] > 10 | posDF$x[i] < 0, -0.3, 0.09)
	}
		
	
	for(jRow in 7:2)
		if(posDF$thrown[jRow])
			draw.circle(x = posDF$x[jRow], y = posDF$y[jRow], col = oaColors(posDF$color[jRow]),  
					radius = posDF$width[jRow]/2, nv = 120, border = oaColors(posDF$color[jRow]))
			#points(x = posDF$x[jRow], y = posDF$y[jRow], col = oaColors(posDF$color[jRow]), cex = 3, pch = 19)
	
	return(posDF)
	
}

#' @export
refreshPlot <- function(posDF) {
	
	drawField()
	draw.circle(x = posDF$x[1], y = posDF$y[1], col = oaColors(posDF$color[1]),  
			radius = posDF$width[1]/2, nv = 120, border = oaColors(posDF$color[1]))
	for(jRow in 2:7)
		if(posDF$thrown[jRow])
			draw.circle(x = posDF$x[jRow], y = posDF$y[jRow], col = oaColors(posDF$color[jRow]),  
					radius = posDF$width[jRow]/2, nv = 120, border = oaColors(posDF$color[jRow]))
	drawHuman(color = "none"); #Sys.sleep(0.3)
	
}
