#' @export
drawField <- function(newPlot = FALSE) {
	if(newPlot) {
		blankPlot(xlim = c(-2, 13), ylim = c(-0.5, 1))
	} else {
		rect(xleft = -4, xright = 15, ybottom = -1, ytop = 2, col = "white", border = "white")
	}
	
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
animateCollision <- function(posDF, step = Inf) {  # startX, distance, color
	
  posDFstart <- posDF
  
  # generalized animation loop

  #  animationStep <- if (is.infinite(step)) Inf else (step-7) %% 5+1  # 7->1, ..., 11->5, 12->1, ...
  animationStep <- if (is.infinite(step)) Inf else step-6 # 7-> 1
  noCycles <- ((animationStep-1) %/% 5 + 1)
  
  xProp <- c(1.2, 2.2, 3.5, 4.3, 5)/5
  yPos  <- c(0.2, 0.325, 0.3, 0.2) 
  
  curData <- posDF
    
  for (iCycle in seq_len(noCycles)) {
    
    flying <- curData[which(curData$thrown & curData$travelDist != 0), ]
    nFlying <- nrow(flying)
    
    stepsInCycle <- if (animationStep >= iCycle*5) 5 else animationStep-5*(iCycle-1) 

    # remove flying balls old positions
    redrawBalls(curData, except = flying$id)
    
    for (jStep in seq_len(min(4, stepsInCycle))) {
      realStep <- (iCycle-1)*5+jStep
      # 1-4
      if (animationStep >= realStep)
        points(x = flying$x + xProp[jStep] * flying$travelDist,
            y = rep(yPos[jStep], nFlying), cex = 1, pch = 19, col = oaColors(flying$color)) 

    }
    
    if (stepsInCycle == 5 &&  animationStep >= iCycle*5) {
      curData$x <- curData$x + curData$travelDist

      # out of bounds check and draw new balls positions
      curData$x <- ifelse(curData$x > 10 & curData$id == 1, 10, curData$x)
      curData$x <- ifelse(curData$x < 0 & curData$id == 1, 0, curData$x)
      curData$y <- ifelse(curData$x > 10 | curData$x < 0, -0.3, 0.09)
      curData$y[1] <- 0.05
      
      thrownData <- curData[curData$thrown, ]
      for (iRow in rev(seq_len(nrow(thrownData))))
        draw.circle(x = thrownData$x[iRow], y = thrownData$y[iRow], col = oaColors(thrownData$color[iRow]),  
            radius = thrownData$width[iRow]/2, nv = 120, border = oaColors(thrownData$color[iRow]))

      curData <- detectCollision(curData)
      
    }
    
  }

  curData
  
##	for(jAnimation in 1:4) {
##		for(i in 1:nrow(flying)) {
#  if (animationStep >= 1)
#    points(x = flying$x + 1.2 * flying$travelDist / 5,
#        y = rep(0.4/2, nFlying), cex = 1, pch = 19, col = oaColors(flying$color)) 
#  if (animationStep >= 2)
#    points(x = flying$x + 2.2 * flying$travelDist / 5,
#        y = rep(0.65/2, nFlying), cex = 1, pch = 19, col = oaColors(flying$color)) 
#  if (animationStep >= 3)
#    points(x = flying$x + 3.5 * flying$travelDist / 5,
#        y = rep(0.6/2, nFlying), cex = 1, pch = 19, col = oaColors(flying$color)) 
#  if (animationStep >= 4)
#    points(x = flying$x + 4.3 * flying$travelDist / 5,
#        y = rep(0.4/2, nFlying), cex = 1, pch = 19, col = oaColors(flying$color)) 
##		}
##		Sys.sleep(0.5)
##	}
#	
#  if (animationStep >= 5) {
#    posDF$x <- posDF$x + posDF$travelDist
#    draw.circle(x = posDF$x[1], y = posDF$y[1], col = oaColors(posDF$color[1]),  
#        radius = posDF$width[1]/2, nv = 120, border = oaColors(posDF$color[1]))
#    #points(x = posDF$x[1], y = 0.05, col = oaColors("red"), pch = 19, cex = 1.5)
#    
#    
#    # determine y values
#    for(i in 1:nrow(posDF)) {
#      if(i == 1) {
#        if(posDF$x[i] > 10) {
#          posDF$x[i] <- 10
#          draw.circle(x = posDF$x[1], y = posDF$y[1], col = oaColors(posDF$color[1]),  
#              radius = posDF$width[1]/2, nv = 120, border = oaColors(posDF$color[1]))
#        }	
#        if(posDF$x[i] < 0) {
#          posDF$x[i] <- 0
#          draw.circle(x = posDF$x[1], y = posDF$y[1], col = oaColors(posDF$color[1]),  
#              radius = posDF$width[1]/2, nv = 120, border = oaColors(posDF$color[1]))
#        }
#        
#      }
#      
#      posDF$y[i] <- ifelse(posDF$x[i] > 10 | posDF$x[i] < 0, -0.3, 0.09)
#      posDF$y[1] <- 0.05
#    }
#    
#    
#    for(jRow in 7:2)
#      if(posDF$thrown[jRow])
#        draw.circle(x = posDF$x[jRow], y = posDF$y[jRow], col = oaColors(posDF$color[jRow]),  
#            radius = posDF$width[jRow]/2, nv = 120, border = oaColors(posDF$color[jRow]))
#    #points(x = posDF$x[jRow], y = posDF$y[jRow], col = oaColors(posDF$color[jRow]), cex = 3, pch = 19)
#    
#  }
#  
#  return(posDF)
	
}

#' @export
refreshPlot <- function(posDF, newPlot = FALSE) {
	
	drawField(newPlot = newPlot)
	draw.circle(x = posDF$x[1], y = posDF$y[1], col = oaColors(posDF$color[1]),  
			radius = posDF$width[1]/2, nv = 120, border = oaColors(posDF$color[1]))
	for(jRow in 2:7)
		if(posDF$thrown[jRow])
			draw.circle(x = posDF$x[jRow], y = posDF$y[jRow], col = oaColors(posDF$color[jRow]),  
					radius = posDF$width[jRow]/2, nv = 120, border = oaColors(posDF$color[jRow]))
	drawHuman(color = "none"); #Sys.sleep(0.3)
	
}

redrawBalls <- function(posDF, except = NULL) {
  
  toClean <- posDF[posDF$thrown, ]
  toDraw <- posDF[posDF$thrown & !(posDF$id %in% except), ]
  
  for (jRow in seq_len(nrow(toClean))) {
    draw.circle(x = toClean$x[jRow], y = toClean$y[jRow], col = "white",  
        radius = toClean$width[jRow]/2*1.05, nv = 120, border = "white")
  }
  
  for (jRow in rev(seq_len(nrow(toDraw)))) {
    draw.circle(x = toDraw$x[jRow], y = toDraw$y[jRow], col = oaColors(toDraw$color[jRow]),  
        radius = toDraw$width[jRow]/2, nv = 120, border = oaColors(toDraw$color[jRow]))
  }

}

