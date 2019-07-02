# Example for the collisioPetanque function.
# 
# Author: Laure Cougnaud
###############################################################################

# size petanque field: 4 * 15m in length
mPetanque <- 0.680 # standards: 680 et 710 grammes
mTarget <- 0.015 # between 10 and 18 gramms
# from: https://www.petanqueshop.com/le-choix-d-une-boule-de-competition
radiusPetanque <- 0.075 # official competition: between 70,5 et 80 mm
radiusTarget <- 0.030 # 30 diameter +- 1mm

# position of thrown ball
x <- 7.9
v <- getSpeedFromDistance(d = x, theta = 45)

dataPetanque <- data.frame(
		ID = c(1:4),
		x = c(x, 2, 3, 8), # coordinates in m
		r = c(radiusPetanque, radiusPetanque, radiusPetanque, radiusTarget),
		m = c(mPetanque, mPetanque, mPetanque, mTarget)
)

collisionPetanque(data = dataPetanque, throwBallID = 1, throwBallV = v)

