# TODO: Add comment
# 
# Author: Vahid Nassiri
###############################################################################
## That's the last point the target can be located
maxTarget <- 10
allRes <- matrix(NA, 1000000, 11)
for (iRep in 1:1000000){
	## put the target somewhere at random, we might begin from
	## hald of the maxLocation so the target would not be placed very 
	## very close to the center.
	allRes[iRep, 1] <- targetLocation <- runif(1, min = maxTarget/2, max = maxTarget)
	## Here we try all the distributions
	allRes[iRep, 2] <- rbinom(1, round(maxTarget * runif(1, 0.5, 1.2))+ 1, rbeta(1, 1.5, 0.6))
	## Adding 0.01 to the rbinom is to prevent zeros
	allRes[iRep, 3] <- rpois(1, maxTarget * rbeta(1,1.5,0.6))
	minUnif <- runif(1, 0, maxTarget/1.1)
	maxUnif <- runif(1, minUnif, minUnif + rpois(1, maxTarget * runif(1, 0.5, 1.5)))
	allRes[iRep, 4] <- runif(1, minUnif, maxUnif)
	allRes[iRep, 5] <- rweibull(1, 0.8, runif(1, maxTarget/2 , maxTarget*2))
	## adding 1.01 to rbinom is to precent negative parameters in exponential dist.
	allRes[iRep, 6] <- rlnorm(1, runif(1,log(maxTarget*0.3), log(maxTarget*1.1)), runif(1, 0, 1))
	allRes[iRep, 7] <- rchisq(1, maxTarget * rbeta(1,1.5,0.6))
	allRes[iRep, 8] <- rnorm(1, maxTarget * runif(1, 0.4, 0.6), runif(1, 0.6, 0.9))
	## These two will be in [0, 1]
	allRes[iRep, 9] <- rbinom(1, 1, runif(1, 0.01, 0.99))
	allRes[iRep, 10] <- rbeta(1, runif(1,1, 10), runif(1, 1, 10))
	## we can also consider degenrate distribution!
	## if they are lucky the randomly selected value is close to
	## the taarget, but then they need to take the risk of a random result or a definite result.
	## So we generate a uniform random variable, but then the random degenerate value
	## is always this. For example, when we say degenrate(3.6), the player know it will get 3.6,
	## now it comes the question of whether it gonna take the risk of selecting this 3.6, or go
	## for some random numbers which could possibly be closer.
	allRes[iRep, 11] <- runif(1, min = maxTarget*0.05, max = maxTarget*1.1)
}
colnames(allRes) <- c("target", "binomial", "Poisson", "uniform", "Weibull", 
		"log-normal", "chi2", "normal", "Bernouli", "beta", "degenerate")
round(apply(allRes, 2, summary), 3)

allRes[]
