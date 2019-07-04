generateNegativeMessage <- function() {
	
	messages <- c("Your ball took a surprising turn.", 
			"You subverted expectations.",
			"Try to be more positive.", 
			"Try throwing the other way next time.", 
			"That was miraculously awful.", 
			"Whoopsie!", 
			"Oopsidoodle!",
			"Merde!")
	
	return(sample(messages, 1))
	
}

generatePositiveMessage <- function(distance) {
	if(distance < 15)
		messages <- c("Your ball hit a tree!", 
				"Your ball hit a bus!", 
				"Your ball (nearly) hit a cat! Please sample more safely.", 
				"This is petanque, not shot put.",
				"Check out these muscles!",
				"You're an outlier!", 
				"Merde!", "Non!")
	if(distance > 15)
		messages <- c("Your ball entered orbit!", 
				"Your ball is travelling to UseR 2020!", 
				"Your ball started an international conflict",
				"Your ball scored on another court!",
				"Your ball landed on the moon.",
				"Your ball went to a closed-source conference.",
				"Your ball was abducted.",
						":(")
	
			return(sample(messages, 1))
}