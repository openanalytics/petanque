## TODO: 
## - improve layout
#' @importFrom DT datatable formatStyle styleEqual
petanqueServer <- function(input, output, session) {
	
	## debugging
	observeEvent(input$debug_console, browser())
	
	## config and initialization 
	MAX_TURNS <- 6
	STEP_MAX <- 8  # animation steps
	DELAY <- 500  # animation step delay in ms 
	
	gameActive <- reactiveVal(FALSE)
	gameEnded <- reactiveVal(FALSE)
	gameData <- reactiveVal(NULL)  		## data frame with all game data
	gamePlot <- reactiveVal(NULL)  		## svg string with plot
	
	players <- reactiveVal(NULL)      ## vector of names
	rankings <- reactiveVal(NULL)
	activePlayer <- reactiveVal(NULL) ## index of active player (1 or 2)
	turnNumber <- reactiveVal(NULL)
	
	animationStep <- reactiveVal(0) ##STEP_MAX)			## animation step (initialized as 'finished')
	animationFinished <- reactiveVal(TRUE)
	
	distance <- reactiveVal(NULL)
	
	distrChoices <- reactiveVal(NULL)	## list of lists
	activeDistr <- reactiveVal(1)			
	chosenDistr <- reactiveVal(NULL)	
	
	refreshRankingsFile <- reactiveVal(0)
	
	## helper reactives
	nDistr <- reactive({
				req(distrChoices())
				length(distrChoices())
			})
	# we always know the current score
	
	score <- reactive({
				req(gameData())
				determineOutcome(gameData())$pointsWon
			})
	# and the winning player
	winner <- reactive({
				req(gameData())
				winnerNumber(determineOutcome(gameData())$winner)
			})
	
	observeEvent(input$helpRanking, {
				if (input[["main-tabs"]] == "Rankings")
					showModal(rankingDocumentation())
			})              
	
	## ui elements
	observeEvent(input$gotoRankings, 
			updateTabsetPanel(session, "main-tabs", selected = "Rankings"))
	observeEvent(input$linkRankings, 
			updateTabsetPanel(session, "main-tabs", selected = "Rankings"))
	observeEvent(input$linkGame, 
			updateTabsetPanel(session, "main-tabs", selected = "Game"))
	
	output$gameUI <- renderUI({
				tagList(
						uiOutput("message", class = "message"),        
						if (gameActive() || gameEnded()) 
							fluidRow(
									column(3, 
#                      h2("Distribution choices"),
											uiOutput("distributionMsg", class = "message"),
											uiOutput("distributionList")
									),
									column(9, 
											uiOutput("gameArea")
									)
							)
				)
				
			})
	
	output$rankingsUI <- renderUI({
				tagList(  
						h2("Ranking"),
						DT::dataTableOutput("rankingTable"),
						div(class = "helpRanking-container", 
								actionButton("helpRanking", "The ranking system", icon = icon('question'))
						)
				)  
			})
	
	## file is checked each time refreshRankingsFile() is changed
#  rankingsData <- reactiveFileReader(1000, session, RANKING_FILE, printRankings)
	rankingsData <- reactive({
				refreshRankingsFile() # to re-run this after ratings update
				printRankings()
			})
	
	output$rankingTable <- DT::renderDataTable({
				dt <- datatable(rankingsData(), rownames = FALSE, class = "hover",
						colnames = c("Rank", "Player", "Rating", "No. Games Played", "No. Wins", "No. Losses"),
						options = list(paging = FALSE))
				if (!is.null(players()))
					dt <- formatStyle(dt,
							columns = "player",  target = "row",
							fontWeight = styleEqual(players(), values = rep("bold", length(players())), default = "normal"),
							color = styleEqual(players(), values = rep("#e52323", length(players())), default = "black")
					)
				dt
			})
	
	output$message <- renderUI({
				
				if (gameEnded() && animationFinished()) {
					# update and show ranking
					updateRanking(players(), winner(), score())
					# trigger file re-reading
					refreshRankingsFile(runif(1))
#					msg <- tagList("Game finished! ", br(), 
#							span(class = paste0("player", winner()), players()[[winner()]]), 
#							" has won with ", score(), paste0(" point", if (score() > 1) "s." else "."))
					msg <- tagList("Game finished! ", br(), 
							"Press", actionLink("gotoRankings", "Right"), "to see rankings.", br(), 
							"Press", actionLink("enter", "Start"), "to start a new game!")
#          playSound("win")
				} else {
					if (!gameActive() && animationFinished()) {
						msg <- tagList("Press", actionLink("enter", "Start"), "to start a new game!") 
					} else {
#						msg <- "Game is in progress!"
						msg <- NULL
					}
					if (!is.null(players())) 
						msg <- tagList(msg, br(), 
								"Playing:", 
								span(sprintf("%s (%s)", players()[[1]], rankings()[[1]]), class = "player1"), "and", 
								span(sprintf("%s (%s)", players()[[2]], rankings()[[2]]), class = "player2"))
#					if (!is.null(turnNumber()) && animationFinished()) 
#						msg <- tagList(msg, br(), "Turn Number:", turnNumber())
#					if (!is.null(activePlayer()) && animationFinished()) 
#						msg <- tagList(msg, "Current player:", 
#								span(class = paste0("player", activePlayer()), players()[[activePlayer()]]))
				}
				
				h2(msg)
				
			})
	
	output$distributionMsg <- renderUI({
				
#				req(nDistr(), animationFinished(), gameActive())
				
				msg <- NULL
				
				if (gameEnded() && animationFinished()) {
					
					msg <- tagList("The winner is", 
							br(), span(class = paste0("player", winner()), players()[[winner()]]), 
							br(), br(), icon("trophy"), 
							br(), br(), paste0("(", score(), " point", if (score() > 1) "s)" else ")"))
					
				} else { 
					
					if (!is.null(turnNumber()) && animationFinished()) 
						msg <- tagList(msg, br(), paste0("Turn number ", turnNumber(), "/6"), br())
					if (!is.null(activePlayer()) && animationFinished()) 
						msg <- tagList(msg, 
								span(class = paste0("player", activePlayer()), players()[[activePlayer()]]),
								br(), "Choose your distribution")
					
				}
				
				h2(msg)
				
			})
	
	
	output$distributionList <- renderUI({
				req(nDistr(), animationFinished(), gameActive())
				
				div(class = "distr-list", 
						lapply(seq_len(nDistr()), function(iDistr) {
									div(id = paste0("distr", iDistr), 
											class = paste("distr", if (!is.null(activeDistr()) && iDistr == activeDistr()) "selected"),
											printDistr(distrChoices()[[iDistr]], inButton = TRUE)
									)
								})
				)
				
			})
	
	output$gameArea <- renderUI({
				msg <- if (!is.null(activePlayer()) && !is.null(chosenDistr())) {
					paste(players()[[3-activePlayer()]], "chose", printDistr(chosenDistr()))
				}
				
				tagList(
						div(style = "color:gray", h3(msg)),
						uiOutput("game")
				)
			})
	
	### plot drawing:
	## our reactive object is `svglite::svgstring` function
	## it gets changed after start or after a distribution is chosen (turn)
	output$game <- renderUI({ HTML(gamePlot()) })
	
	## after distribution is chosen, sample from it and start animation
	observeEvent(chosenDistr(), {
				picked <- chosenDistr()
				# sample distance from distribution
				distance(
						distanceFromDistribution(
								distribution = picked[["type"]],
								param1 = picked[["param1"]],
								param2 = picked[["param2"]]
						)
				)
				# start animation
				animationFinished(FALSE)
				animationStep(1)
				collisNo(0)
				intermediateData(NULL)
			})
	
	
	intermediateData <- reactiveVal(NULL)
	collisNo <- reactiveVal(0)
	
	## animation
	observe({
				invalidateLater(DELAY, session)
				isolate({
							req(!animationFinished(), distance())  # animationStep() < STEP_MAX
							curStep <- animationStep()
							
							if (curStep <= 6) {
								posDFarg <- gameData() # original data
							} else { 
								posDFarg <- intermediateData()
							}
							
							# update (redraw) plot
							svgStr <- svgDevice()
							# FIXME: not ideal passing distribution and params again?..
							posDF <- throwBall(distance = distance(), distribution = chosenDistr()[["type"]],
									param1 = chosenDistr()[["param1"]], param2 = chosenDistr()[["param2"]],
									posDF = posDFarg, step = curStep) #, collisNo = collisNo())
							
							finished <- FALSE
							
							if (curStep == 6) {
								posDF <- detectCollision(posDF) #, collisNo = collisNo)
								if (any(posDF$travelDist != 0)) { # continue
#                  playSound("hit")
									intermediateData(posDF)  # save returned data with travelDist
									collisNo(collisNo() + 1) # inc collision
									# call animate via step...
								} else {
									finished <- TRUE
								}
							}
							
							if (curStep > 6) {
								if ((curStep-6) %% 5 == 0) {
									# finished collision animation
									if (collisNo() < 4) { # limit number of collisions
										posDF <- detectCollision(posDF) #, collisNo = collisNo)
										if (any(posDF$travelDist != 0)) { # continue
#                      playSound("hit")
#                      intermediateData(posDF)
											collisNo(collisNo() + 1) # inc collision
											# call animate via step...
										} else {
											finished <- TRUE
										}
									} else {
										finished <- TRUE
									}
								} else { # we are in progress with animation
#                  intermediateData(posDF)
								}
							}
							
							if (finished) {
								# TODO
#                refreshPlot(posDF, newPlot = FALSE)
								if (turnNumber() <= MAX_TURNS && !gameEnded()) {
									drawHuman(color = posDF$color[turnNumber()+1]) #; Sys.sleep(1)
								}
#                playSound("ball")
								animationFinished(TRUE)
								gameData(posDF)
							} else {
								animationStep(curStep + 1)
							}
							
							dev.off()
							gamePlot(svgStr())
							
						})
			})
	
	## keyboard inputs ('enter' can be also clicked) 
	observeEvent(input$enter, {
				
				if (!gameActive()) {
					## just started the game
					# ask for player names
					showModal(playerInfoModal())
					# further logic is called only after confirmation of names 
				} else {  
					## have chosen a distribution   
					picked <- distrChoices()[[activeDistr()]] 
					chosenDistr(picked)
					# change the player
					curPlayer <- activePlayer()
					activePlayer(3-curPlayer)
					# increase turn
					nextTurn <- turnNumber() + 1
					if (nextTurn > MAX_TURNS) {
						# end game
						gameEnded(TRUE)
						gameActive(FALSE)
					} else {
						# resample distributions # TODO check that this is working with posDF input
						distrChoices(generateOptions(gameData()))
						# reset distr selection to the 1st
						activeDistr(1)
						# set next turn
						turnNumber(nextTurn)
					}
				}
			})
	
	observeEvent(input$up, {
				if (!is.null(distrChoices())) {
					curDistr <- activeDistr()
					newDistr <- if (curDistr == 1) nDistr() else curDistr-1
					activeDistr(newDistr)
				}
			})
	
	observeEvent(input$down, {
				if (!is.null(distrChoices())) {
					curDistr <- activeDistr()
					newDistr <- if (curDistr == nDistr()) 1 else curDistr+1
					activeDistr(newDistr)
				}
			})
	
	# switch between game and rankings
	observeEvent(input$right, {
				curTab <- input[["main-tabs"]]
				if (curTab == "Game") {
					updateTabsetPanel(session, "main-tabs", selected = "Rankings")
				} else if (curTab == "Rankings") {
					updateTabsetPanel(session, "main-tabs", selected = "Game")
				}
			})
	observeEvent(input$left, {
				curTab <- input[["main-tabs"]]
				if (curTab == "Game") {
					updateTabsetPanel(session, "main-tabs", selected = "Rankings")
				} else if (curTab == "Rankings") {
					updateTabsetPanel(session, "main-tabs", selected = "Game")
				}
			})
	
	observeEvent(input$confirmPlayers, {
				if (nchar(input$player1) == 0) {
					showModal(playerInfoModal(failed1 = TRUE))
				} else if (nchar(input$player2) == 0) {
					showModal(playerInfoModal(failed2 = TRUE))
				} else if (input$player1 == input$player2) {
					showModal(playerInfoModal(failedBoth = TRUE))
				} else {
					removeModal()
					updateTabsetPanel(session, "main-tabs", selected = "Game")
					playerNames <- c(input$player1, input$player2)
					# save names in the DB
					oldPlayers <- getPlayers()
					if (!all(playerNames %in% oldPlayers))
						addPlayers(playerNames)
					
					# start game
					startNewGame(playerNames)
					
				}
			})
	
	## this function is inside server as we modify many reactives
	startNewGame <- function(playerNames) {
		# start game
		gameActive(TRUE)
		gameEnded(FALSE)
		gameData(NULL)
		players(NULL)
		activePlayer(NULL)
		turnNumber(NULL)
		chosenDistr(NULL)
		
		# draw initial game area
		svgStr <- svgDevice()
		posDF <- newGame()
		dev.off()
		gamePlot(svgStr())
		gameData(posDF)
		
		# use names in the game
		players(playerNames)
		# get their rankings
		refreshRankingsFile(runif(1))
		rankings(c(getRanking(playerNames[1])$rating, getRanking(playerNames[2])$rating))
		# Set active player
		activePlayer(1)
		# Set turn
		turnNumber(1)
		# Sample distribution choices
		distrChoices(generateOptions(posDF))
	}
	
}

#' @importFrom svglite svgstring
svgDevice <- function() {
	svgstring(standalone = FALSE, height = 400/72, width = 900/72)
}