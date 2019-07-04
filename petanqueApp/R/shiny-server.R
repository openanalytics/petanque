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
  
  step <- reactiveVal(STEP_MAX)			## animation step (initialized as 'finished')

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
  
  ## ui elements
  observeEvent(input$gotoRankings, 
      updateTabsetPanel(session, "main-tabs", selected = "Rankings"))
  
  output$gameUI <- renderUI({
        tagList(
            uiOutput("message", class = "message"),        
            if (gameActive() || gameEnded()) 
              fluidRow(
                  column(3, 
                      h2("Distribution choices"),
                      uiOutput("distributuionList")
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
            DT::dataTableOutput("rankingTable")
        )
      })

  # FIXME: file is checked every second, is there a better solution?
#  rankingsData <- reactiveFileReader(1000, session, RANKING_FILE, printRankings)
  rankingsData <- reactive({
        refreshRankingsFile() # to re-run this after ratings update
        printRankings()
      })

  output$rankingTable <- DT::renderDataTable({
        dt <- datatable(rankingsData(), rownames = FALSE, class = "hover",
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
        
        if (gameEnded() && step() == STEP_MAX) {
          msg <- tagList("Game finished! ", br(), 
              span(class = paste0("player", winner()), players()[[winner()]]), 
              " has won with ", score(), paste0(" point", if (score() > 1) "s." else "."))
          msg <- tagList(msg, actionLink("gotoRankings", "See Rankings."), br(), 
              "Press", actionLink("enter", "Start"), "to start a new game!") 
        } else {
          if (!gameActive() && step() == STEP_MAX) {
            msg <- tagList("Press", actionLink("enter", "Start"), "to start a new game!") 
          } else {
            msg <- "Game is in progress!"
          }
          if (!is.null(players())) 
            msg <- tagList(msg, br(), 
                "Playing:", 
                span(sprintf("%s (%s)", players()[[1]], rankings()[[1]]), class = "player1"), "and", 
                span(sprintf("%s (%s)", players()[[2]], rankings()[[2]]), class = "player2"))
          if (!is.null(turnNumber()) && step() == STEP_MAX) 
            msg <- tagList(msg, br(), "Turn Number:", turnNumber())
          if (!is.null(activePlayer()) && step() == STEP_MAX) 
            msg <- tagList(msg, "Current player:", 
                span(class = paste0("player", activePlayer()), players()[[activePlayer()]]))
        }
    
        h2(msg)
        
      })
  
  output$distributuionList <- renderUI({
        req(nDistr(), step() == STEP_MAX, gameActive())
        
        div(class = "distr-list", 
            lapply(seq_len(nDistr()), function(iDistr) {
                  div(id = paste0("distr", iDistr), 
                      class = paste("distr", if (!is.null(activeDistr()) && iDistr == activeDistr()) "selected"),
                      printDistr(distrChoices()[[iDistr]])
                  )
                })
        )
        
      })
  
  output$gameArea <- renderUI({
        msg <- if (!is.null(activePlayer()) && !is.null(chosenDistr())) {
              paste(players()[[3-activePlayer()]], "chose", printDistr(chosenDistr()))
            } else "Choose your distribution"
        
        tagList(
            h3(msg),
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
        step(1)
      })

  ## animation
  observe({
        invalidateLater(DELAY, session)
        isolate({
              req(step() < STEP_MAX, distance())
              curStep <- step()
              
              # update (redraw) plot
              svgStr <- svgDevice()
              # FIXME: not ideal passing distribution and params again?..
              posDF <- throwBall(distance = distance(), distribution = chosenDistr()[["type"]],
                  param1 = chosenDistr()[["param1"]], param2 = chosenDistr()[["param2"]],
                  posDF = gameData(), step = curStep)
              dev.off()
              gamePlot(svgStr())

              # update data frame
              if (curStep == STEP_MAX-1)
                gameData(posDF)
              
              step(curStep + 1)
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
            # update and show ranking
            updateRanking(players(), winner(), score())
            # trigger file re-reading
            refreshRankingsFile(runif(1))
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
    rankings(c(getRanking(playerNames[1])$rating, getRanking(playerNames[2])$rating))
    # Set active player
    activePlayer(1)
    # Set turn
    turnNumber(1)
    # Sample distribution choices
    distrChoices(generateOptions(posDF))
  }
  
}

svgDevice <- function() {
  svglite::svgstring(standalone = FALSE, height = 400/72, width = 900/72)
}