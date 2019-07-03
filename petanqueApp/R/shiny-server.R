## TODO: 
## - improve layout

petanqueServer <- function(input, output, session) {
  
  # debugging
  observeEvent(input$debug_console, browser())
  
  observeEvent(input$gotoRankings, 
      updateTabsetPanel(session, "main-tabs", selected = "Rankings"))
  
  output$gameUI <- renderUI({
        tagList(
            uiOutput("message"),        
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
        h1("Rankings")
        DT::dataTableOutput("rankingTable")
      })
  
  output$rankingTable <- DT::renderDataTable({
        # TODO
        ranking <- getRanking()
        DT::datatable(ranking, rownames = TRUE)
      })
  
  output$message <- renderUI({
        
        if (gameEnded() && step() == STEP_MAX) {
          msg <- tagList("Game finished! ", 
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
                "Playing:", span(players()[[1]], class = "player1"), "and", span(players()[[2]], class = "player2"))
          if (!is.null(turnNumber())) 
            msg <- tagList(msg, br(), "Turn Number:", turnNumber())
          if (!is.null(activePlayer())) 
            msg <- tagList(msg, "Current player:", 
                span(class = paste0("player", activePlayer()), players()[[activePlayer()]]))
        }
    
        h2(msg)
        
      })
  
  output$distributuionList <- renderUI({
        req(nDistr())
        
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
  ## our reactive object is `svglite::svgstring` device
  ## it gets changed after start or after a distribution is chosen (turn)
  
  ## update game field on each turn
  observeEvent(chosenDistr(), {
        picked <- chosenDistr()
        # sample distance
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

  # animation
  observe({
        invalidateLater(500, session)
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
              if (curStep == 7)
                gameData(posDF)
              
              step(curStep + 1)
            })
      })
  
  output$game <- renderUI({ HTML(gamePlot()) })
  
  gameActive <- reactiveVal(FALSE)
  gameEnded <- reactiveVal(FALSE)
  gameData <- reactiveVal(NULL)  ## data frame with all game data
  gamePlot <- reactiveVal(NULL)
  
  players <- reactiveVal(NULL)  ## vector of names
  activePlayer <- reactiveVal(NULL)  ## index of active player (1 or 2)
  turnNumber <- reactiveVal(NULL)
  MAX_TURNS <- 6
  STEP_MAX <- 8  # animation steps
  
  step <- reactiveVal(STEP_MAX)
  distance <- reactiveVal(NULL)
    
  distrChoices <- reactiveVal(NULL)
  activeDistr <- reactiveVal(1)
  chosenDistr <- reactiveVal(NULL)
  
  nDistr <- reactive({
        req(distrChoices())
        length(distrChoices())
      })
  
  score <- reactive({
        req(gameData())
        determineOutcome(gameData())$pointsWon
      })
  winner <- reactive({
        req(gameData())
        winnerNumber(determineOutcome(gameData())$winner)
      })
  
# inputs
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
            # TODO: update and show ranking
            updateRanking(players(), winner(), score())
            # end game
            gameEnded(TRUE)
            gameActive(FALSE)
          } else {
            # resample distributions
            distrChoices(generateOptions())
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
  
  startNewGame <- function(playerNames) {
    # start game
    players(NULL)
    activePlayer(NULL)
    turnNumber(NULL)
    chosenDistr(NULL)
    gameEnded(FALSE)
    gameActive(TRUE)
    gameData(NULL)
    
    # draw initial game area
    svgStr <- svgDevice()
    posDF <- newGame()
    dev.off()
    gamePlot(svgStr())
    gameData(posDF)
    
    # use names for the game
    players(playerNames)
    # Set active player
    activePlayer(1)
    # Set turn
    turnNumber(1)
    # Sample distribution
    distrChoices(generateOptions())
  }
  
}

svgDevice <- function() {
  svglite::svgstring(standalone = FALSE, height = 400/72, width = 800/72)
}