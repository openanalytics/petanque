petanqueServer <- function(input, output, session) {
  
  # debugging
  observeEvent(input$debug_console, browser())
  
  observeEvent(input$gotoRankings, 
      updateTabsetPanel(session, "main-tabs", selected = "Rankings"))
  
  output$gameUI <- renderUI({
        h1("Game on!")
        
        
        fluidRow(
            uiOutput("message"),
            h2("Distributions"),
            column(4, uiOutput("distributuionList")),
            column(8, uiOutput("gameArea"))
        )
        
      })
  
  output$rankingsUI <- renderUI({
        h1("Rankings")
      })
  
  output$message <- renderUI({
        h2(
            span(if (gameEnded()) "Game finished!"),
            span(if (!gameActive()) "Press \"Start\" to start the game!" else "Game is in progress!"),
            span(if (!is.null(players())) "Playing:", paste(players(), collapse = " and ")),
            span(if (!is.null(turnNumber())) paste("Turn Number:", turnNumber())),
            span(if (!is.null(activePlayer())) paste("Current player:", players()[[activePlayer()]]))
        )
      })
  
  output$distributuionList <- renderUI({
        req(nDistr())
        
        div(class = "distr-list", 
            lapply(seq_len(nDistr()), function(iDistr) {
                  div(id = paste0("distr", iDistr), class = paste("distr", if (!is.null(activeDistr()) && iDistr == activeDistr()) "selected"),
                      distrChoices()[[iDistr]]
                  )
                })
        )
        
      })
  
  output$gameArea <- renderUI({
        req(chosenDistr())
        
        tagList(
            if (gameEnded()) {
              span(
                  paste0("Game finished with Player ", winner(), "(", players()[[winner()]], ") winning with ", score(), " points."),
                  actionLink("gotoRankings", "See Rankings.") 
              )
            } else {
              paste("Player", 3-activePlayer(), "chose", chosenDistr())
            },
            plotOutput("pdf")
        )
      })
  
  output$pdf <- renderPlot({
        req(chosenDistr())
        n <- 1000
        dat <- switch(chosenDistr(),
            "normal(4, 3)" = rnorm(n, 4, 3), 
            "normal(5, 2)" = rnorm(n, 5, 2),
            "poisson(6)" = rpois(n, 6),
            "poisson(8)" = rpois(n, 8),
            "gamma(6, 2)" = rgamma(n, 6, 2),
            "gamma(5, 1)" = rgamma(n, 5, 1)
        )
        hist(dat, xlim = c(0, 10), freq = FALSE)
        # target
        points(x = targetLoc(), y = 0, pch = 20, col = "red")
      })
  
  gameActive <- reactiveVal(FALSE)
  gameEnded <- reactiveVal(FALSE)
  players <- reactiveVal(NULL)
  activePlayer <- reactiveVal(NULL)
  turnNumber <- reactiveVal(NULL)
  MAX_TURNS <- 6
  winner <- reactiveVal(NULL)
  score <- reactiveVal(NULL)
  
  targetLoc <- reactiveVal(NULL)
  
  distrChoices <- reactiveVal(NULL)
  activeDistr <- reactiveVal(1)
  chosenDistr <- reactiveVal(NULL)
  
  nDistr <- reactive({
        req(distrChoices())
        length(distrChoices())
      })
  
  # inputs
  observeEvent(input$enter, {
        if (!gameActive()) {
          ## just started the game
          players(NULL)
          gameEnded(FALSE)  
          gameActive(TRUE)
          # TODO: ask for player names
          showModal(playerInfoModal())
          # sample the target
          targetLoc(sampleTarget())
          # Set active player
          activePlayer(1)
          # Set turn
          turnNumber(1)
          # Sample distribution
          distrChoices(sampleDistribution())
        } else {
          ## have chosen a distribution   
          # TODO: show a throw
          chosenDistr(distrChoices()[[activeDistr()]])
          
          ## Switch player:
          # change the player
          curPlayer <- activePlayer()
          activePlayer(3-curPlayer)
          # resample distributions
          distrChoices(sampleDistribution())
          # reset distr selection to the 1st
          activeDistr(1)
          # increase turn
          nextTurn <- turnNumber() + 1
          if (nextTurn > MAX_TURNS) {
            # end game
            gameEnded(TRUE)
            activePlayer(NULL)
            turnNumber(NULL)
            # TODO: determine the winner and show score
            score(sample(1:3, 1))
            winner(sample(1:2, 1))
            # TODO: update and show ranking
            #updateRanking()
            # TODO: prepare for new game
            gameActive(FALSE)
          } else {
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
          # use names for the game
          players(playerNames)
        }
      })
  
}

playerInfoModal <- function(failed1 = FALSE, failed2 = FALSE, failedBoth = FALSE) {
  allPlayers <- getPlayers()
  
  selectizeOpts <- list(placeholder = "please choose or enter new", create = TRUE,
      onInitialize = I('function() { this.setValue(""); }'))
  
  modalDialog(title = "Players", 

      if (failedBoth)
        span("Please choose two different names", class = "warn"),
      
      if (failed1)
        span("Invalid player 1 name", class = "warn"),
      
      selectizeInput("player1", "Player 1:", choices = allPlayers, 
          multiple = FALSE, options = selectizeOpts),

      if (failed2)
        span("Invalid player 2 name", class = "warn"),
      
      selectizeInput("player2", "Player 2:", choices = allPlayers, 
          multiple = FALSE, options = selectizeOpts),

      footer = actionButton("confirmPlayers", "Start!"), 
      size = "m", 
      easyClose = FALSE)
}

PLAYER_FILE <- "/tmp/petanque-players.rds"

getPlayers <- function(file = PLAYER_FILE) {
  if (file.exists(file))
    out <- readRDS(file)
  else 
    out <- c()
  out
}

addPlayers <- function(players, file = PLAYER_FILE) {
  if (file.exists(file))
    out <- readRDS(file)
  else 
    out <- c()
  
  out <- unique(c(out, players))
  saveRDS(out, file)  
}

sampleDistribution <- function() {
  # TODO: distribution selection
  sample(c("normal(4, 3)", "normal(5, 2)", "poisson(6)", "poisson(8)", "gamma(6, 2)", "gamma(5, 1)"), size = 3, replace = FALSE)
}

sampleTarget <- function() {
  # TODO
  runif(1, 3, 9)
}