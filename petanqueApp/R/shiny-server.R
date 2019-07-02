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
            span(if (!is.null(turnNumber())) paste("Turn Number:", turnNumber())),
            span(if (!is.null(activePlayer())) paste("Current player:", activePlayer()))
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
              span(paste0("Game finished with Player ", winner(), " winning with ", score(), " points."),
                  actionLink("gotoRankings", "See Rankings."), 
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
          # just started the game
          gameEnded(FALSE)  
          gameActive(TRUE)
          # sample the target
          targetLoc(runif(1, 3, 9))
          # Set active player
          activePlayer(1)
          # Set turn
          turnNumber(1)
          # Sample distribution
          distrChoices(sampleDistribution())
        } else {
          # have chosen a distribution   
          # TODO: make a throw
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
  
}

sampleDistribution <- function() {
  # TODO: distr selection
  sample(c("normal(4, 3)", "normal(5, 2)", "poisson(6)", "poisson(8)", "gamma(6, 2)", "gamma(5, 1)"), size = 3, replace = FALSE)
}
