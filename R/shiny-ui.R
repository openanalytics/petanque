## TODO:
## - make it possible to switch to/from ranking with buttons
## - add button icons on the screen (for start/enter, up/down, ranking/game)
## - add useR logo
## - remove tabs or make them nicer


#' Create title panel.
#' @param appName String with application name.
#' @param pkgName String with package name.
#' @param logo String, path to first logo.
#' @param logo2 String, path to second logo.
#' @param subtitle Subtitle string
#' @param version Logical, if TRUE (by default) the application
#' version is included in the app.
#' @return \code{\link[shiny]{titlePanel}}.
#' @import shiny
#' @importFrom utils packageVersion packageDescription
oaTitlePanel <- function(appName, pkgName, logo, logo2 = NULL, subtitle = NULL, version = TRUE) {
  
  addResourcePath("logo", dirname(normalizePath(logo)))
  
  titlePanel(
      windowTitle = appName, 
      title = list(
          if (version)
            span(class = "version", 
                HTML("version:", as.character(packageVersion(pkgName)),
                    "<br> updated:", 
                    format(as.Date(packageDescription(pkgName, fields = "Packaged"))) %OR% 
                        format(as.Date(packageDescription(pkgName, fields = "Date"))) %OR% "N/A"
                )
            ),
          
          div(class = "logo-div",
              if (!is.null(logo2) && file.exists(logo2))
                img(src = file.path("logo", basename(logo2)), class = "logo2"),
              if (file.exists(logo))
                img(src = file.path("logo", basename(logo)), class = "logo"),
              h1(appName),
              if (!is.null(subtitle))
                h3(subtitle)
  
          )
      )
  )
}


#' Shiny UI script.
#' @param debug Logical, should the debug functionality
#' be included in the app (FALSE by default).
#' @return \code{\link[shiny]{fluidPage}}.
#' @import shiny
petanqueUI <- function(debug = FALSE) {
  fluidPage(
      
      # debugging
      if (isTRUE(debug))
        actionLink(inputId = "debug_console", label = "Connect with console"),
      
      # keyboard inputs
      # here we prevent key presses when modal is shown
      # TODO: add keys to go to ranking and back to the game (+ show controls on the screen) 
      tags$script('
              $(document).on("keydown", function (e) {
   						  if (!document.getElementById("shiny-modal")) {
                  switch (e.which) {
                    case 40: // down
                      Shiny.onInputChange("down", Math.random(1));
                      break;
                    case 38: // up
                      Shiny.onInputChange("up", Math.random(1));
                      break;
                    case 37: // left
                      Shiny.onInputChange("left", Math.random(1));
                      break;
                    case 39: // right
                      Shiny.onInputChange("right", Math.random(1));
                      break;
										case 72: // h
                      Shiny.onInputChange("helpRanking", Math.random(1));
                      break;
                    case 13: // enter
                      Shiny.onInputChange("enter", Math.random(1));
                      break;
                    case 32: // space
                      Shiny.onInputChange("enter", Math.random(1));
                      break;

                  }
                }
              });
              '), 
      
      includeCSS(system.file("resources", "custom.css", package = "petanque")),      
      
      # title panel
      oaTitlePanel(appName = "\u2119\u00e9tanque \u2119aram\u00e9trique", 
          subtitle = "a shiny app", 
          pkgName = "petanque",
          logo = system.file("resources", "logo_text.png", package = "petanque"),
          logo2 = system.file("resources", "useR2019.png", package = "petanque"),
          version = FALSE
      ),
      
      # main contents
      tabsetPanel(id = "main-tabs",
          tabPanel("Game", 
              uiOutput("gameUI")
          ),
          tabPanel("Rankings",
              uiOutput("rankingsUI", class = "rankings")
          )
      ),
      
      fluidRow(
          column(12, 
              div(class = "controls",  
                  strong("Controls:"), "Use up/down arrows to choose distribution, 'Enter' or 'Space' to start/select, left/right arrows to switch between", actionLink("linkGame", "Game"), "and", actionLink("linkRankings", "Rankings"), ", 'h' for ranking system help")
          )
      )
  
  )
}

#' Print information on players in a modal dialog.
#' @param failed1 Logical, if TRUE (FALSE by default), player 1 has failed the game.
#' @param failed2 Logical, if TRUE (FALSE by default), player 2 has failed the game.
#' @param failedBoth Logical, if TRUE (FALSE by default), both players have failed the game.
#' @return \code{\link[shiny]{modalDialog}}
#' @import shiny
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
      
      footer = tagList(
#          modalButton("Cancel"),
          actionButton("confirmPlayers", "Start!")
      ), 
      size = "m", 
      easyClose = FALSE)
}

#' Create documentation about the ranking in the application.
#' @return \code{\link[shiny]{modalDialog}}
#' @import shiny
rankingDocumentation <- function() {
  modalDialog(title = "The rating system",
      withMathJax(
          includeHTML(system.file("resources", "help.html", package = "petanque"))
      ),
      easyClose = TRUE,
      size = 'l'
  )
}



