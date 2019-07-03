## TODO:
## - make it possible to switch to/from ranking with buttons
## - add button icons on the screen (for start/enter, up/down, ranking/game)
## - add useR logo
## - remove tabs or make them nicer

#' @importFrom utils packageDescription packageVersion
oaTitlePanel <- function(appName, pkgName, logo, logo2 = NULL, version = TRUE) {
  
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
          
          div(
              if (!is.null(logo2) && file.exists(logo2))
                img(src = file.path("logo", basename(logo2)), class = "logo2"),
              appName,
              if (file.exists(logo))
                img(src = file.path("logo", basename(logo)), class = "logo"),
              class = "logo-div"
          )
      )
  )
}

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
                    case 13: // enter
                      Shiny.onInputChange("enter", Math.random(1));
                      break;
                  }
                }
              });
              '), 
      
      includeCSS(system.file("resources", "custom.css", package = "petanqueApp")),      
      
      # title panel
      oaTitlePanel(appName = "Petanque Shiny App", pkgName = "petanqueApp",
          logo = system.file("resources", "logo_text.png", package = "petanqueApp"),
          logo2 = system.file("resources", "useR2019.png", package = "petanqueApp"),
          version = FALSE
      ),
      
      # main contents
      tabsetPanel(id = "main-tabs",
          tabPanel("Game", 
              uiOutput("gameUI")
          ),
          tabPanel("Rankings",
              uiOutput("rankingsUI")
          )
      )
  
  )
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
      
      footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmPlayers", "Start!")
      ), 
      size = "m", 
      easyClose = FALSE)
}

