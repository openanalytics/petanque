#' @importFrom utils packageDescription packageVersion
oaTitlePanel <- function(appName, pkgName, logo, version = TRUE) {
  
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
          
          div(if (file.exists(logo)) 
                img(src = file.path("logo", basename(logo)), class = "logo"), 
              appName,
              class = "logo-div")
      )
  )
}

petanqueUI <- function(debug = FALSE) {
  function(request) {
    fluidPage(
        
        # debugging
        if (isTRUE(debug))
          actionLink(inputId = "debug_console", label = "Connect with console"),
        
        includeCSS(system.file("resources", "custom.css", package = "petanqueApp")),      
        
        # title panel
        oaTitlePanel(appName = "Petanque Shiny App", pkgName = "petanqueApp",
            logo = system.file("resources", "logo.png", package = "petanqueApp"),
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
}
