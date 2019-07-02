petanqueServer <- function(input, output, session) {
  
  # debugging
  observeEvent(input$debug_console, browser())
  
  output$gameUI <- renderUI({
        h1("Game on!")
      })
  
  output$rankingsUI <- renderUI({
        h1("Rankings")
      })
  
}
